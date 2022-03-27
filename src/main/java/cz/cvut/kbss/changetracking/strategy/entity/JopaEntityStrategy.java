package cz.cvut.kbss.changetracking.strategy.entity;

import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.changetracking.exception.AccessDeniedException;
import cz.cvut.kbss.changetracking.exception.ClassNotAuditedException;
import cz.cvut.kbss.changetracking.exception.IdNotMatchingException;
import cz.cvut.kbss.changetracking.exception.ObjectsNotCompatibleException;
import cz.cvut.kbss.changetracking.model.ChangeVector;
import cz.cvut.kbss.changetracking.util.ClassUtil;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class JopaEntityStrategy implements EntityStrategy<FieldSpecification<?, ?>> {
	private final Metamodel metamodel;

	public JopaEntityStrategy(Metamodel metamodel) {
		this.metamodel = metamodel;
	}

	@Override
	public final <TEntity> Collection<ChangeVector> getChangeVectors(TEntity older, TEntity newer, boolean requireSameId) {
		var type1 = getObjectType(older);
		var type2 = getObjectType(newer);
		var id1 = getObjectId(older);
		var id2 = getObjectId(newer);

		if (requireSameId && !Objects.equals(id1, id2))
			throw new IdNotMatchingException(id1, id2);

		String typeName;
		Collection<FieldSpecification<?, ?>> fieldSpecs;

		if (type1.equals(type2)) {
			var clazz = older.getClass();
			checkClassSupported(clazz);

			typeName = type1;
			fieldSpecs = getAttributes(older);
		} else {
			// get common ancestor
			var clazz = ClassUtil
				.getCommonSuperclass(older.getClass(), newer.getClass())
				.orElseThrow(() -> new ObjectsNotCompatibleException(older, newer));

			checkClassSupported(clazz);
			typeName = getTypeName(clazz);

			fieldSpecs = getAttributes(clazz);
		}


		return fieldSpecs
			.stream()
			.flatMap(attr -> {
				var val1 = getAttributeValue(attr, older);
				var val2 = getAttributeValue(attr, newer);
				if (!Objects.equals(val1, val2)) {
					if (attr instanceof Attribute | attr instanceof TypesSpecification) {
						return Stream.of(new ChangeVector(typeName, id1, getAttributeName(attr), val1));
					} else if (attr instanceof PropertiesSpecification) {
						return createVectorsForUnmappedProperties(typeName, id1, (Map<?, ?>) val1, (Map<?, ?>) val2);
					} else {
						// can still be a QueryAttribute or Identifier - either way, we're not dealing with it here
						return null;
					}
				} else return null;
			})
			.filter(Objects::nonNull)
			.collect(Collectors.toList());
	}

	@Override
	public String getTypeName(Class<?> clazz) {
		return clazz.getAnnotation(OWLClass.class).iri();
	}

	@Override
	public void checkClassSupported(@NotNull Class<?> clazz) {
		if (!clazz.isAnnotationPresent(Audited.class) || !clazz.isAnnotationPresent(OWLClass.class))
			throw new ClassNotAuditedException(clazz);
	}

	@Override
	@SuppressWarnings("unchecked")
	public Collection<FieldSpecification<?, ?>> getAttributes(Class<?> clazz) {
		return (Collection<FieldSpecification<?, ?>>) metamodel.entity(clazz).getFieldSpecifications();
	}

	protected Stream<ChangeVector> createVectorsForUnmappedProperties(
		String objectType, String objectId,
		Map<?, ?> older,
		Map<?, ?> newer
	) {
		var olderEmpty = older == null || older.isEmpty();
		var newerEmpty = newer == null || newer.isEmpty();

		// early exits
		if (olderEmpty) {
			if (newerEmpty) {
				return Stream.empty();
			} else {
				return newer
					.keySet()
					.stream()
					.map(k -> new ChangeVector(objectType, objectId, k.toString(), null));
			}
		} else if (newerEmpty) {
			return older
				.entrySet()
				.stream()
				.map(entry -> new ChangeVector(objectType, objectId, entry.getKey().toString(), entry.getValue()));
		}

		var diffsFromOlder = older
			.entrySet()
			.stream()
			.filter(entry -> !Objects.equals(older.get(entry.getValue()), newer.get(entry.getKey())))
			.collect(Collectors.toMap(
				Map.Entry::getKey,
				entry -> new ChangeVector(objectType, objectId, entry.getKey().toString(), entry.getValue())
			));

		var diffsFromNewer = newer
			.keySet()
			.stream()
			.filter(k -> !diffsFromOlder.containsKey(k))
			.map(o -> new ChangeVector(objectType, objectId, o.toString(), null));

		return Stream.concat(diffsFromOlder.values().stream(), diffsFromNewer);
	}

	@Override
	public String getAttributeName(FieldSpecification<?, ?> field) {
		var jField = field.getJavaField();
		if (field instanceof Attribute && jField.isAnnotationPresent(OWLDataProperty.class)) {
			return ((Attribute<?, ?>) field).getIRI().toString();
		} else if (field instanceof TypesSpecification) {
			return RDF.TYPE;
		} else if (field instanceof PropertiesSpecification | field instanceof Identifier | field instanceof QueryAttribute) {
			return null;
		} else {
			// TODO: at least log
			return jField.getName();
		}
	}

	@Override
	public Object getAttributeValue(FieldSpecification<?, ?> field, Object instance) {
		Objects.requireNonNull(field);
		Objects.requireNonNull(instance);
		var jField = field.getJavaField();
		if (!jField.canAccess(instance) && !jField.trySetAccessible()) {
			throw new AccessDeniedException(instance, jField.getName());
		}

		try {
			return jField.get(instance);
		} catch (IllegalAccessException e) {
			throw new AccessDeniedException(e);
		}
	}

	@Override
	public String getObjectId(Object o) {
		try {
			return metamodel.entity(o.getClass()).getIdentifier().getJavaField().get(o).toString();
		} catch (IllegalAccessException e) {
			throw new AccessDeniedException(e);
		}
	}
}
