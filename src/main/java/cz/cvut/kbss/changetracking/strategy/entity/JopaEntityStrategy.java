package cz.cvut.kbss.changetracking.strategy.entity;

import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.changetracking.annotation.IgnoreChanges;
import cz.cvut.kbss.changetracking.exception.AccessDeniedException;
import cz.cvut.kbss.changetracking.exception.ClassNotAuditedException;
import cz.cvut.kbss.changetracking.exception.IdNotMatchingException;
import cz.cvut.kbss.changetracking.exception.ObjectsNotCompatibleException;
import cz.cvut.kbss.changetracking.model.ChangeVector;
import cz.cvut.kbss.changetracking.util.ClassUtil;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import org.jetbrains.annotations.NotNull;

import java.net.URI;
import java.net.URL;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class JopaEntityStrategy extends BaseEntityStrategy<FieldSpecification<?, ?>> {
	private final Metamodel metamodel;

	public JopaEntityStrategy(Metamodel metamodel) {
		this.metamodel = metamodel;
	}

	@Override
	public final <TEntity> Collection<ChangeVector<?>> getChangeVectors(
		TEntity older,
		TEntity newer,
		boolean requireSameId
	) {
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
				if (attr.isInferred() || shouldIgnoreChanges(attr))
					return null;

				var val1 = getAttributeValue(attr, older);
				var val2 = getAttributeValue(attr, newer);

				if (!Objects.equals(val1, val2)) {
					if (attr instanceof Attribute) {
						// if ObjectProperty (association), determine the associated entity's (entities') identifier(s)
						if (((Attribute<?, ?>) attr).isAssociation()) {
							var oldId = extractEntityIdentifier(attr, val1);
							return Stream.of(new ChangeVector<>(typeName, id1, getAttributeName(attr), oldId));
						} else {
							return Stream.of(new ChangeVector<>(typeName, id1, getAttributeName(attr), val1));
						}
					} else if (attr instanceof TypesSpecification) {
						return Stream.of(new ChangeVector<>(typeName, id1, getAttributeName(attr), val1));
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

	private boolean shouldIgnoreChanges(FieldSpecification<?, ?> specification) {
		return specification.getJavaField().isAnnotationPresent(IgnoreChanges.class);
	}

	// TODO: refactor to use this from JOPA API as it was just copied
	private static final Set<Class<?>> SUPPORTED_IDENTIFIER_TYPES = Set.of(URI.class, URL.class, String.class);

	/**
	 * Extract an identifier from an associated entity, or multiple identifiers if the association is x-to-many.
	 *
	 * @param otherEntity Might in fact be any identifier type supported by JOPA (that being
	 * {@link #SUPPORTED_IDENTIFIER_TYPES}), an actual instance of the target entity class, or a {@link Collection} of
	 * either of the former.
	 */
	private Object extractEntityIdentifier(FieldSpecification<?, ?> attr, Object otherEntity) {
		if (otherEntity == null) return null;

		if (otherEntity instanceof Collection) {
			// still might contain either of the other two variants
			return ((Collection<?>) otherEntity)
				.stream()
				.map(instance -> extractEntityIdentifier(attr, instance))
				.collect(Collectors.toList());
		} else if (SUPPORTED_IDENTIFIER_TYPES.contains(otherEntity.getClass())) {
			return otherEntity;
		} else {
			// entity instance
			return getAttributeValue(
				metamodel.entity(((Bindable<?>) attr).getBindableJavaType()).getIdentifier(),
				otherEntity
			);
		}
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

	protected Stream<ChangeVector<?>> createVectorsForUnmappedProperties(
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
					.map(k -> new ChangeVector<>(objectType, objectId, k.toString(), null));
			}
		} else if (newerEmpty) {
			return older
				.entrySet()
				.stream()
				.map(entry -> new ChangeVector<>(objectType, objectId, entry.getKey().toString(), entry.getValue()));
		}

		var diffsFromOlder = older
			.entrySet()
			.stream()
			.filter(entry -> !Objects.equals(older.get(entry.getValue()), newer.get(entry.getKey())))
			.collect(Collectors.toMap(
				Map.Entry::getKey,
				entry -> new ChangeVector<>(objectType, objectId, entry.getKey().toString(), entry.getValue())
			));

		var diffsFromNewer = newer
			.keySet()
			.stream()
			.filter(k -> !diffsFromOlder.containsKey(k))
			.map(o -> new ChangeVector<>(objectType, objectId, o.toString(), null));

		return Stream.concat(diffsFromOlder.values().stream(), diffsFromNewer);
	}

	@Override
	public String getAttributeName(FieldSpecification<?, ?> field) {
		var jField = field.getJavaField();
		if (field instanceof Attribute &&
			(jField.isAnnotationPresent(OWLDataProperty.class)
				|| jField.isAnnotationPresent(OWLObjectProperty.class)
				|| jField.isAnnotationPresent(OWLAnnotationProperty.class)
				// OPTIMIZE: why? isn't instanceof enough?
			)
		) {
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
