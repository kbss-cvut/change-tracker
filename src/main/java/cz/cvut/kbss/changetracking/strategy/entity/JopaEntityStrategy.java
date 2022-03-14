package cz.cvut.kbss.changetracking.strategy.entity;

import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.changetracking.exception.AccessDeniedException;
import cz.cvut.kbss.changetracking.exception.ClassNotAuditedException;
import cz.cvut.kbss.changetracking.exception.ObjectsNotCompatibleException;
import cz.cvut.kbss.changetracking.model.ChangeVector;
import cz.cvut.kbss.changetracking.util.ClassUtil;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.Objects;
import java.util.stream.Collectors;

public class JopaEntityStrategy implements EntityStrategy<FieldSpecification<?, ?>> {
	private final Metamodel metamodel;

	public JopaEntityStrategy(Metamodel metamodel) {
		this.metamodel = metamodel;
	}

	@Override
	public final <TEntity> Collection<ChangeVector> getChangeVectors(TEntity older, TEntity newer) {
		var type1 = getObjectType(older);
		var type2 = getObjectType(newer);
		var id1 = getObjectId(older);
		// TODO: id2?

		String typeName;
		Collection<FieldSpecification<?, ?>> attributes;

		if (type1.equals(type2)) {
			typeName = type1;
			attributes = getAttributes(older);
		} else {
			// get common ancestor
			var clazz = ClassUtil.getCommonSuperclass(older.getClass(), newer.getClass());
			if (clazz == null)
				throw new ObjectsNotCompatibleException(older, newer);

			checkClassSupported(clazz);
			// TODO: same logic as in getObjectType but with a class instead of an instance - prevent using ugly hacks like
			//  the following - so extracting would make some sense
			//typeName = getObjectType(clazz.getConstructors()[0].newInstance(null));
			typeName = clazz.getAnnotation(OWLClass.class).iri();
			// TODO: @MappedSuperclass

			// TODO: refactor getAttributes to work with classes - analogically to the above
			//noinspection unchecked
			attributes = (Collection<FieldSpecification<?, ?>>) metamodel.entity(clazz).getFieldSpecifications();
		}


		return attributes
			.stream()
			.map(attr -> {
				var val1 = getAttributeValue(attr, older);
				var val2 = getAttributeValue(attr, newer);
				if (!Objects.equals(val1, val2)) {
					final var attributeName = getAttributeName(attr);
					return new ChangeVector(typeName, id1, attributeName, val1);
				} else return null;
			})
			.filter(Objects::nonNull)
			.collect(Collectors.toList());
	}

	@Override
	public void checkClassSupported(@NotNull Class<?> clazz) {
		if (!clazz.isAnnotationPresent(Audited.class) || !clazz.isAnnotationPresent(OWLClass.class))
			throw new ClassNotAuditedException(clazz);
	}

	@Override
	@SuppressWarnings("unchecked")
	public Collection<FieldSpecification<?, ?>> getAttributes(Object entity) {
		return (Collection<FieldSpecification<?, ?>>) metamodel.entity(entity.getClass()).getFieldSpecifications();
	}

	@Override
	public String getAttributeName(FieldSpecification<?, ?> field) {
		var jField = field.getJavaField();
		if (jField.isAnnotationPresent(OWLDataProperty.class)) {
			return jField.getAnnotation(OWLDataProperty.class).iri();
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
	public String getObjectType(Object o) {
		return o.getClass().getAnnotation(OWLClass.class).iri();
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
