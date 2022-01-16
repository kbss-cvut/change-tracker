package cz.cvut.kbss.changetracking.strategy.entity;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.changetracking.exception.AccessDeniedException;
import cz.cvut.kbss.changetracking.exception.ChangeTrackingException;
import cz.cvut.kbss.changetracking.exception.ClassNotAuditedException;
import cz.cvut.kbss.changetracking.exception.JsonException;
import cz.cvut.kbss.changetracking.model.JsonChangeVector;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;

import java.util.*;
import java.util.stream.Collectors;

public class JopaEntityStrategy implements EntityStrategy<Attribute<?, ?>> {
	/**
	 * Set of classes natively supported by Jackson's {@link ObjectMapper}.
	 */
	protected static final Set<Class<?>> supportedAttributeClasses = new HashSet<>(List.of(
		String.class,
		Character.class,
		char.class,
		Double.class,
		double.class,
		Float.class,
		float.class,
		Integer.class,
		int.class,
		Boolean.class,
		boolean.class
	));
	private final ObjectMapper objectMapper = new ObjectMapper();
	private final Metamodel metamodel;

	public JopaEntityStrategy(Metamodel metamodel) {
		this.metamodel = metamodel;
	}

	@Override
	public final <TEntity> Collection<JsonChangeVector> getChangeVectors(TEntity older, TEntity newer) {
		var type = getObjectType(older);
		var id1 = getObjectId(older);
		// TODO: id2?

		return getAttributes(older)
			.stream()
			.map(attr -> {
				var val1 = getAttributeValue(attr, older);
				var val2 = getAttributeValue(attr, newer);
				if (!Objects.equals(val1, val2)) {
					final var attributeName = getAttributeName(attr);
					try {
						return new JsonChangeVector(type, id1, val1.getClass(), attributeName, convertValueToJson(val1));
					} catch (JsonException e) {
						throw new ChangeTrackingException(String.format(
							"Failed to create change vector for attribute '%s'",
							attributeName
						), e);
					}
				} else return null;
			})
			.filter(Objects::nonNull)
			.collect(Collectors.toList());
	}

	@Override
	public String convertValueToJson(Object object) {
		try {
			if (object instanceof String || object instanceof Number || object instanceof Boolean || object == null) {
				return objectMapper.writeValueAsString(object);
			} else return objectMapper.writeValueAsString(object.toString());
		} catch (JsonProcessingException e) {
			throw new JsonException("convert value to", e);
		}
	}

	@Override
	public Object convertValueFromJson(String type, String json) {
		try {
			for (var clazz : JopaEntityStrategy.supportedAttributeClasses) {
				if (Objects.equals(type, clazz.getCanonicalName()))
					return objectMapper.readValue(json, clazz);
			}
		} catch (JsonProcessingException e) {
			throw new JsonException("convert value of type '" + type + "' from", e);
		}

		throw new JsonException("convert value of unsupported type '" + type + "' from");
	}

	@Override
	public void checkClassSupported(Class<?> clazz) {
		if (!clazz.isAnnotationPresent(Audited.class) || !clazz.isAnnotationPresent(OWLClass.class))
			throw new ClassNotAuditedException(clazz);
	}

	@Override
	@SuppressWarnings("unchecked")
	public Collection<Attribute<?, ?>> getAttributes(Object entity) {
		return (Collection<Attribute<?, ?>>) metamodel.entity(entity.getClass()).getAttributes();
	}

	@Override
	public String getAttributeName(Attribute<?, ?> field) {
		return field.getIRI().toString();
	}

	@Override
	public Object getAttributeValue(Attribute<?, ?> field, Object instance) {
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
