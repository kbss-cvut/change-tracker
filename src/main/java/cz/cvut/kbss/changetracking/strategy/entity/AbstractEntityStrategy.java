package cz.cvut.kbss.changetracking.strategy.entity;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cvut.kbss.changetracking.exception.ChangeTrackingException;
import cz.cvut.kbss.changetracking.exception.JsonException;
import cz.cvut.kbss.changetracking.model.JsonChangeVector;

import java.util.*;
import java.util.stream.Collectors;

public abstract class AbstractEntityStrategy<TEntity, TField> implements EntityStrategy<TEntity> {
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
	protected final ObjectMapper objectMapper = new ObjectMapper();

	protected abstract Collection<TField> getAttributes(TEntity entity);

	protected abstract String getAttributeName(TField field);

	protected abstract Object getAttributeValue(TField field, TEntity instance);

	@Override
	public final Collection<JsonChangeVector> getChangeVectors(TEntity older, TEntity newer) {
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
			for (var clazz : supportedAttributeClasses) {
				if (Objects.equals(type, clazz.getCanonicalName()))
					return objectMapper.readValue(json, clazz);
			}
		} catch (JsonProcessingException e) {
			throw new JsonException("convert value of type '" + type + "' from", e);
		}

		throw new JsonException("convert value of unsupported type '" + type + "' from");
	}
}
