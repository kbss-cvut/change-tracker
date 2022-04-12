package cz.cvut.kbss.changetracking.strategy.storage;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cvut.kbss.changetracking.exception.JsonException;
import cz.cvut.kbss.changetracking.exception.UnsupportedAttributeTypeException;
import cz.cvut.kbss.changetracking.model.ChangeVector;
import cz.cvut.kbss.changetracking.model.JsonChangeVector;
import cz.cvut.kbss.changetracking.util.ClassUtil;

import java.util.*;
import java.util.stream.Collectors;

/**
 * {@link StorageStrategy} contains code to be reused by all storage strategies that use JSON as the serialization
 * format for attribute values.
 */
public abstract class JsonBasedStorageStrategy implements StorageStrategy {
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
		boolean.class,
		Collection.class // TODO: implement without being naive
	));
	protected final ObjectMapper objectMapper = new ObjectMapper();

	/**
	 * Convert vectors from {@link JsonChangeVector} to {@link ChangeVector}, i.e. convert their previousValues
	 * from JSON back to their original types.
	 *
	 * @param jsonList The list of vectors to convert.
	 * @return Vectors with proper values.
	 * @throws cz.cvut.kbss.changetracking.exception.JsonException Based on
	 * {@link JpaStorageStrategy#convertValueFromJson}.
	 */
	List<ChangeVector<?>> convertVectorsFromJson(List<JsonChangeVector> jsonList) {
		return jsonList.stream().map(jsonVector -> new ChangeVector<>(
			jsonVector,
			convertValueFromJson(jsonVector.getAttributeType(), jsonVector.getPreviousValue())
		)).collect(Collectors.toList());
	}

	/**
	 * Convert a value of any supported attribute type into JSON.
	 *
	 * @param object object The value.
	 * @return A JSON representation of the value.
	 * @throws cz.cvut.kbss.changetracking.exception.JsonException When conversion to JSON fails.
	 */
	String convertValueToJson(Object object) {
		try {
			if (supportedAttributeClasses.stream().anyMatch(clazz -> clazz.isInstance(object))) {
				return objectMapper.writeValueAsString(object);
			}
			throw new UnsupportedAttributeTypeException(object.getClass().getCanonicalName());
		} catch (JsonProcessingException e) {
			throw new JsonException("convert value to", e);
		}
	}

	/**
	 * Convert a value from JSON to a type supported by this {@link StorageStrategy}.
	 *
	 * @param type Fully qualified class name of the type.
	 * @param json JSON representation of the value.
	 * @return The original value.
	 * @throws JsonException When conversion from JSON fails, for example if the class is not supported.
	 */
	Object convertValueFromJson(String type, String json) {
		try {
			for (var clazz : supportedAttributeClasses) {
				var className = clazz.getCanonicalName();
				if (Objects.equals(type, className)) {
					return objectMapper.readValue(json, clazz);
				} else {
					var arrayClass = ClassUtil.getArrayClass(clazz);
					if (Objects.equals(type, arrayClass.getCanonicalName())) {
						return List.of(objectMapper.readValue(json, arrayClass));
					}
				}
			}
		} catch (JsonProcessingException e) {
			throw new JsonException("convert value of type '" + type + "' from", e);
		}

		throw new UnsupportedAttributeTypeException(type);
	}
}
