package cz.cvut.kbss.changetracking.strategy.storage;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cvut.kbss.changetracking.exception.JsonException;
import cz.cvut.kbss.changetracking.exception.UnsupportedAttributeTypeException;
import cz.cvut.kbss.changetracking.model.ChangeVector;
import cz.cvut.kbss.changetracking.model.JsonChangeVector;
import cz.cvut.kbss.changetracking.util.ClassUtil;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

/**
 * {@link StorageStrategy} contains code to be reused by all storage strategies that use JSON as the serialization
 * format for attribute values.
 */
public abstract class JsonBasedStorageStrategy implements StorageStrategy {
	protected final ObjectMapper objectMapper;

	/**
	 * Sole constructor for subclasses.
	 * @param objectMapper A custom instance of objectMapper to use. If not passed here, a default will be created.
	 */
	protected JsonBasedStorageStrategy(
		@Nullable ObjectMapper objectMapper
	) {
		if (objectMapper != null) {
			this.objectMapper = objectMapper;
		} else {
			this.objectMapper = new ObjectMapper();
			this.objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		}
	}

	@Override
	public void save(ChangeVector<?>... rawVectors) {
		Arrays
			.stream(rawVectors)
			.map(vector -> {
				var value = vector.getPreviousValue();
				var valueClass = value == null ? Object.class : value.getClass();

				return new JsonChangeVector(
					vector,
					valueClass,
					convertValueToJson(value)
				);
			})
			.forEach(this::saveVector);
	}

	/**
	 * Save a JSON change vector to the database.
	 * @param vector The vector to save.
	 */
	protected abstract void saveVector(JsonChangeVector vector);

	/**
	 * Convert vectors from {@link JsonChangeVector} to {@link ChangeVector}, i.e. convert their previousValues
	 * from JSON back to their original types.
	 *
	 * @param jsonList The list of vectors to convert.
	 * @return Vectors with proper values.
	 * @throws cz.cvut.kbss.changetracking.exception.JsonException Based on
	 * {@link JpaStorageStrategy#convertValueFromJson}.
	 */
	protected List<ChangeVector<?>> convertVectorsFromJson(List<JsonChangeVector> jsonList) {
		return jsonList.stream().map(jsonVector -> new ChangeVector<>(
			jsonVector,
			convertValueFromJson(jsonVector.getAttributeType(), jsonVector.getPreviousValue())
		)).collect(Collectors.toList());
	}

	/**
	 * Convert a value into JSON.
	 *
	 * @param object object The value.
	 * @return A JSON representation of the value.
	 * @throws cz.cvut.kbss.changetracking.exception.JsonException When conversion to JSON fails.
	 */
	protected String convertValueToJson(Object object) {
		try {
			return objectMapper.writeValueAsString(object);
		} catch (JsonProcessingException e) {
			throw new JsonException("convert value to", e);
		}
	}

	/**
	 * Convert a value from JSON to the original type.
	 *
	 * @param type Fully qualified class name of the type.
	 * @param json JSON representation of the value.
	 * @return The original value.
	 * @throws JsonException When conversion from JSON fails, for example if the class is not supported.
	 * @throws UnsupportedAttributeTypeException When the target class can't be found.
	 */
	protected Object convertValueFromJson(String type, String json) {
		try {
			var arrayClassOptional = ClassUtil.getArrayClassByName(type);
			if (arrayClassOptional.isPresent()) {
				Class<Object[]> arrayClass = arrayClassOptional.get();
				return List.of(objectMapper.readValue(json, arrayClass));
			} else {
				var clazz = Class.forName(type);
				return objectMapper.readValue(json, clazz);
			}
		} catch (JsonProcessingException e) {
			throw new JsonException("convert value of type '" + type + "' from", e);
		} catch (ClassNotFoundException e) {
			throw new UnsupportedAttributeTypeException(type);
		}
	}
}
