package cz.cvut.kbss.changetracking.strategy.storage

import com.fasterxml.jackson.core.JsonProcessingException
import com.fasterxml.jackson.databind.DeserializationFeature
import com.fasterxml.jackson.databind.ObjectMapper
import cz.cvut.kbss.changetracking.exception.JsonException
import cz.cvut.kbss.changetracking.exception.UnsupportedAttributeTypeException
import cz.cvut.kbss.changetracking.model.ChangeVector
import cz.cvut.kbss.changetracking.model.JsonChangeVector
import cz.cvut.kbss.changetracking.util.ClassUtil

/**
 * [StorageStrategy] contains code to be reused by all storage strategies that use JSON as the serialization
 * format for attribute values.
 *
 * @constructor Sole constructor for subclasses.
 * @param objectMapper A custom instance of objectMapper to use. If not passed here, a default will be created.
 */
abstract class JsonBasedStorageStrategy protected constructor(
	objectMapper: ObjectMapper?
) : StorageStrategy {
	protected val objectMapper: ObjectMapper

	init {
		if (objectMapper != null) {
			this.objectMapper = objectMapper
		} else {
			this.objectMapper = ObjectMapper()
			this.objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
		}
	}

	override fun save(vararg vectors: ChangeVector<*>) {
		vectors.map {
			val value = it.previousValue
			val valueClass = value?.javaClass ?: Any::class.java
			return@map JsonChangeVector(it, valueClass, convertValueToJson(value))
		}.forEach(::saveVector)
	}

	/**
	 * Save a JSON change vector to the database.
	 * @param vector The vector to save.
	 */
	protected abstract fun saveVector(vector: JsonChangeVector)

	/**
	 * Convert vectors from [JsonChangeVector] to [ChangeVector], i.e. convert their previousValues
	 * from JSON back to their original types.
	 *
	 * @param jsonList The list of vectors to convert.
	 * @return Vectors with proper values.
	 * @throws cz.cvut.kbss.changetracking.exception.JsonException Based on
	 * [JpaStorageStrategy.convertValueFromJson].
	 */
	protected fun convertVectorsFromJson(jsonList: List<JsonChangeVector>): List<ChangeVector<*>> {
		return jsonList.map { ChangeVector(it, convertValueFromJson(it.attributeType!!, it.previousValue!!)) }
	}

	/**
	 * Convert a value into JSON.
	 *
	 * @param obj object The value.
	 * @return A JSON representation of the value.
	 * @throws cz.cvut.kbss.changetracking.exception.JsonException When conversion to JSON fails.
	 */
	fun convertValueToJson(obj: Any?): String {
		return try {
			objectMapper.writeValueAsString(obj)
		} catch (e: JsonProcessingException) {
			throw JsonException("convert value to", e)
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
	fun convertValueFromJson(type: String, json: String): Any {
		return try {
			val arrayClassOptional = ClassUtil.getArrayClassByName<Any>(type)
			if (arrayClassOptional.isPresent) {
				val arrayClass = arrayClassOptional.get()
				listOf(*objectMapper.readValue(json, arrayClass))
			} else {
				val clazz = Class.forName(type)
				objectMapper.readValue(json, clazz)
			}
		} catch (e: JsonProcessingException) {
			throw JsonException("convert value of type '$type' from", e)
		} catch (e: ClassNotFoundException) {
			throw UnsupportedAttributeTypeException(type)
		}
	}
}
