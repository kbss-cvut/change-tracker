package cz.cvut.kbss.changetracking.strategy.storage

import cz.cvut.kbss.changetracking.model.ChangeVector
import java.time.Instant

/**
 * Strategy for storing and accessing change vectors.
 */
interface StorageStrategy {
	/**
	 * Persist the provided change vectors to the underlying store.
	 *
	 * @param vectors The change vectors to be persisted.
	 * @implNote This method is responsible for performing any potentially required serialization.
	 */
	fun save(vararg vectors: ChangeVector<*>)

	/**
	 * Get all change vectors for an object identified by its type and ID.
	 *
	 * @param objectType String representation of the object's type.
	 * @param objectId   String representation of the object's identifier.
	 * @return A list of change vectors for the given object, possibly empty, sorted descending by timestamp.
	 * @apiNote This method is responsible for performing any potentially required deserialization of attribute values.
	 */
	fun getAllForObject(objectType: String, objectId: String): List<ChangeVector<*>>

	/**
	 * Get all change vectors since a timestamp (inclusive).
	 *
	 * @param timestamp Inclusive lower boundary of change vectors returned.
	 * @return A list of change vectors since the timestamp, possibly empty, sorted descending by timestamp.
	 * @apiNote This method is responsible for performing any potentially required deserialization of attribute values.
	 */
	fun getChangesSince(timestamp: Instant): List<ChangeVector<*>>

	/**
	 * Get all changed objects of a given type since a timestamp (inclusive).
	 *
	 * @param timestamp  Inclusive lower boundary of change vectors returned.
	 * @param objectType Nullable string representation of the objects' type.
	 * @return A list of change vectors, possibly empty, sorted descending by timestamp.
	 * @apiNote This method is responsible for performing any potentially required deserialization of attribute values.
	 */
	fun getChangesOfTypeSince(timestamp: Instant, objectType: String?): List<ChangeVector<*>>
}
