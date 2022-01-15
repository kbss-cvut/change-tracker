package cz.cvut.kbss.changetracking.strategy.storage;

import cz.cvut.kbss.changetracking.model.JsonChangeVector;

import java.time.Instant;
import java.util.List;

/**
 * Strategy for storing and accessing change vectors.
 */
public interface StorageStrategy {
	/**
	 * Persist the provided change vectors to the underlying store.
	 *
	 * @param vectors The change vectors to be persisted.
	 * @apiNote The vectors are already expected to contain JSON-encoded attribute values.
	 */
	void save(JsonChangeVector... vectors);

	/**
	 * Get all change vectors for an object identified by its type and ID.
	 *
	 * @param objectType String representation of the object's type.
	 * @param objectId   String representation of the object's identifier.
	 * @return A list of change vectors for the given object, possibly empty, sorted descending by timestamp.
	 * @apiNote Beware that the change vectors' attribute values are JSON-encoded in the {@link JsonChangeVector}s
	 * returned.
	 */
	List<JsonChangeVector> getAllForObject(String objectType, String objectId);

	/**
	 * Get all change vectors since a timestamp (inclusive).
	 *
	 * @param timestamp Inclusive lower boundary of change vectors returned.
	 * @return A list of change vectors since the timestamp, possibly empty, sorted descending by timestamp.
	 * @apiNote Beware that the change vectors' attribute values are JSON-encoded in the {@link JsonChangeVector}s
	 * returned.
	 */
	List<JsonChangeVector> getChangesSince(Instant timestamp);

	/**
	 * Get all changed objects of a given type since a timestamp (inclusive).
	 *
	 * @param timestamp  Inclusive lower boundary of change vectors returned.
	 * @param objectType String representation of the objects' type.
	 * @return A list of change vectors, possibly empty, sorted descending by timestamp.
	 * @apiNote Beware that the change vectors' attribute values are JSON-encoded in the {@link JsonChangeVector}s
	 * returned.
	 */
	List<JsonChangeVector> getChangesOfTypeSince(Instant timestamp, String objectType);
}
