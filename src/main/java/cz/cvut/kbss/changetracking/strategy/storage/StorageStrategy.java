package cz.cvut.kbss.changetracking.strategy.storage;

import cz.cvut.kbss.changetracking.model.ChangeVector;

import java.time.Instant;
import java.util.Collection;

/**
 * Strategy for storing and accessing change vectors.
 */
public interface StorageStrategy {
  /**
   * Persist the provided change vectors to the underlying store.
   *
   * @param vectors The change vectors to be persisted.
   */
  void save(ChangeVector... vectors);

  /**
   * Get all change vectors for an object identified by its type and ID.
   *
   * @param objectType String representation of the object's type.
   * @param objectId   String representation of the object's identifier.
   * @return A collection of change vectors for the given object, possibly empty.
   */
  Collection<ChangeVector> getAllForObject(String objectType, String objectId);

  /**
   * Get all change vectors since a timestamp (inclusive).
   *
   * @param timestamp Inclusive lower boundary of change vectors returned.
   * @return A collection of change vectors since the timestamp, possibly empty.
   */
  Collection<ChangeVector> getChangesSince(Instant timestamp);

  /**
   * Get all changed objects of a given type since a timestamp (inclusive).
   *
   * @param timestamp  Inclusive lower boundary of change vectors returned.
   * @param objectType String representation of the objects' type.
   * @return A collection of change vectors, possibly empty.
   */
  Collection<ChangeVector> getChangesOfTypeSince(Instant timestamp, String objectType);
}
