package cz.cvut.kbss.changetracking;

import cz.cvut.kbss.changetracking.exception.ObjectsNotCompatibleException;
import cz.cvut.kbss.changetracking.model.ChangeVector;
import cz.cvut.kbss.changetracking.strategy.entity.EntityStrategy;
import cz.cvut.kbss.changetracking.strategy.storage.StorageStrategy;

import java.time.Instant;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class ChangeTracker {

	protected EntityStrategy entityStrategy;
	protected StorageStrategy storageStrategy;

	public ChangeTracker(EntityStrategy entityStrategy, StorageStrategy storageStrategy) {
		this.entityStrategy = entityStrategy;
		this.storageStrategy = storageStrategy;
	}

	public void setClassStrategy(EntityStrategy entityStrategy) {
		this.entityStrategy = entityStrategy;
	}

	public void setStorageStrategy(StorageStrategy storageStrategy) {
		this.storageStrategy = storageStrategy;
	}

	/**
	 * The same as the standard compare, but ignoring changes in the objects' identifiers: if it is not equal, no
	 * exception will be thrown but a change vector WILL NOT be created for the identifier.
	 *
	 * @see #compare(Object, Object, String)
	 */
	public Collection<ChangeVector<?>> compareIgnoringIds(Object older, Object newer, String authorId) {
		Objects.requireNonNull(older);
		Objects.requireNonNull(newer);

		var vectors = entityStrategy.getChangeVectors(older, newer, false);

		if (authorId == null)
			return vectors;

		return vectors
			.stream()
			.peek(vector -> vector.setAuthorId(authorId))
			.collect(Collectors.toList());
	}

	/**
	 * Compare two revisions of an object. If the objects' IDs don't match, throw an exception.
	 *
	 * @param older The older revision of the object.
	 * @param newer The newer revision of the object.
	 * @param authorId An application-unique identifier of the entity responsible for the change. If specified, it will
	 *                 be propagated to all created vectors.
	 * @return A collection of change vectors between the two revisions.
	 * @throws ObjectsNotCompatibleException                                  If the objects are not mutually compatible.
	 * @throws cz.cvut.kbss.changetracking.exception.ClassNotAuditedException If at least one of the objects' classes is
	 *                                                                        supported by the current {@link
	 *                                                                        EntityStrategy}.
	 * @throws cz.cvut.kbss.changetracking.exception.IdNotMatchingException   When the objects' IDs don't match.
	 */
	public Collection<ChangeVector<?>> compare(Object older, Object newer, String authorId) {
		Objects.requireNonNull(older);
		Objects.requireNonNull(newer);

		var vectors = entityStrategy.getChangeVectors(older, newer, true);

		if (authorId == null)
			return vectors;

		return vectors
			.stream()
			.peek(vector -> vector.setAuthorId(authorId))
			.collect(Collectors.toList());
	}

	/**
	 * Compare two revisions of an object and save the resulting change vectors.
	 *
	 * @param older The older revision of the object.
	 * @param newer The newer revision of the object.
	 * @param authorId An application-unique identifier of the entity responsible for the change. If specified, it will
	 *                 be propagated to all created vectors.
	 * @param <T>   The object's type.
	 * @throws ObjectsNotCompatibleException                                  If the objects are not mutually compatible.
	 * @throws cz.cvut.kbss.changetracking.exception.ClassNotAuditedException If at least one of the objects' classes is
	 *                                                                        supported by the current {@link
	 *                                                                        EntityStrategy}.
	 * @throws cz.cvut.kbss.changetracking.exception.IdNotMatchingException   When the objects' IDs don't match.
	 */
	public <T> void compareAndSave(T older, T newer, String authorId) {
		var vectors = compare(older, newer, authorId);
		storageStrategy.save(vectors.toArray(ChangeVector<?>[]::new));
	}

	/**
	 * Get all change vectors for an object.
	 * <p>
	 * This method calls {@link StorageStrategy#getAllForObject(String, String)}.
	 */
	public List<ChangeVector<?>> getAllForObject(String objectType, String objectId) {
		return storageStrategy.getAllForObject(objectType, objectId);
	}

	/**
	 * Get all change vectors since a timestamp (inclusive).
	 * <p>
	 * This method calls {@link StorageStrategy#getChangesSince(Instant)}.
	 */
	public List<ChangeVector<?>> getChangesSince(Instant timestamp) {
		return storageStrategy.getChangesSince(timestamp);
	}

	/**
	 * Get all change vectors of an object type since a timestamp (inclusive).
	 * <p>
	 * This method calls {@link StorageStrategy#getChangesOfTypeSince(Instant, String)} and converts the attributes'
	 * values to the original types.
	 */
	public List<ChangeVector<?>> getChangesOfTypeSince(Instant timestamp, String objectType) {
		return storageStrategy.getChangesOfTypeSince(timestamp, objectType);
	}
}
