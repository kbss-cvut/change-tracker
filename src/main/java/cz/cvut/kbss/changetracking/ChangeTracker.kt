package cz.cvut.kbss.changetracking

import cz.cvut.kbss.changetracking.exception.ObjectsNotCompatibleException
import cz.cvut.kbss.changetracking.model.ChangeVector
import cz.cvut.kbss.changetracking.strategy.entity.EntityStrategy
import cz.cvut.kbss.changetracking.strategy.storage.StorageStrategy
import java.time.Instant
import java.util.*

class ChangeTracker(var entityStrategy: EntityStrategy, var storageStrategy: StorageStrategy) {

	/**
	 * The same as the standard compare, but ignoring changes in the objects' identifiers: if it is not equal, no
	 * exception will be thrown but a change vector WILL NOT be created for the identifier.
	 *
	 * @see .compare
	 */
	fun compareIgnoringIds(older: Any, newer: Any, authorId: String?): Collection<ChangeVector<*>> {
		Objects.requireNonNull(older)
		Objects.requireNonNull(newer)
		val vectors = entityStrategy.getChangeVectors(older, newer, false)
		return if (authorId == null) vectors else vectors.onEach { it.authorId = authorId }
	}

	/**
	 * Compare two revisions of an object. If the objects' IDs don't match, throw an exception.
	 *
	 * @param older The older revision of the object.
	 * @param newer The newer revision of the object.
	 * @param authorId An application-unique identifier of the entity responsible for the change. If specified, it will
	 * be propagated to all created vectors.
	 * @return A collection of change vectors between the two revisions.
	 * @throws ObjectsNotCompatibleException                                  If the objects are not mutually compatible.
	 * @throws cz.cvut.kbss.changetracking.exception.ClassNotAuditedException If at least one of the objects' classes is
	 * supported by the current [                                                                        ].
	 * @throws cz.cvut.kbss.changetracking.exception.IdNotMatchingException   When the objects' IDs don't match.
	 */
	fun compare(older: Any, newer: Any, authorId: String?): Collection<ChangeVector<*>> {
		Objects.requireNonNull(older)
		Objects.requireNonNull(newer)
		val vectors = entityStrategy.getChangeVectors(older, newer, true)
		return if (authorId == null) vectors else vectors.onEach { it.authorId = authorId }
	}

	/**
	 * Compare two revisions of an object and save the resulting change vectors.
	 *
	 * @param older The older revision of the object.
	 * @param newer The newer revision of the object.
	 * @param authorId An application-unique identifier of the entity responsible for the change. If specified, it will
	 * be propagated to all created vectors.
	 * @param <T>   The object's type.
	 * @throws ObjectsNotCompatibleException                                  If the objects are not mutually compatible.
	 * @throws cz.cvut.kbss.changetracking.exception.ClassNotAuditedException If at least one of the objects' classes is
	 * supported by the current [                                                                        ].
	 * @throws cz.cvut.kbss.changetracking.exception.IdNotMatchingException   When the objects' IDs don't match.
	</T> */
	fun <T : Any> compareAndSave(older: T, newer: T, authorId: String?) {
		val vectors = compare(older, newer, authorId)
		storageStrategy.save(*vectors.toTypedArray())
	}

	/**
	 * Get all change vectors for an object.
	 *
	 * This method calls [StorageStrategy.getAllForObject].
	 */
	fun getAllForObject(objectType: String, objectId: String): List<ChangeVector<*>> {
		return storageStrategy.getAllForObject(objectType, objectId)
	}

	/**
	 * Get all change vectors since a timestamp (inclusive).
	 *
	 * This method calls [StorageStrategy.getChangesSince].
	 */
	fun getChangesSince(timestamp: Instant): List<ChangeVector<*>> = storageStrategy.getChangesSince(timestamp)

	/**
	 * Get all change vectors of an object type since a timestamp (inclusive).
	 *
	 * This method calls [StorageStrategy.getChangesOfTypeSince] and converts the attributes'
	 * values to the original types.
	 */
	fun getChangesOfTypeSince(timestamp: Instant, objectType: String): List<ChangeVector<*>> {
		return storageStrategy.getChangesOfTypeSince(timestamp, objectType)
	}
}
