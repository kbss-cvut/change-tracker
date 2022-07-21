package cz.cvut.kbss.changetracking.strategy.entity

import cz.cvut.kbss.changetracking.model.ChangeVector

/**
 * Entity class management strategy.
 */
interface EntityStrategy {
	/**
	 * Create change vectors between two revisions of an object. The revisions have to be mutually compatible, i.e.
	 * instances of the same class or have a common non-interface ancestor in the class hierarchy.
	 *
	 *
	 * This method is NOT responsible for setting [ChangeVector.authorId]!
	 *
	 * @param older         The older revision of the observed object.
	 * @param newer         The newer revision of the observed object.
	 * @param requireSameId If true, throw an exception when the objects' IDs don't match.
	 * @return A collection of change vectors, representing the changes between the revisions.
	 * @throws cz.cvut.kbss.changetracking.exception.ChangeTrackingException When vectors are not successfully created.
	 * @throws cz.cvut.kbss.changetracking.exception.IdNotMatchingException  When the objects' IDs don't match and `requireSameId` is `true`.
	 */
	fun <T : Any> getChangeVectors(older: T, newer: T, requireSameId: Boolean): Collection<ChangeVector<*>>
}
