package cz.cvut.kbss.changetracking.strategy.entity;

import cz.cvut.kbss.changetracking.model.ChangeVector;

import java.util.Collection;

/**
 * Entity class management strategy.
 * <p>
 * TODO: rename (EntityComparisonStrategy?)
 *
 * @param <TField> Generic high-level type of attributes/fields used in the entity. If an implementation of this
 *                 strategy is not to use a metamodel, {@link java.lang.reflect.Field} should be used.
 */
public interface EntityStrategy<TField> {

	/**
	 * Create change vectors between two revisions of an object. The revisions have to be mutually compatible, i.e.
	 * instances of the same class or have a common non-interface ancestor in the class hierarchy.
	 *
	 * @param older         The older revision of the observed object.
	 * @param newer         The newer revision of the observed object.
	 * @param requireSameId If true, throw an exception when the objects' IDs don't match.
	 * @return A collection of change vectors, representing the changes between the revisions.
	 * @throws cz.cvut.kbss.changetracking.exception.ChangeTrackingException When vectors are not successfully created.
	 * @throws cz.cvut.kbss.changetracking.exception.IdNotMatchingException  When the objects' IDs don't match and {@code
	 *                                                                       requireSameId} is {@code true}.
	 */
	<T> Collection<ChangeVector> getChangeVectors(T older, T newer, boolean requireSameId);

}
