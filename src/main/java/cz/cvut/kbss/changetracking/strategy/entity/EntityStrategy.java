package cz.cvut.kbss.changetracking.strategy.entity;

import cz.cvut.kbss.changetracking.model.ChangeVector;

import java.util.Collection;

/**
 * Entity class management strategy.
 * <p>
 * TODO: rename (EntityComparisonStrategy?)
 *
 * @param <TField> Generic high-level type of attributes/fields used in the entity. If an implementation of this
 *                strategy is not to use a metamodel, {@link java.lang.reflect.Field} should be used.
 */
public interface EntityStrategy<TField> {
  /**
   * Check if a class is supported for auditing and if not, throw
   * {@link cz.cvut.kbss.changetracking.exception.ClassNotAuditedException}.
   *
   * @throws cz.cvut.kbss.changetracking.exception.ClassNotAuditedException} If the class is not supported.
   */
  void checkClassSupported(Class<?> clazz);

  /**
   * Create change vectors between two revisions of an object. The revisions have to be mutually compatible, i.e.
	 * instances of the same class or have a common non-interface ancestor in the class hierarchy.
   *
   * @param older The older revision of the observed object.
   * @param newer The newer revision of the observed object.
   * @param requireSameId If true, throw an exception when the objects' IDs don't match.
	 * @return A collection of change vectors, representing the changes between the revisions.
   * @throws cz.cvut.kbss.changetracking.exception.ChangeTrackingException When vectors are not successfully created.
	 * @throws cz.cvut.kbss.changetracking.exception.IdNotMatchingException When the objects' IDs don't match and
	 * {@code requireSameId} is {@code true}.
   */
  <T> Collection<ChangeVector> getChangeVectors(T older, T newer, boolean requireSameId);

  /**
   * Get an application-unique string representation of the object's type. This MAY be the name of the object's class.
   *
   * @param o The object.
   * @return An application-unique string representation of the object's type.
   */
	default String getObjectType(Object o) {
		return getTypeName(o.getClass());
	}

	/**
	 * Get an application-unique string representation of the class (type). This MAY be the name of the class.
	 *
	 * @see EntityStrategy#getObjectType(Object)
	 */
	String getTypeName(Class<?> clazz);

  /**
   * Get an identifier of the object which is unique within its type.
   *
   * @param o The object.
   * @return A unique identifier of the object within its type.
   * @see EntityStrategy#getObjectType(Object)
   */
  String getObjectId(Object o);

	/**
	 * Get the attributes/fields of the supplied entity type.
	 *
	 * @param clazz The entity type whose attributes to get.
	 * @return An iterable collection of the attributes.
	 */
  Collection<TField> getAttributes(Class<?> clazz);

	/**
   * Get the attributes/fields of the supplied entity's type.
	 *
   * @param object The entity whose attributes to get.
   * @return An iterable collection of the attributes.
   */
  default Collection<TField> getAttributes(Object object) {
		return getAttributes(object.getClass());
	}

  /**
   * Get the name of an attribute/field.
	 *
   * @param field The attribute/field to get the name of.
   * @return The name of the attribute/field.
   */
  String getAttributeName(TField field);

  /**
   * Get the value of an attribute on an entity instance.
   * @param field The attribute.
   * @param instance The entity.
   * @return The value of the attribute on the instance.
   */
  Object getAttributeValue(TField field, Object instance);
}
