package cz.cvut.kbss.changetracking.strategy.entity;

import cz.cvut.kbss.changetracking.model.JsonChangeVector;

import java.util.Collection;

/**
 * Entity class management strategy.
 * <p>
 * TODO: rename (EntityComparisonStrategy?)
 *
 * @param <TEntity> Type of all entities supported by the strategy. If the entity classes do not share a common
 *                  superclass, {@link Object} can be used.
 */
public interface EntityStrategy<TEntity> {
  /**
   * Check if a class is supported for auditing and if not, throw
   * {@link cz.cvut.kbss.changetracking.exception.ClassNotAuditedException}.
   *
   * @throws cz.cvut.kbss.changetracking.exception.ClassNotAuditedException} If the class is not supported.
   */
  void checkClassSupported(Class<?> clazz);

  /**
   * Create change vectors between two revisions of an object.
   *
   * @param older The older revision of the observed object.
   * @param newer The newer revision of the observed object.
   * @return A collection of change vectors, representing the changes between the revisions.
   * @throws cz.cvut.kbss.changetracking.exception.ChangeTrackingException When vectors are not successfully created.
   */
  Collection<JsonChangeVector> getChangeVectors(TEntity older, TEntity newer);

  /**
   * Get an application-unique string representation of the object's type. This MAY be the name of the object's class.
   *
   * @param o The object.
   * @return An application-unique string representation of the object's type.
   */
  String getObjectType(Object o);

  /**
   * Get an identifier of the object which is unique within its type.
   *
   * @param o The object.
   * @return A unique identifier of the object within its type.
   * @see EntityStrategy#getObjectType(Object)
   */
  String getObjectId(Object o);

  /**
   * Convert a value of any supported attribute type into JSON.
   *
   * @param object object The value.
   * @return A JSON representation of the value.
   * @throws cz.cvut.kbss.changetracking.exception.JsonException When conversion to JSON fails.
   */
  String convertValueToJson(Object object);

  /**
   * Convert a value from JSON to a type supported by this {@link EntityStrategy}.
   *
   * @param type Fully qualified class name of the type.
   * @param json JSON representation of the value.
   * @return The original value.
   * @throws cz.cvut.kbss.changetracking.exception.JsonException When conversion from JSON fails, for example if the
   *                                                             class is not supported.
   */
  Object convertValueFromJson(String type, String json);
}
