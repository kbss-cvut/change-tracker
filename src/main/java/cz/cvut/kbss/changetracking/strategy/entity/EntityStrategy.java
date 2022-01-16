package cz.cvut.kbss.changetracking.strategy.entity;

import cz.cvut.kbss.changetracking.model.JsonChangeVector;

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
   * Create change vectors between two revisions of an object.
   *
   * @param older The older revision of the observed object.
   * @param newer The newer revision of the observed object.
   * @return A collection of change vectors, representing the changes between the revisions.
   * @throws cz.cvut.kbss.changetracking.exception.ChangeTrackingException When vectors are not successfully created.
   */
  <T> Collection<JsonChangeVector> getChangeVectors(T older, T newer);

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

  /**
   * Get the attributes/fields of the supplied entity's type.
   * @param object The entity whose attributes to get.
   * @return An iterable collection of the attributes.
   */
  Collection<TField> getAttributes(Object object);

  /**
   * Get the name of an attribute/field.
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
