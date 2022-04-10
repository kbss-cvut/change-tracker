package cz.cvut.kbss.changetracking.strategy.entity;

import java.util.Collection;

/**
 * Base abstract implementation of {@link EntityStrategy} including methods that are likely to be required for different
 * concrete implementations.
 *
 * @param <TField> Generic high-level type of attributes/fields used in the entity. If an implementation of this
 *                 strategy is not to use a metamodel, {@link java.lang.reflect.Field} should be used.
 */
public abstract class BaseEntityStrategy<TField> implements EntityStrategy {
	/**
	 * Check if a class is supported for auditing and if not, throw
	 * {@link cz.cvut.kbss.changetracking.exception.ClassNotAuditedException}.
	 *
	 * @throws cz.cvut.kbss.changetracking.exception.ClassNotAuditedException If the class is not supported.
	 */
	public abstract void checkClassSupported(Class<?> clazz);

	/**
	 * Get an application-unique string representation of the object's type. This MAY be the name of the object's class.
	 *
	 * @param o The object.
	 * @return An application-unique string representation of the object's type.
	 */
	public String getObjectType(Object o) {
		return getTypeName(o.getClass());
	}

	/**
	 * Get an application-unique string representation of the class (type). This MAY be the name of the class.
	 *
	 * @see BaseEntityStrategy#getObjectType(Object)
	 */
	public abstract String getTypeName(Class<?> clazz);

	/**
	 * Get an identifier of the object which is unique within its type.
	 *
	 * @param o The object.
	 * @return A unique identifier of the object within its type.
	 * @see BaseEntityStrategy#getObjectType(Object)
	 */
	public abstract String getObjectId(Object o);

	/**
	 * Get the attributes/fields of the supplied entity type.
	 *
	 * @param clazz The entity type whose attributes to get.
	 * @return An iterable collection of the attributes.
	 */
	public abstract Collection<TField> getAttributes(Class<?> clazz);

	/**
	 * Get the attributes/fields of the supplied entity's type.
	 *
	 * @param object The entity whose attributes to get.
	 * @return An iterable collection of the attributes.
	 */
	public Collection<TField> getAttributes(Object object) {
		return getAttributes(object.getClass());
	}

	/**
	 * Get the name of an attribute/field.
	 *
	 * @param field The attribute/field to get the name of.
	 * @return The name of the attribute/field.
	 */
	public abstract String getAttributeName(TField field);

	/**
	 * Get the value of an attribute on an entity instance.
	 *
	 * @param field    The attribute.
	 * @param instance The entity.
	 * @return The value of the attribute on the instance.
	 */
	public abstract Object getAttributeValue(TField field, Object instance);
}
