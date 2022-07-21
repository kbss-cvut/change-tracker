package cz.cvut.kbss.changetracking.strategy.entity

/**
 * Base abstract implementation of [EntityStrategy] including methods that are likely to be required for different
 * concrete implementations.
 *
 * @param <TField> Generic high-level type of attributes/fields used in the entity. If an implementation of this
 * strategy is not to use a metamodel, [java.lang.reflect.Field] should be used.
 */
abstract class BaseEntityStrategy<TField : Any> : EntityStrategy {
	/**
	 * Check if a class is supported for auditing and if not, throw
	 * [cz.cvut.kbss.changetracking.exception.ClassNotAuditedException].
	 *
	 * @throws cz.cvut.kbss.changetracking.exception.ClassNotAuditedException If the class is not supported.
	 */
	abstract fun checkClassSupported(clazz: Class<*>)

	/**
	 * Get an application-unique string representation of the object's type. This MAY be the name of the object's class.
	 *
	 * @param o The object.
	 * @return An application-unique string representation of the object's type.
	 */
	fun getObjectType(o: Any): String = getTypeName(o.javaClass)

	/**
	 * Get an application-unique string representation of the class (type). This MAY be the name of the class.
	 *
	 * @see BaseEntityStrategy.getObjectType
	 */
	abstract fun getTypeName(clazz: Class<*>): String

	/**
	 * Get an identifier of the object which is unique within its type.
	 *
	 * @param o The object.
	 * @return A unique identifier of the object within its type.
	 * @see BaseEntityStrategy.getObjectType
	 */
	abstract fun getObjectId(o: Any): String

	/**
	 * Get the attributes/fields of the supplied entity type.
	 *
	 * @param clazz The entity type whose attributes to get.
	 * @return An iterable collection of the attributes.
	 */
	abstract fun getAttributes(clazz: Class<*>): Collection<TField>

	/**
	 * Get the attributes/fields of the supplied entity's type.
	 *
	 * @param o The entity whose attributes to get.
	 * @return An iterable collection of the attributes.
	 */
	fun getObjectAttributes(o: Any): Collection<TField> = getAttributes(o.javaClass)

	/**
	 * Get the name of an attribute/field.
	 *
	 * @param field The attribute/field to get the name of.
	 * @return The name of the attribute/field.
	 */
	abstract fun getAttributeName(field: TField): String?

	/**
	 * Get the value of an attribute on an entity instance.
	 *
	 * @param field    The attribute.
	 * @param instance The entity.
	 * @return The value of the attribute on the instance.
	 */
	abstract fun getAttributeValue(field: TField, instance: Any): Any?
}
