package cz.cvut.kbss.changetracking.strategy.entity

import cz.cvut.kbss.changetracking.annotation.Audited
import cz.cvut.kbss.changetracking.annotation.IgnoreChanges
import cz.cvut.kbss.changetracking.exception.AccessDeniedException
import cz.cvut.kbss.changetracking.exception.ClassNotAuditedException
import cz.cvut.kbss.changetracking.exception.IdNotMatchingException
import cz.cvut.kbss.changetracking.exception.ObjectsNotCompatibleException
import cz.cvut.kbss.changetracking.model.ChangeVector
import cz.cvut.kbss.changetracking.util.ClassUtil
import cz.cvut.kbss.jopa.model.PersistenceProperties
import cz.cvut.kbss.jopa.model.annotations.OWLClass
import cz.cvut.kbss.jopa.model.metamodel.*
import cz.cvut.kbss.jopa.vocabulary.RDF
import java.util.*
import java.util.stream.Collectors

open class JopaEntityStrategy(metamodel: Metamodel) : BaseEntityStrategy<FieldSpecification<*, *>>() {
	protected val metamodel: Metamodel
	protected val entityClassMap: Map<out Class<*>, EntityType<*>>

	init {
		this.metamodel = Objects.requireNonNull(metamodel)
		entityClassMap = metamodel.entities.associateBy { it.bindableJavaType }
	}

	override fun <TEntity : Any> getChangeVectors(
		older: TEntity,
		newer: TEntity,
		requireSameId: Boolean
	): Collection<ChangeVector<*>> {
		val type1 = getObjectType(older)
		val type2 = getObjectType(newer)
		val id1 = getObjectId(older)
		val id2 = getObjectId(newer)

		if (requireSameId && id1 != id2) throw IdNotMatchingException(id1, id2)

		val typeName: String
		val fieldSpecs: Collection<FieldSpecification<*, *>>

		if (type1 == type2) {
			val clazz: Class<*> = older.javaClass
			checkClassSupported(clazz)
			typeName = type1
			fieldSpecs = getObjectAttributes(older)
		} else {
			// get common ancestor
			val clazz = ClassUtil.getCommonSuperclass(older.javaClass, newer.javaClass)
				.orElseThrow { ObjectsNotCompatibleException(older, newer) }
			checkClassSupported(clazz)
			typeName = getTypeName(clazz)
			fieldSpecs = getAttributes(clazz)
		}

		return fieldSpecs.flatMap { attr ->
			if (shouldIgnoreChanges(attr)) return@flatMap emptyList()

			val val1 = getAttributeValue(attr, older)
			val val2 = getAttributeValue(attr, newer)

			if (!consideredEqual(val1, val2)) {
				if (attr is Attribute<*, *>) {
					// if ObjectProperty (association), determine the associated entity's (entities') identifier(s)
					if (attr.isAssociation) {
						val oldId = extractEntityIdentifier(attr, val1)
						return@flatMap listOf(ChangeVector(typeName, id1, getAttributeName(attr)!!, oldId))
					} else {
						return@flatMap listOf(ChangeVector(typeName, id1, getAttributeName(attr)!!, val1))
					}
				} else if (attr is TypesSpecification<*, *>) {
					return@flatMap listOf(ChangeVector(typeName, id1, getAttributeName(attr)!!, val1))
				} else if (attr is PropertiesSpecification<*, *, *, *>) {
					return@flatMap createVectorsForUnmappedProperties(
						typeName,
						id1,
						val1 as Map<*, *>?,
						val2 as Map<*, *>?
					)
				} else {
					// can still be a QueryAttribute or Identifier - either way, we're not dealing with it here
					return@flatMap emptyList()
				}
			} else return@flatMap emptyList()
		}
	}

	protected fun shouldIgnoreChanges(specification: FieldSpecification<*, *>): Boolean {
		return specification.isInferred || specification.javaField.isAnnotationPresent(IgnoreChanges::class.java)
	}

	/**
	 * Returns true if the arguments are either equal or equal in meaning to each other and false otherwise.
	 *
	 *
	 * In case of JOPA entity instances, this method compares them using their identifiers. Other than that, this method
	 * is an extension of [Objects.equals], taking into account the fact that for JOPA's data,
	 * an empty set (generified to an empty collection) has the same functional value as a null.
	 *
	 * @implNote While [.entityClassMap] could be passed as an argument here, it is left as an instance variable.
	 * The only reason to pass the map as an argument (and regenerate it every time [.getChangeVectors] runs) would
	 * be to make sure runtime additions/removals/changes of the metamodel-tracked entities would be taken into account.
	 * These runtime changes are extremely unlikely (borderline impossible) and do not justify the performance penalty.
	 */
	protected fun consideredEqual(a: Any?, b: Any?): Boolean {
		if (a != null && b != null) {
			val aClass: Class<*> = a.javaClass
			val bClass: Class<*> = b.javaClass
			if (entityClassMap.containsKey(aClass) && entityClassMap.containsKey(bClass)) {
				return getAttributeValue(
					entityClassMap[aClass]!!.identifier,
					a
				) == getAttributeValue(entityClassMap[bClass]!!.identifier, b)
			}
		}
		return if (a == b) true else a is Collection<*> && a.isEmpty() && b == null
			|| b is Collection<*> && b.isEmpty() && a == null
	}

	/**
	 * Extract an identifier from an associated entity, or multiple identifiers if the association is x-to-many.
	 *
	 * @param otherEntity Might in fact be any identifier type supported by JOPA (that being
	 * [PersistenceProperties.IDENTIFIER_TYPES]), an actual instance of the target entity class, or a
	 * [Collection] of either of the former.
	 */
	protected fun extractEntityIdentifier(attr: FieldSpecification<*, *>, otherEntity: Any?): Any? {
		if (otherEntity == null) return null
		return if (otherEntity is Collection<*>) {
			// still might contain either of the other two variants
			val collector = if (otherEntity is Set<*>) Collectors.toSet() else Collectors.toList<Any>()
			otherEntity
				.stream()
				.map { instance: Any? -> extractEntityIdentifier(attr, instance) }
				.collect(collector)
		} else if (PersistenceProperties.IDENTIFIER_TYPES.contains(otherEntity.javaClass)) {
			otherEntity
		} else {
			// entity instance
			getAttributeValue(
				metamodel.entity((attr as Bindable<*>).bindableJavaType).identifier,
				otherEntity
			)
		}
	}

	override fun getTypeName(clazz: Class<*>): String {
		return clazz.getAnnotation(OWLClass::class.java).iri
	}

	override fun checkClassSupported(clazz: Class<*>) {
		if (!clazz.isAnnotationPresent(Audited::class.java)
			|| !clazz.isAnnotationPresent(OWLClass::class.java)
		)
			throw ClassNotAuditedException(clazz)
	}

	override fun getAttributes(clazz: Class<*>): Collection<FieldSpecification<*, *>> {
		return metamodel.entity(clazz).fieldSpecifications
	}

	protected fun createVectorsForUnmappedProperties(
		objectType: String,
		objectId: String,
		older: Map<*, *>?,
		newer: Map<*, *>?
	): List<ChangeVector<*>> {
		val olderEmpty = older == null || older.isEmpty()
		val newerEmpty = newer == null || newer.isEmpty()

		// early exits
		if (olderEmpty) {
			return if (newerEmpty) {
				emptyList()
			} else {
				newer!!
					.keys
					.map { k -> ChangeVector(objectType, objectId, k.toString(), null) }
			}
		} else if (newerEmpty) {
			return older!!
				.entries
				.map { (key, value) -> ChangeVector(objectType, objectId, key.toString(), value) }
		}

		val diffsFromOlder = older!!
			.filter { (key, value) -> older[value] != newer!![key] }

		val diffsFromNewer = newer!!
			.filter { (k) -> !diffsFromOlder.containsKey(k) }
			.map { o -> ChangeVector(objectType, objectId, o.toString(), null) }

		return diffsFromOlder.map { (key, value) ->
			ChangeVector(
				objectType,
				objectId,
				key.toString(),
				value
			)
		} + diffsFromNewer
	}

	override fun getAttributeName(field: FieldSpecification<*, *>): String? {
		val jField = field.javaField
		return when (field) {
			is Attribute<*, *> -> field.iri.toString()
			is TypesSpecification<*, *> -> RDF.TYPE
			is PropertiesSpecification<*, *, *, *>, is Identifier<*, *>, is QueryAttribute<*, *> -> null
			else -> {
				// TODO: at least log
				jField.name
			}
		}
	}

	override fun getAttributeValue(field: FieldSpecification<*, *>, instance: Any): Any? {
		Objects.requireNonNull(instance)
		val jField = field.javaField
		if (!jField.canAccess(instance) && !jField.trySetAccessible()) {
			throw AccessDeniedException(instance, jField.name)
		}
		return try {
			jField[instance]
		} catch (e: IllegalAccessException) {
			throw AccessDeniedException(e)
		}
	}

	override fun getObjectId(o: Any): String {
		return try {
			metamodel.entity(o.javaClass).identifier.javaField[o].toString()
		} catch (e: IllegalAccessException) {
			throw AccessDeniedException(e)
		}
	}
}
