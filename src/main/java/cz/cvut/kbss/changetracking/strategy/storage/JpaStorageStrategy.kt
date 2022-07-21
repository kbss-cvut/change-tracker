package cz.cvut.kbss.changetracking.strategy.storage

import com.fasterxml.jackson.databind.ObjectMapper
import cz.cvut.kbss.changetracking.model.ChangeVector
import cz.cvut.kbss.changetracking.model.JsonChangeVector
import java.time.Instant
import javax.persistence.EntityManager
import javax.persistence.PersistenceContext

/**
 * [JpaStorageStrategy] is a [StorageStrategy] implementation that uses the Java Persistence API for storing
 * change vectors in a single type with a catch-all column.
 */
open class JpaStorageStrategy @JvmOverloads constructor(
	@PersistenceContext protected val em: EntityManager,
	objectMapper: ObjectMapper? = null
) :
	JsonBasedStorageStrategy(objectMapper) {

	override fun saveVector(vector: JsonChangeVector) {
		em.persist(vector)
	}

	override fun getAllForObject(objectType: String, objectId: String): List<ChangeVector<*>> {
		val cb = em.criteriaBuilder
		val cq = cb.createQuery(JsonChangeVector::class.java)
		val root = cq.from(JsonChangeVector::class.java)
		val predicates = listOf(
			cb.equal(root.get<Any>("objectType"), objectType),
			cb.equal(root.get<Any>("objectId"), objectId)
		)

		cq
			.select(root)
			.where(cb.and(*predicates.toTypedArray()))
			.orderBy(cb.desc(root.get<Any>("timestamp")))
		return convertVectorsFromJson(em.createQuery(cq).resultList)
	}

	override fun getChangesSince(timestamp: Instant): List<ChangeVector<*>> {
		return getChangesOfTypeSince(timestamp, null)
	}

	override fun getChangesOfTypeSince(timestamp: Instant, objectType: String?): List<ChangeVector<*>> {
		val cb = em.criteriaBuilder
		val cq = cb.createQuery(JsonChangeVector::class.java)
		val root = cq.from(JsonChangeVector::class.java)

		val predicates = ArrayList(listOf(cb.greaterThanOrEqualTo(root.get("timestamp"), timestamp)))

		if (objectType != null) {
			predicates.add(cb.equal(root.get<Any>("objectType"), objectType))
		}

		cq
			.select(root)
			.where(cb.and(*predicates.toTypedArray()))
			.orderBy(cb.desc(root.get<Any>("timestamp")))
		return convertVectorsFromJson(em.createQuery(cq).resultList)
	}
}
