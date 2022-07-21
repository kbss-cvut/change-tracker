package cz.cvut.kbss.changetracking.model

import cz.cvut.kbss.jopa.model.annotations.OWLClass
import java.io.Serializable
import java.time.Instant
import java.util.*
import javax.persistence.Column
import javax.persistence.MappedSuperclass

/**
 * [ChangeVector] is the change vector representation to be used by the client application. Instances of this
 * class already had their [ChangeVector.previousValue] converted from its database representation to the
 * original type (if applicable).
 *
 * @param <T> Type of the [ChangeVector.previousValue] field.
 * @apiNote This is NOT a database entity by itself! It however has to bear JPA annotations to allow JPA entity
 * classes to inherit from it.
</T> */
@MappedSuperclass
@OWLClass(iri = "http://onto.fel.cvut.cz/ontologies/slovn\u00edk/agendov\u00fd/popis-dat/pojem/zm\u011bna")
open class ChangeVector<T> : Serializable {

	@Column(columnDefinition = "text")
	var previousValue: T? = null
	lateinit var attributeName: String
	lateinit var objectType: String
	lateinit var objectId: String
	var authorId: String? = null
	lateinit var timestamp: Instant

	constructor(
		vector: JsonChangeVector,
		previousValue: T
	) : this(
		vector.objectType,
		vector.objectId,
		vector.attributeName,
		previousValue,
		vector.timestamp
	) {
		authorId = vector.authorId
	}

	@JvmOverloads
	constructor(
		objectType: String,
		objectId: String,
		attributeName: String,
		previousValue: T?,
		timestamp: Instant = Instant.now()
	) {
		this.objectType = objectType
		this.objectId = objectId
		this.attributeName = attributeName
		this.previousValue = previousValue
		this.timestamp = timestamp
	}

	constructor()

	override fun toString(): String {
		return "ChangeVector{" +
			"timestamp=" + timestamp +
			", previousValue=" + previousValue +
			", attributeName='" + attributeName + '\'' +
			", objectType='" + objectType + '\'' +
			", objectId='" + objectId + '\'' +
			", authorId='" + authorId + '\'' +
			'}'
	}

	override fun equals(other: Any?): Boolean {
		if (this === other) return true
		if (other !is ChangeVector<*>) return false
		return (timestamp == other.timestamp && previousValue == other.previousValue
			&& attributeName == other.attributeName && objectType == other.objectType && objectId == other.objectId && authorId == other.authorId)
	}

	override fun hashCode(): Int {
		return Objects.hash(timestamp, previousValue, attributeName, objectType, objectId, authorId)
	}
}
