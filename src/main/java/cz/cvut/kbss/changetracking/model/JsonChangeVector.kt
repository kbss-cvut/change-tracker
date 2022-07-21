package cz.cvut.kbss.changetracking.model

import javax.persistence.*

/**
 * [JsonChangeVector] is the change vector entity class, representing a single change to an audited object. The
 * previous value of the changed attribute is encoded as JSON for easy storage in both relational databases and MongoDB,
 * for example.
 *
 * @apiNote This class should not be directly used to read previous values! Use [ChangeVector] instead.
 */
@Entity
@Table(name = "change_vector")
open class JsonChangeVector : ChangeVector<String> {
	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	var id: Long? = null
		private set

	var attributeType: String? = null

	constructor(
		vector: ChangeVector<*>,
		attributeClass: Class<*>,
		previousValueJson: String
	) : super(
		vector.objectType,
		vector.objectId,
		vector.attributeName,
		previousValueJson,
		vector.timestamp,
	) {
		authorId = vector.authorId
		attributeType = attributeClass.name
	}

	constructor()

	override fun toString(): String {
		return "JsonChangeVector{" +
			"timestamp=" + timestamp +
			", previousValue=" + previousValue +
			", attributeName='" + attributeName + '\'' +
			", objectType='" + objectType + '\'' +
			", objectId='" + objectId + '\'' +
			", id=" + id +
			", attributeType='" + attributeType + '\'' +
			'}'
	}
}
