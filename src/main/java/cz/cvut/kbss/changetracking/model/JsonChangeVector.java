package cz.cvut.kbss.changetracking.model;

import org.jetbrains.annotations.NotNull;

import javax.persistence.*;

/**
 * {@link JsonChangeVector} is the change vector entity class, representing a single change to an audited object. The
 * previous value of the changed attribute is encoded as JSON for easy storage in both relational databases and MongoDB,
 * for example.
 *
 * @apiNote This class should not be directly used to read previous values! Use {@link ChangeVector} instead.
 */
@Entity
@Table(name = "change_vector")
public class JsonChangeVector extends ChangeVector<String> {
	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	protected Long id;

	protected String attributeType;

	public JsonChangeVector(
		@NotNull ChangeVector<?> vector,
		@NotNull Class<?> attributeClass,
		@NotNull String previousValueJson
	) {
		super(
			vector.getObjectType(),
			vector.getObjectId(),
			vector.getAttributeName(),
			previousValueJson,
			vector.getTimestamp()
		);
		this.attributeType = attributeClass.getCanonicalName();
	}

	public JsonChangeVector() {
	}

	public Long getId() {
		return id;
	}

	public String getAttributeType() {
		return attributeType;
	}

	@Override
	public String toString() {
		return "JsonChangeVector{" +
			"timestamp=" + timestamp +
			", previousValue=" + previousValue +
			", attributeName='" + attributeName + '\'' +
			", objectType='" + objectType + '\'' +
			", objectId='" + objectId + '\'' +
			", id=" + id +
			", attributeType='" + attributeType + '\'' +
			'}';
	}
}
