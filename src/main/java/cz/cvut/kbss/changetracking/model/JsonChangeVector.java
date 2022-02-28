package cz.cvut.kbss.changetracking.model;

import org.jetbrains.annotations.NotNull;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * {@link JsonChangeVector} is the change vector entity class, representing a single change to an audited object. The
 * previous value of the changed attribute is encoded as JSON for easy storage in both relational databases and MongoDB,
 * for example.
 *
 * @apiNote This class should not be directly used to read previous values! Use {@link ChangeVector} instead.
 */
@Entity
@Table(name = "change_vector")
public class JsonChangeVector extends AbstractChangeVector<String> {
	private String attributeType;

	public JsonChangeVector(
		@NotNull ChangeVector vector,
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

	public String getAttributeType() {
		return attributeType;
	}
}
