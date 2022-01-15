package cz.cvut.kbss.changetracking.model;

import org.jetbrains.annotations.NotNull;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import java.io.Serializable;
import java.time.Instant;

/**
 * Change vector entity class, representing a single change to an audited object at a given timestamp.
 */
@Entity
public class ChangeVector implements Serializable {
	@Id
	@GeneratedValue
	private Long id;

	private Instant timestamp;

	private Object previousValue;

	private String attributeType;

	private String attributeName;

	private String objectType;

	private String objectId;

	public ChangeVector(
		@NotNull String objectType,
		@NotNull String objectId,
		@NotNull String attributeName,
		@NotNull Object previousValue
	) {
		this.attributeType = previousValue.getClass().getCanonicalName();
		this.objectType = objectType;
		this.previousValue = previousValue;
		this.attributeName = attributeName;
		this.objectId = objectId;
		this.timestamp = Instant.now();
	}

	public ChangeVector() {
	}

	public Long getId() {
		return id;
	}

	public Instant getTimestamp() {
		return timestamp;
	}

	public Object getPreviousValue() {
		return previousValue;
	}

	public String getAttributeType() {
		return attributeType;
	}

	public String getAttributeName() {
		return attributeName;
	}

	public String getObjectType() {
		return objectType;
	}

	public String getObjectId() {
		return objectId;
	}
}
