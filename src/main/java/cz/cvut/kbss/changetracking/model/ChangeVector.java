package cz.cvut.kbss.changetracking.model;

import org.jetbrains.annotations.NotNull;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;
import java.io.Serializable;
import java.time.Instant;

/**
 * {@link ChangeVector} is the change vector representation to be used by the client application. Instances of this
 * class already had their {@link ChangeVector#getPreviousValue()} converted from its database representation to the
 * original type (if applicable).
 *
 * @param <T> Type of the {@link ChangeVector#previousValue} field.
 * @apiNote This is NOT a database entity by itself! It however has to bear JPA annotations to allow JPA entity
 * classes to inherit from it.
 */
@MappedSuperclass
public class ChangeVector<T> implements Serializable {

	protected Instant timestamp;

	@Column(columnDefinition = "json")
	protected T previousValue;

	protected String attributeName;

	protected String objectType;

	protected String objectId;

	public ChangeVector(
		@NotNull JsonChangeVector vector,
		T previousValue
	) {
		this(
			vector.getObjectType(),
			vector.getObjectId(),
			vector.getAttributeName(),
			previousValue,
			vector.getTimestamp()
		);
	}

	public ChangeVector(
		@NotNull String objectType,
		@NotNull String objectId,
		@NotNull String attributeName,
		T previousValue
	) {
		this(objectType, objectId, attributeName, previousValue, Instant.now());
	}

	public ChangeVector(
		@NotNull String objectType,
		@NotNull String objectId,
		@NotNull String attributeName,
		T previousValue,
		@NotNull Instant timestamp
	) {
		this.objectType = objectType;
		this.objectId = objectId;
		this.attributeName = attributeName;
		this.previousValue = previousValue;
		this.timestamp = timestamp;
	}

	public ChangeVector() {
	}

	public Instant getTimestamp() {
		return timestamp;
	}

	public T getPreviousValue() {
		return previousValue;
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

	@Override
	public String toString() {
		return "ChangeVector{" +
			"timestamp=" + timestamp +
			", previousValue=" + previousValue +
			", attributeName='" + attributeName + '\'' +
			", objectType='" + objectType + '\'' +
			", objectId='" + objectId + '\'' +
			'}';
	}

	// TODO: hashCode, equals
}
