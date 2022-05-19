package cz.cvut.kbss.changetracking.model;

import org.jetbrains.annotations.NotNull;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.MappedSuperclass;
import java.io.Serializable;
import java.time.Instant;
import java.util.Objects;

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

	@Column(columnDefinition = "text")
	protected T previousValue;

	protected String attributeName;

	protected String objectType;

	protected String objectId;

	protected String authorId;

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
		this.authorId = vector.authorId;
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

	public String getAuthorId() {
		return authorId;
	}

	public void setAuthorId(String authorId) {
		this.authorId = authorId;
	}

	@Override
	public String toString() {
		return "ChangeVector{" +
			"timestamp=" + timestamp +
			", previousValue=" + previousValue +
			", attributeName='" + attributeName + '\'' +
			", objectType='" + objectType + '\'' +
			", objectId='" + objectId + '\'' +
			", authorId='" + authorId + '\'' +
			'}';
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof ChangeVector)) return false;
		ChangeVector<?> that = (ChangeVector<?>) o;
		return timestamp.equals(that.timestamp)
			&& Objects.equals(previousValue, that.previousValue)
			&& attributeName.equals(that.attributeName)
			&& objectType.equals(that.objectType)
			&& objectId.equals(that.objectId)
			&& Objects.equals(authorId, that.authorId);
	}

	@Override
	public int hashCode() {
		return Objects.hash(timestamp, previousValue, attributeName, objectType, objectId, authorId);
	}
}
