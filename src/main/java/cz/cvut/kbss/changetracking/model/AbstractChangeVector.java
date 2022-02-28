package cz.cvut.kbss.changetracking.model;

import org.jetbrains.annotations.NotNull;

import javax.persistence.*;
import java.io.Serializable;
import java.time.Instant;

/**
 * {@link AbstractChangeVector} is an abstraction of change vectors with a type parameter. It features attributes shared
 * between both {@link JsonChangeVector} and {@link ChangeVector}.
 *
 * @param <T> Type of the {@link AbstractChangeVector#previousValue} field.
 * @apiNote This class should probably not be used outside the change tracking module.
 */
@MappedSuperclass
public abstract class AbstractChangeVector<T> implements Serializable {
	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	protected Long id;

	protected Instant timestamp;

	@Column(columnDefinition = "json")
	protected T previousValue;

	protected String attributeName;

	protected String objectType;

	protected String objectId;

	public AbstractChangeVector(
		@NotNull String objectType,
		@NotNull String objectId,
		@NotNull String attributeName,
		T previousValue
	) {
		this.objectType = objectType;
		this.objectId = objectId;
		this.attributeName = attributeName;
		this.previousValue = previousValue;
		this.timestamp = Instant.now();
	}

	public AbstractChangeVector() {
	}

	public Long getId() {
		return id;
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
		return "AbstractChangeVector{" +
			"id=" + id +
			", timestamp=" + timestamp +
			", previousValue=" + previousValue +
			", attributeName='" + attributeName + '\'' +
			", objectType='" + objectType + '\'' +
			", objectId='" + objectId + '\'' +
			'}';
	}

	// TODO: hashCode, equals
}
