package cz.cvut.kbss.changetracking.model;

import org.jetbrains.annotations.NotNull;

/**
 * {@link ChangeVector} is the change vector class to be used in the client application. Instances of this class already
 * had their {@link AbstractChangeVector#getPreviousValue()} converted from its database representation to the original
 * type (if applicable).
 *
 * @apiNote This is NOT a database entity by itself!
 */
public class ChangeVector extends AbstractChangeVector<Object> {
	public ChangeVector(
		@NotNull JsonChangeVector vector,
		Object previousValue
	) {
		super(
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
		Object previousValue
	) {
		super(objectType, objectId, attributeName, previousValue);
	}
}
