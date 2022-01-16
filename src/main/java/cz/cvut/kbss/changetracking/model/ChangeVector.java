package cz.cvut.kbss.changetracking.model;

import com.fasterxml.jackson.core.JsonProcessingException;
import org.jetbrains.annotations.NotNull;

/**
 * {@link ChangeVector} is the change vector class to be used in the client application. Instances of this class already
 * had their {@link AbstractChangeVector#getPreviousValue()} converted from its JSON representation to the original
 * type, as specified by {@link JsonChangeVector#getAttributeType()}.
 *
 * @apiNote This is NOT a database entity! See {@link JsonChangeVector} instead.
 */
public class ChangeVector extends AbstractChangeVector<Object> {
	public ChangeVector(
		@NotNull JsonChangeVector vector,
		Object previousValue
	) throws JsonProcessingException {
		super(
			vector.getObjectType(),
			vector.getObjectId(),
			vector.getAttributeName(),
			previousValue
		);
	}
}
