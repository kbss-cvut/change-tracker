package cz.cvut.kbss.changetracking.exception;

/**
 * {@link JsonException} is a {@link ChangeTrackingException} thrown when an attribute's value fails to de/serialize
 * from or to JSON.
 */
public class JsonException extends ChangeTrackingException {
	public JsonException(String verbClause, Throwable cause) {
		super(String.format("Failed to %s JSON", verbClause), cause);
	}

	public JsonException(String verbClause) {
		super(String.format("Failed to %s JSON", verbClause));
	}
}
