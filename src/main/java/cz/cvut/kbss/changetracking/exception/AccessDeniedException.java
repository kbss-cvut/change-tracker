package cz.cvut.kbss.changetracking.exception;

/**
 * {@link AccessDeniedException} is a {@link ChangeTrackingException} used to indicate that the change tracker was
 * unable to access an object or one of its attributes/fields.
 */
public class AccessDeniedException extends ChangeTrackingException {
	public AccessDeniedException(Throwable cause) {
		super("Failed to access object or field", cause);
	}

	public AccessDeniedException(Object o, String fieldName) {
		super("Failed to access field " + fieldName + " on object: " + o.toString());
	}
}
