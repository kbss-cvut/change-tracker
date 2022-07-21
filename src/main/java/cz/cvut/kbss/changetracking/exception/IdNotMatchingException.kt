package cz.cvut.kbss.changetracking.exception;

/**
 * {@link IdNotMatchingException} is a {@link ChangeTrackingException} thrown when objects differ in their
 * identifiers and the user requested strict ID comparison.
 */
public class IdNotMatchingException extends ChangeTrackingException {
	public IdNotMatchingException(String id1, String id2) {
		super(String.format("ID of objects does not match and required: '%s', '%s'", id1, id2));
	}
}
