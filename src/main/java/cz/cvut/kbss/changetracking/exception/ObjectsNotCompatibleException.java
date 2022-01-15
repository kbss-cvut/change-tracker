package cz.cvut.kbss.changetracking.exception;

/**
 * {@link ObjectsNotCompatibleException} is a {@link ChangeTrackingException} describing the situation when two objects
 * of incompatible classes are attempted to be compared.
 */
public class ObjectsNotCompatibleException extends ChangeTrackingException {
	public ObjectsNotCompatibleException(Object o1, Object o2) {
		super("Objects are not compatible with each other: " + o1.toString() + " and " + o2.toString());
	}
}
