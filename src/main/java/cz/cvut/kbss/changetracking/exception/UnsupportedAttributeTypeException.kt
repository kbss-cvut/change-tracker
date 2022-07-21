package cz.cvut.kbss.changetracking.exception;

/**
 * {@link UnsupportedAttributeTypeException} is thrown upon encountering an attribute type the change tracker (more
 * specifically, the {@link cz.cvut.kbss.changetracking.strategy.storage.StorageStrategy} does not support and can
 * not process.
 */
public class UnsupportedAttributeTypeException extends ChangeTrackingException {
	public UnsupportedAttributeTypeException(String typeName) {
		super(String.format("Unsupported attribute type encountered: %s", typeName));
	}
}
