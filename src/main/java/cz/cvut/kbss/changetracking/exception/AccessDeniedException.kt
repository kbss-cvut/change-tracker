package cz.cvut.kbss.changetracking.exception

/**
 * [AccessDeniedException] is a [ChangeTrackingException] used to indicate that the change tracker was
 * unable to access an object or one of its attributes/fields.
 */
class AccessDeniedException : ChangeTrackingException {
	constructor(cause: Throwable) : super("Failed to access object or field", cause)
	constructor(o: Any, fieldName: String) : super("Failed to access field $fieldName on object: $o")
}
