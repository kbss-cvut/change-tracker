package cz.cvut.kbss.changetracking.exception

/**
 * [ChangeTrackingException] is the change tracker's specific [RuntimeException].
 *
 * All exceptions thrown by the change tracker should inherit from this class.
 */
abstract class ChangeTrackingException : RuntimeException {
	constructor(s: String, cause: Throwable) : super(s, cause)
	constructor(s: String) : super(s)
}
