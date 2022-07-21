package cz.cvut.kbss.changetracking.exception

/**
 * [IdNotMatchingException] is a [ChangeTrackingException] thrown when objects differ in their
 * identifiers and the user requested strict ID comparison.
 */
class IdNotMatchingException(id1: String, id2: String) :
	ChangeTrackingException("ID of objects does not match and required: '$id1', '$id2'")
