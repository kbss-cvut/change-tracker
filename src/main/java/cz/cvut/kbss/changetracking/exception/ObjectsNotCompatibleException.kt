package cz.cvut.kbss.changetracking.exception

/**
 * [ObjectsNotCompatibleException] is a [ChangeTrackingException] describing the situation when two objects
 * of incompatible classes are attempted to be compared.
 */
class ObjectsNotCompatibleException(o1: Any, o2: Any) :
	ChangeTrackingException("Objects are not compatible with each other: $o1 and $o2")
