package cz.cvut.kbss.changetracking.exception

/**
 * [UnsupportedAttributeTypeException] is thrown upon encountering an attribute type the change tracker (more
 * specifically, the [cz.cvut.kbss.changetracking.strategy.storage.StorageStrategy] does not support and can
 * not process.
 */
class UnsupportedAttributeTypeException(typeName: String) :
	ChangeTrackingException("Unsupported attribute type encountered: $typeName")
