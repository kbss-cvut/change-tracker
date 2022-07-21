package cz.cvut.kbss.changetracking.exception

/**
 * [ClassNotAuditedException] is used when trying to run [cz.cvut.kbss.changetracking.ChangeTracker.compare]
 * on an object whose class is not supported for change tracking. With the default
 * [cz.cvut.kbss.changetracking.strategy.entity.EntityStrategy], this means the class is missing the
 * [cz.cvut.kbss.changetracking.annotation.Audited] annotation.
 */
class ClassNotAuditedException(clazz: Class<*>) :
	ChangeTrackingException("Object class doesn't support auditing: " + clazz.name)
