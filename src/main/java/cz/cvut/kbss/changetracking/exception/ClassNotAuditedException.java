package cz.cvut.kbss.changetracking.exception;

/**
 * {@link ClassNotAuditedException} is used when trying to run {@link cz.cvut.kbss.changetracking.ChangeTracker#compare}
 * on an object whose class is not supported for change tracking. With the default {@link
 * cz.cvut.kbss.changetracking.strategy.entity.EntityStrategy}, this means the class is missing the {@link
 * cz.cvut.kbss.changetracking.annotation.Audited} annotation).
 */
public class ClassNotAuditedException extends ChangeTrackingException {
	public ClassNotAuditedException(Class<?> clazz) {
		super("Object class doesn't support auditing: " + clazz.getCanonicalName());
	}
}
