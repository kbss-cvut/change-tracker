package cz.cvut.kbss.changetracking.annotation

/**
 * Marks fields whose changes in an [Audited] instance should be ignored.
 *
 *
 * This allows to exclude certain attributes (e.g., provenance data) from change tracking.
 */
@Retention(AnnotationRetention.RUNTIME)
@Target(AnnotationTarget.FIELD)
annotation class IgnoreChanges
