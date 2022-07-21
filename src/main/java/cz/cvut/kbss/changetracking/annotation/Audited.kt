package cz.cvut.kbss.changetracking.annotation

/**
 * `@Audited` is used to enable change tracking for an entity class.
 *
 * [cz.cvut.kbss.changetracking.strategy.entity.BaseEntityStrategy.checkClassSupported] implementations are
 * encouraged to check if the examined class has this annotation.
 */
@Retention(AnnotationRetention.RUNTIME)
@Target(AnnotationTarget.ANNOTATION_CLASS, AnnotationTarget.CLASS)
annotation class Audited
