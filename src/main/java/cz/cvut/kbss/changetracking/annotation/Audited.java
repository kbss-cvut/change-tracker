package cz.cvut.kbss.changetracking.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * {@code @Audited} is used to enable change tracking for an entity class.
 * <p>
 * {@link cz.cvut.kbss.changetracking.strategy.entity.EntityStrategy#checkClassSupported(Class)} implementations are
 * encouraged to check if the examined class has this annotation.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface Audited {
}

