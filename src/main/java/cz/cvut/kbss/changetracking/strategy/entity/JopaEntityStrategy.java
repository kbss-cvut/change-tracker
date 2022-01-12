package cz.cvut.kbss.changetracking.strategy.entity;

import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.changetracking.exception.AccessDeniedException;
import cz.cvut.kbss.changetracking.exception.ClassNotAuditedException;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;

import java.util.Collection;
import java.util.Objects;

public class JopaEntityStrategy extends AbstractEntityStrategy<Object, Attribute<?, ?>> {
  private final Metamodel metamodel;

  public JopaEntityStrategy(Metamodel metamodel) {
    this.metamodel = metamodel;
  }

  @Override
  public void checkClassSupported(Class<?> clazz) {
    if (!clazz.isAnnotationPresent(Audited.class) || !clazz.isAnnotationPresent(OWLClass.class))
      throw new ClassNotAuditedException(clazz);
  }

  // FIXME: java: incompatible types: java.util.Set<cz.cvut.kbss.jopa.model.metamodel.Attribute<? super
  //  capture#1 of ? extends java.lang.Object,?>> cannot be converted to java.util.Set<cz.cvut.kbss.jopa.model
  //  .metamodel.Attribute<?,?>>
  //  (prevented by casting for now but may indicate a deeper-seated generics issue)
  @Override
  protected Collection<Attribute<?, ?>> getAttributes(Object entity) {
    return (Collection<Attribute<?, ?>>) metamodel.entity(entity.getClass()).getAttributes();
  }

  @Override
  protected String getAttributeName(Attribute<?, ?> field) {
    return field.getIRI().toString();
  }

  @Override
  protected Object getAttributeValue(Attribute<?, ?> field, Object instance) {
    Objects.requireNonNull(field);
    Objects.requireNonNull(instance);
    var jField = field.getJavaField();
    if (!jField.canAccess(instance) && !jField.trySetAccessible()) {
      throw new AccessDeniedException(instance, jField.getName());
    }

    try {
      return jField.get(instance);
    } catch (IllegalAccessException e) {
      throw new AccessDeniedException(e);
    }
  }

  @Override
  public String getObjectType(Object o) {
    return o.getClass().getAnnotation(OWLClass.class).iri();
  }

  @Override
  public String getObjectId(Object o) {
    try {
      return metamodel.entity(o.getClass()).getIdentifier().getJavaField().get(o).toString();
    } catch (IllegalAccessException e) {
      throw new AccessDeniedException(e);
    }
  }
}
