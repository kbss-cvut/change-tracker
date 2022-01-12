package cz.cvut.kbss.changetracking.strategy.entity;

import cz.cvut.kbss.changetracking.model.ChangeVector;

import java.util.Collection;
import java.util.Objects;
import java.util.stream.Collectors;

public abstract class AbstractEntityStrategy<TEntity, TField> implements EntityStrategy<TEntity> {
  protected abstract Collection<TField> getAttributes(TEntity entity);

  protected abstract String getAttributeName(TField field);

  protected abstract Object getAttributeValue(TField field, TEntity instance);


  @Override
  public final Collection<ChangeVector> getChangeVectors(TEntity older, TEntity newer) {
    var type = getObjectType(older);
    var id1 = getObjectId(older);
    // TODO: id2?

    return getAttributes(older)
      .stream()
      .map(attr -> {
        var val1 = getAttributeValue(attr, older);
        var val2 = getAttributeValue(attr, newer);
        if (!Objects.equals(val1, val2)) {
          return new ChangeVector(type, id1, getAttributeName(attr), val1);
        } else return null;
      })
      .filter(Objects::nonNull)
      .collect(Collectors.toList());
  }
}
