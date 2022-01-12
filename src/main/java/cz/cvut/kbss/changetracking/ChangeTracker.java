package cz.cvut.kbss.changetracking;

import cz.cvut.kbss.changetracking.exception.ObjectsNotCompatibleException;
import cz.cvut.kbss.changetracking.model.ChangeVector;
import cz.cvut.kbss.changetracking.strategy.entity.EntityStrategy;
import cz.cvut.kbss.changetracking.strategy.storage.StorageStrategy;

import java.util.Collection;
import java.util.Objects;

// TODO: deal with EntityStrategy generics
public class ChangeTracker {

  private EntityStrategy entityStrategy;
  private StorageStrategy storageStrategy;

  public ChangeTracker(EntityStrategy entityStrategy, StorageStrategy storageStrategy) {
    this.entityStrategy = entityStrategy;
    this.storageStrategy = storageStrategy;
  }

  public void setClassStrategy(EntityStrategy entityStrategy) {
    this.entityStrategy = entityStrategy;
  }

  public void setStorageStrategy(StorageStrategy storageStrategy) {
    this.storageStrategy = storageStrategy;
  }

  /**
   * Compare two revisions of an object.
   *
   * @param older The older revision of the object.
   * @param newer The newer revision of the object.
   * @return A collection of change vectors between the two revisions.
   * @throws ObjectsNotCompatibleException                                  If the objects are not mutually compatible.
   * @throws cz.cvut.kbss.changetracking.exception.ClassNotAuditedException If at least one of the objects' classes is
   *                                                                        supported by the current {@link
   *                                                                        EntityStrategy}.
   */
  public Collection<ChangeVector> compare(Object older, Object newer) {
    Objects.requireNonNull(older);
    Objects.requireNonNull(newer);

    var aClass = newer.getClass();
    var bClass = older.getClass();
    if (aClass != bClass)
      // TODO: inheritance
      throw new ObjectsNotCompatibleException(older, newer);

    entityStrategy.checkClassSupported(aClass);

    return entityStrategy.getChangeVectors(older, newer);
  }

  /**
   * Compare two revisions of an object and save the resulting change vectors.
   *
   * @param older The older revision of the object.
   * @param newer The newer revision of the object.
   * @param <T>   The object's type.
   */
  public <T> void compareAndSave(T older, T newer) {
    var vectors = compare(older, newer);
    storageStrategy.save(vectors.toArray(ChangeVector[]::new));
  }
}
