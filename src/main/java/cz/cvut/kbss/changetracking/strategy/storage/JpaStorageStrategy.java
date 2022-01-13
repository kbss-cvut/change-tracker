package cz.cvut.kbss.changetracking.strategy.storage;

import cz.cvut.kbss.changetracking.model.ChangeVector;
import org.jetbrains.annotations.Nullable;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.Predicate;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

public class JpaStorageStrategy implements StorageStrategy {
  @PersistenceContext
  private final EntityManager em;

  public JpaStorageStrategy(EntityManager em) {
    this.em = em;
  }

  @Override
  public void save(ChangeVector... vectors) {
    for (var vec : vectors) {
      em.persist(vec);
    }
  }

  @Override
  public List<ChangeVector> getAllForObject(String objectType, String objectId) {
    var cb = em.getCriteriaBuilder();
    var cq = cb.createQuery(ChangeVector.class);
    var root = cq.from(ChangeVector.class);
    var predicates = List.of(
      cb.equal(root.get("objectType"), objectType),
      cb.equal(root.get("objectId"), objectId)
    );

    cq
      .select(root)
      .where(cb.and(predicates.toArray(Predicate[]::new)))
      .orderBy(cb.desc(root.get("timestamp")));
    return em.createQuery(cq).getResultList();
  }

  @Override
  public List<ChangeVector> getChangesSince(Instant timestamp) {
    return getChangesOfTypeSince(timestamp, null);
  }

  @Override
  public List<ChangeVector> getChangesOfTypeSince(Instant timestamp, @Nullable String objectType) {
    var cb = em.getCriteriaBuilder();
    var cq = cb.createQuery(ChangeVector.class);
    var root = cq.from(ChangeVector.class);
    var predicates = new ArrayList<>(List.of(cb.ge(root.get("timestamp").as(Long.class), timestamp.toEpochMilli())));
    if (objectType != null) {
      predicates.add(cb.equal(root.get("objectType"), objectType));
    }

    cq
      .select(root)
      .where(cb.and(predicates.toArray(Predicate[]::new)))
      .orderBy(cb.desc(root.get("timestamp")));
    return em.createQuery(cq).getResultList();
  }
}
