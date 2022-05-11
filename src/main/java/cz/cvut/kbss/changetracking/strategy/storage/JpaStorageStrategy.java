package cz.cvut.kbss.changetracking.strategy.storage;

import cz.cvut.kbss.changetracking.model.ChangeVector;
import cz.cvut.kbss.changetracking.model.JsonChangeVector;
import org.jetbrains.annotations.Nullable;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.Predicate;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * {@link JpaStorageStrategy} is a {@link StorageStrategy} implementation that uses the Java Persistence API for storing
 * change vectors in a single type with a catch-all column.
 */
public class JpaStorageStrategy extends JsonBasedStorageStrategy {

	@PersistenceContext
	private final EntityManager em;

	public JpaStorageStrategy(EntityManager em) {
		this.em = em;
	}

	@Override
	@Transactional
	public void save(ChangeVector<?>... rawVectors) {
		Arrays
			.stream(rawVectors)
			.map(vector -> {
				var value = vector.getPreviousValue();
				var valueClass = value == null ? Object.class : value.getClass();

				return new JsonChangeVector(
					vector,
					valueClass,
					convertValueToJson(value)
				);
			})
			.forEach(em::persist);
	}

	@Override
	public List<ChangeVector<?>> getAllForObject(String objectType, String objectId) {
		var cb = em.getCriteriaBuilder();
		var cq = cb.createQuery(JsonChangeVector.class);
		var root = cq.from(JsonChangeVector.class);
		var predicates = List.of(
			cb.equal(root.get("objectType"), objectType),
			cb.equal(root.get("objectId"), objectId)
		);

		cq
			.select(root)
			.where(cb.and(predicates.toArray(Predicate[]::new)))
			.orderBy(cb.desc(root.get("timestamp")));
		return convertVectorsFromJson(em.createQuery(cq).getResultList());
	}

	@Override
	public List<ChangeVector<?>> getChangesSince(Instant timestamp) {
		return getChangesOfTypeSince(timestamp, null);
	}

	@Override
	public List<ChangeVector<?>> getChangesOfTypeSince(Instant timestamp, @Nullable String objectType) {
		var cb = em.getCriteriaBuilder();
		var cq = cb.createQuery(JsonChangeVector.class);
		var root = cq.from(JsonChangeVector.class);
		var predicates = new ArrayList<>(List.of(cb.greaterThanOrEqualTo(root.get("timestamp"), timestamp)));
		if (objectType != null) {
			predicates.add(cb.equal(root.get("objectType"), objectType));
		}

		cq
			.select(root)
			.where(cb.and(predicates.toArray(Predicate[]::new)))
			.orderBy(cb.desc(root.get("timestamp")));
		return convertVectorsFromJson(em.createQuery(cq).getResultList());
	}
}
