package cz.cvut.kbss.changetracking;

import com.fasterxml.jackson.core.JsonProcessingException;
import cz.cvut.kbss.changetracking.exception.ObjectsNotCompatibleException;
import cz.cvut.kbss.changetracking.model.ChangeVector;
import cz.cvut.kbss.changetracking.model.JsonChangeVector;
import cz.cvut.kbss.changetracking.strategy.entity.EntityStrategy;
import cz.cvut.kbss.changetracking.strategy.storage.StorageStrategy;

import java.time.Instant;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class ChangeTracker {

	@SuppressWarnings("rawtypes")
	private EntityStrategy entityStrategy;
	private StorageStrategy storageStrategy;

	@SuppressWarnings("rawtypes")
	public ChangeTracker(EntityStrategy entityStrategy, StorageStrategy storageStrategy) {
		this.entityStrategy = entityStrategy;
		this.storageStrategy = storageStrategy;
	}

	@SuppressWarnings("rawtypes")
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
	@SuppressWarnings("unchecked")
	public Collection<JsonChangeVector> compare(Object older, Object newer) {
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
		storageStrategy.save(vectors.toArray(JsonChangeVector[]::new));
	}

	/**
	 * Convert vectors from {@link JsonChangeVector} to {@link ChangeVector}, i.e. convert their previousValues from JSON
	 * back to their original types.
	 *
	 * @param vectors The vectors to convert.
	 * @return Vectors with proper values.
	 * @throws cz.cvut.kbss.changetracking.exception.JsonException Based on {@link EntityStrategy#convertValueFromJson}.
	 */
	protected List<ChangeVector> convertVectorValuesFromJson(List<JsonChangeVector> vectors) {
		return vectors.stream().map(vector -> {
			try {
				return new ChangeVector(
					vector,
					entityStrategy.convertValueFromJson(vector.getAttributeType(), vector.getPreviousValue())
				);
			} catch (JsonProcessingException e) {
				e.printStackTrace();
				return null;
			}
		}).collect(Collectors.toList());
	}

	/**
	 * Get all change vectors for an object.
	 * <p>
	 * This method calls {@link StorageStrategy#getAllForObject(String, String)} and then converts the attributes' values
	 * from JSON to the original types.
	 *
	 * @throws cz.cvut.kbss.changetracking.exception.JsonException Based on {@link EntityStrategy#convertValueFromJson}.
	 */
	public List<ChangeVector> getAllForObject(String objectType, String objectId) {
		final var vectors = storageStrategy.getAllForObject(objectType, objectId);
		return convertVectorValuesFromJson(vectors);
	}

	/**
	 * Get all change vectors since a timestamp (inclusive).
	 * <p>
	 * This method calls {@link StorageStrategy#getChangesSince(Instant)} and converts the attributes' values to the
	 * original types.
	 */
	public List<ChangeVector> getChangesSince(Instant timestamp) {
		final var vectors = storageStrategy.getChangesSince(timestamp);
		return convertVectorValuesFromJson(vectors);
	}

	/**
	 * Get all change vectors of an object type since a timestamp (inclusive).
	 * <p>
	 * This method calls {@link StorageStrategy#getChangesOfTypeSince(Instant, String)} and converts the attributes'
	 * values to the original types.
	 */
	public List<ChangeVector> getChangesOfTypeSince(Instant timestamp, String objectType) {
		final var vectors = storageStrategy.getChangesOfTypeSince(timestamp, objectType);
		return convertVectorValuesFromJson(vectors);
	}
}
