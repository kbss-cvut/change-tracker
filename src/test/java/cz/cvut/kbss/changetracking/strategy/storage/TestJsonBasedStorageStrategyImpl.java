package cz.cvut.kbss.changetracking.strategy.storage;

import cz.cvut.kbss.changetracking.model.ChangeVector;
import cz.cvut.kbss.changetracking.model.JsonChangeVector;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

/**
 * Empty implementation of {@link JsonBasedStorageStrategy} for testing purposes.
 */
public class TestJsonBasedStorageStrategyImpl extends JsonBasedStorageStrategy {
	public TestJsonBasedStorageStrategyImpl() {
		super(null);
	}

	@Override
	protected void saveVector(JsonChangeVector vector) {
	}

	@Override
	public List<ChangeVector<?>> getAllForObject(String objectType, String objectId) {
		return new ArrayList<>();
	}

	@Override
	public List<ChangeVector<?>> getChangesSince(Instant timestamp) {
		return new ArrayList<>();
	}

	@Override
	public List<ChangeVector<?>> getChangesOfTypeSince(Instant timestamp, String objectType) {
		return new ArrayList<>();
	}
}
