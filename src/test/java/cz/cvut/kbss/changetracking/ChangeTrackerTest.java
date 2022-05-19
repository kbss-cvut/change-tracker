package cz.cvut.kbss.changetracking;

import cz.cvut.kbss.changetracking.model.ChangeVector;
import cz.cvut.kbss.changetracking.testmodel.Home;
import cz.cvut.kbss.changetracking.strategy.entity.EntityStrategy;
import cz.cvut.kbss.changetracking.strategy.storage.StorageStrategy;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

public class ChangeTrackerTest {
	static EntityStrategy entityStrategyMock = mock(EntityStrategy.class);
	static StorageStrategy storageStrategyMock = mock(StorageStrategy.class);
	static ChangeTracker changeTracker = new ChangeTracker(entityStrategyMock, storageStrategyMock);

	@BeforeAll
	static void setup() {
		var vector = new ChangeVector<>(TestIRIs.CLASS_HOME, TestIRIs.INSTANCE_HOME, TestIRIs.PROPERTY_CITY, "Sydney");
		when(entityStrategyMock.getChangeVectors(any(), any(), anyBoolean())).thenReturn(Collections.singletonList(vector));
	}

	@Test
	void compare_oneVector_setsAuthorId() {
		var home1 = new Home(TestIRIs.INSTANCE_HOME, "Sydney");
		var home2 = new Home(TestIRIs.INSTANCE_HOME, "Los Angeles");
		var authorId = "JamesFord";
		var vectors = changeTracker.compare(home1, home2, authorId).toArray(ChangeVector<?>[]::new);
		assertEquals(1, vectors.length);
		assertEquals(authorId, vectors[0].getAuthorId());
	}
}
