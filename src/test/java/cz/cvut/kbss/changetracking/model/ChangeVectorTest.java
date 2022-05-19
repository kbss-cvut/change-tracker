package cz.cvut.kbss.changetracking.model;

import cz.cvut.kbss.changetracking.TestIRIs;
import org.junit.jupiter.api.Test;

import java.time.Instant;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ChangeVectorTest {
	@Test
	void equals_sameEmptyInstance_true() {
		var vector = new ChangeVector<>();
		assertEquals(vector, vector);
	}

	@Test
	void equals_twoInstancesWithSameAttributes_true() {
		var instant = Instant.now();
		var vector1 = new ChangeVector<>(
			TestIRIs.CLASS_CAR,
			TestIRIs.INSTANCE_CAR,
			TestIRIs.PROPERTY_OBJECT_HAS_OWNER,
			TestIRIs.INSTANCE_MOTHER,
			instant
		);
		var vector2 = new ChangeVector<>(
			TestIRIs.CLASS_CAR,
			TestIRIs.INSTANCE_CAR,
			TestIRIs.PROPERTY_OBJECT_HAS_OWNER,
			TestIRIs.INSTANCE_MOTHER,
			instant
		);
		assertEquals(vector1, vector2);
	}
}
