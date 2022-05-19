package cz.cvut.kbss.changetracking.model;

import cz.cvut.kbss.changetracking.TestIRIs;
import org.junit.jupiter.api.Test;

import java.time.Instant;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class JsonChangeVectorTest {
	@Test
	void constructor_copyFromRawWithValuesWithoutAuthor_resultEqualsOrigin() {
		var origin = new ChangeVector<>(
			TestIRIs.CLASS_CAR,
			TestIRIs.INSTANCE_CAR,
			TestIRIs.PROPERTY_OBJECT_HAS_OWNER,
			TestIRIs.INSTANCE_MOTHER,
			Instant.now()
		);
		var result = new JsonChangeVector(origin, String.class, origin.getPreviousValue());

		assertEquals(origin, result);
	}

	@Test
	void constructor_copyFromRawWithValuesWithAuthor_resultEqualsOrigin() {
		var origin = new ChangeVector<>(
			TestIRIs.CLASS_CAR,
			TestIRIs.INSTANCE_CAR,
			TestIRIs.PROPERTY_OBJECT_HAS_OWNER,
			TestIRIs.INSTANCE_MOTHER,
			Instant.now()
		);
		origin.setAuthorId("author name");
		var result = new JsonChangeVector(origin, String.class, origin.getPreviousValue());

		assertEquals(origin, result);
	}
}
