package cz.cvut.kbss.changetracking.util;

import cz.cvut.kbss.changetracking.model.Home;
import cz.cvut.kbss.changetracking.model.House;
import cz.cvut.kbss.changetracking.model.UndergraduateStudent;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class ClassUtilTest {

	@Test
	void getCommonSuperclass_stringAndInteger_returnsNull() {
		assertNull(ClassUtil.getCommonSuperclass(String.class, Integer.class));
	}

	@Test
	void getCommonSuperclass_stringAndString_returnsString() {
		assertEquals(String.class, ClassUtil.getCommonSuperclass(String.class, String.class));
	}

	@Test
	void getCommonSuperclass_houseAndHome_returnsHome() {
		assertEquals(Home.class, ClassUtil.getCommonSuperclass(House.class, Home.class));
	}

	@Test
	void getCommonSuperclass_houseAndUndergradStudent_returnsNull() {
		assertNull(ClassUtil.getCommonSuperclass(House.class, UndergraduateStudent.class));
	}
}
