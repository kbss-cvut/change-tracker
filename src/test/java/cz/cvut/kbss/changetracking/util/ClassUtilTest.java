package cz.cvut.kbss.changetracking.util;

import cz.cvut.kbss.changetracking.model.Home;
import cz.cvut.kbss.changetracking.model.House;
import cz.cvut.kbss.changetracking.model.UndergraduateStudent;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

@SuppressWarnings("OptionalGetWithoutIsPresent")
class ClassUtilTest {

	@Test
	void getCommonSuperclass_stringAndInteger_returnsEmptyOptional() {
		assertTrue(ClassUtil.getCommonSuperclass(String.class, Integer.class).isEmpty());
	}

	@Test
	void getCommonSuperclass_stringAndString_returnsString() {
		assertEquals(String.class, ClassUtil.getCommonSuperclass(String.class, String.class).get());
	}

	@Test
	void getCommonSuperclass_houseAndHome_returnsHome() {
		assertEquals(Home.class, ClassUtil.getCommonSuperclass(House.class, Home.class).get());
	}

	@Test
	void getCommonSuperclass_houseAndUndergradStudent_returnsEmptyOptional() {
		assertTrue(ClassUtil.getCommonSuperclass(House.class, UndergraduateStudent.class).isEmpty());
	}

	@Test
	void getArrayClass_string_returnsStringArrayClass() {
		assertEquals(String[].class, ClassUtil.getArrayClass(String.class));
	}

	@Test
	void getArrayClass_stringArray_returnsStringArrayArrayClass() {
		assertEquals(String[][].class, ClassUtil.getArrayClass(String[].class));
	}
}
