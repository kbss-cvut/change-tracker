package cz.cvut.kbss.changetracking.util;

import cz.cvut.kbss.changetracking.exception.UnsupportedAttributeTypeException;
import cz.cvut.kbss.changetracking.testmodel.Home;
import cz.cvut.kbss.changetracking.testmodel.House;
import cz.cvut.kbss.changetracking.testmodel.UndergraduateStudent;
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
	void getArrayClassForSingular_string_returnsStringArrayClass() {
		assertEquals(String[].class, ClassUtil.getArrayClassFromSingular(String.class));
	}

	@Test
	void getArrayClassForSingular_stringArray_returnsStringArrayArrayClass() {
		assertEquals(String[][].class, ClassUtil.getArrayClassFromSingular(String[].class));
	}

	@Test
	void getArrayClassByName_stringArray_stringArrayClass() {
		assertEquals(String[].class, ClassUtil.getArrayClassByName("java.lang.String[]").get());
	}

	@Test
	void getArrayClassByName_string_emptyOptional() {
		assertTrue(ClassUtil.getArrayClassByName("java.lang.String").isEmpty());
	}

	@Test
	void getArrayClassByName_unknownClassName_throws() {
		assertThrows(UnsupportedAttributeTypeException.class, () -> ClassUtil.getArrayClassByName("xx.xxxxxx.x[]"));
	}
}
