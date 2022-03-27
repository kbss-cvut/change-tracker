package cz.cvut.kbss.changetracking.strategy.storage;

import cz.cvut.kbss.changetracking.exception.UnsupportedAttributeTypeException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

class JpaStorageStrategyTest {
	// TODO: implement tests using Spring Boot
	JpaStorageStrategy strategy = new JpaStorageStrategy(null);

	static Stream<Arguments> providePrimitiveArgumentsForJsonConversion() {
		return Stream.of(
			Arguments.of(String.class, "\"\"", ""),
			Arguments.of(String.class, "\"hello\"", "hello"),
			Arguments.of(Character.class, "\"c\"", 'c'),
			Arguments.of(char.class, "\"x\"", 'x'),
			Arguments.of(Double.class, "66.667", 66.667d),
			Arguments.of(double.class, "10.03", 10.03d),
			Arguments.of(Float.class, "9999999.0", 9999999f),
			Arguments.of(float.class, "-13.9",  - 13.9f),
			Arguments.of(Integer.class, "100", 100),
			Arguments.of(int.class, "10", 10),
			Arguments.of(Boolean.class, "true", true),
			Arguments.of(boolean.class, "false", false)
		);
	}

	@ParameterizedTest
	@MethodSource("providePrimitiveArgumentsForJsonConversion")
	void convertValueFromJson_allSupportedPrimitives_convertsProperly(Class<?> clazz, String json, Object expected) {
		var converted = strategy.convertValueFromJson(clazz.getCanonicalName(), json);
		assertEquals(expected, converted);
		assertTrue(clazz.isPrimitive() || clazz.isInstance(converted));
	}

	@ParameterizedTest
	@MethodSource("providePrimitiveArgumentsForJsonConversion")
	void convertValueToJson_allSupportedPrimitives_convertsProperly(Class<?> _clazz, String expectedJson, Object raw) {
		assertEquals(expectedJson, strategy.convertValueToJson(raw));
	}

	@Test
	void convertValueToJson_unsupportedType_throws() {
		var city = new City("Los Angeles");

		assertThrows(UnsupportedAttributeTypeException.class, () -> strategy.convertValueToJson(city));
	}

	/**
	 * An unsupported type example.
	 */
	protected static class City {
		protected String name;

		public City(String name) {
			this.name = name;
		}

		public String getName() {
			return name;
		}
	}
}
