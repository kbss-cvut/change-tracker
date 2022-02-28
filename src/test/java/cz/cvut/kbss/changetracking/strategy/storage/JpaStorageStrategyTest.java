package cz.cvut.kbss.changetracking.strategy.storage;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class JpaStorageStrategyTest {
	// TODO: implement tests using Spring Boot
	JpaStorageStrategy strategy = new JpaStorageStrategy(null);

	static Stream<Arguments> provideArgumentsForJsonConversion() {
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
	@MethodSource("provideArgumentsForJsonConversion")
	void convertValueFromJson_allSupportedTypes_convertsProperly(Class<?> clazz, String json, Object expected) {
		var converted = strategy.convertValueFromJson(clazz.getCanonicalName(), json);
		assertEquals(expected, converted);
		assertTrue(clazz.isPrimitive() || clazz.isInstance(converted));
	}

	@ParameterizedTest
	@MethodSource("provideArgumentsForJsonConversion")
	void convertValueToJson_allSupportedTypes_convertsProperly(Class<?> _clazz, String expectedJson, Object raw) {
		assertEquals(expectedJson, strategy.convertValueToJson(raw));
	}
}
