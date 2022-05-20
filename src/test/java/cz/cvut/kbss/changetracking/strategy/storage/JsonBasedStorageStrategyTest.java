package cz.cvut.kbss.changetracking.strategy.storage;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cvut.kbss.changetracking.exception.UnsupportedAttributeTypeException;
import cz.cvut.kbss.changetracking.util.ClassUtil;
import org.junit.jupiter.api.Named;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.opentest4j.AssertionFailedError;

import java.util.*;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

class JsonBasedStorageStrategyTest {
	JsonBasedStorageStrategy strategy = new TestJsonBasedStorageStrategyImpl();
	static ObjectMapper mapper = new ObjectMapper();

	static Stream<Arguments> providePrimitiveArgumentsForJsonConversion() {
		return Stream.of(
			Arguments.of(String.class, "\"\"", ""),
			Arguments.of(String.class, "\"hello\"", "hello"),
			Arguments.of(Character.class, "\"c\"", 'c'),
			Arguments.of(Double.class, "66.667", 66.667d),
			Arguments.of(Float.class, "9999999.0", 9999999f),
			Arguments.of(Integer.class, "100", 100),
			Arguments.of(Boolean.class, "true", true)
		);
	}

	/**
	 * Check if a deserialized serialized set has the same size and elements as its original form. This is required
	 * because set elements may not always be (de)serialized in the initial order.
	 */
	static <E> void assertJsonArrayContainsSetValues(String jsonArray, Set<E> elements, Class<E> targetClass) throws JsonProcessingException {
		var list = List.of((Object[]) mapper.readValue(jsonArray, ClassUtil.getArrayClassFromSingular(targetClass)));
		assertEquals(elements.size(), list.size());
		assertTrue(list.containsAll(elements));
	}

	static Stream<Arguments> providePrimitiveListArgumentsForJsonConversion() {
		return Stream.of(
			Arguments.of(Named.of("string list", List.of("Aaron", "Shephard")), String.class, "[\"Aaron\",\"Shephard\"]"),
			Arguments.of(Named.of("integer list", List.of(4, 8)), Integer.class, "[4,8]"),
			Arguments.of(Named.of("double list", List.of(1.5, 1.6)), Double.class, "[1.5,1.6]"),
			Arguments.of(Named.of("boolean list", List.of(false, true)), Boolean.class, "[false,true]")
		);
	}

	@SuppressWarnings("unchecked")
	static Stream<Arguments> providePrimitiveSetArgumentsForJsonConversion() {
		return providePrimitiveListArgumentsForJsonConversion().map(args -> {
			var argArray = args.get();
			var named = (Named<List<?>>) argArray[0];
			return Arguments.of(
				Named.of(
					named.getName().replace("list", "set"),
					new HashSet<>(named.getPayload())
				),
				argArray[1]
			);
		});
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
	void convertValueToJson_emptySet_returnsEmptyJsonArray() {
		assertEquals("[]", strategy.convertValueToJson(new HashSet<String>()));
	}

	@ParameterizedTest(name = "{0}")
	@MethodSource("providePrimitiveSetArgumentsForJsonConversion")
	<E> void convertValueToJson_set_returnsJsonArray(Set<E> set, Class<E> clazz) throws JsonProcessingException {
		var json = strategy.convertValueToJson(set);
		assertJsonArrayContainsSetValues(json, set, clazz);
	}

	@Test
	void convertValueToJson_emptyList_returnsEmptyJsonArray() {
		assertEquals("[]", strategy.convertValueToJson(new ArrayList<String>()));
	}

	@ParameterizedTest(name = "{0}")
	@MethodSource("providePrimitiveListArgumentsForJsonConversion")
	<E> void convertValueToJson_list_returnsJsonArray(List<E> list, Class<E> _clazz, String expectedJson) {
		var json = strategy.convertValueToJson(list);
		assertEquals(expectedJson, json);
	}

	@ParameterizedTest
	@MethodSource("providePrimitiveListArgumentsForJsonConversion")
	<E> void convertValueFromJson_primitives_returnsCorrectIterable(List<E> expectedList, Class<E> clazz, String json) {
		var value = strategy.convertValueFromJson(clazz.getCanonicalName() + "[]", json);
		assertIterableEquals(expectedList, (Iterable<?>) value);
	}
}
