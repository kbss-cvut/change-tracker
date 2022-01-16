package cz.cvut.kbss.changetracking.strategy.entity;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.changetracking.exception.ClassNotAuditedException;
import cz.cvut.kbss.changetracking.model.JsonChangeVector;
import cz.cvut.kbss.jopa.loaders.PersistenceUnitClassFinder;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.net.URI;
import java.time.Instant;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

public class JopaEntityStrategyTest {
	static final String studentClassIri = "http://uob.iodt.ibm.com/univ-bench-dl.owl#UndergraduateStudent";
	static final String studentInstanceIri = "http://www.oni.unsc.org/spartanII/John117";
	static EntityStrategy<Attribute<?, ?>> strategy;

	final ObjectMapper mapper = new ObjectMapper();

	@BeforeAll
	static void prepareStrategy() {
		final var config = new Configuration();
		config.set(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.changetracking.strategy.entity");
		var metamodel = new MetamodelImpl(config);
		metamodel.build(new PersistenceUnitClassFinder());
		strategy = new JopaEntityStrategy(metamodel);
	}

	@SuppressWarnings("cast")
	static Stream<Arguments> provideArgumentsForJsonConversion() {
		return Stream.of(
			Arguments.of(String.class, "\"\"", (Object) ""),
			Arguments.of(String.class, "\"hello\"", (Object) "hello"),
			Arguments.of(Character.class, "\"c\"", (Object) 'c'),
			Arguments.of(char.class, "\"x\"", (Object) 'x'),
			Arguments.of(Double.class, "66.667", (Object) 66.667d),
			Arguments.of(double.class, "10.03", (Object) 10.03d),
			Arguments.of(Float.class, "9999999.0", (Object) 9999999f),
			Arguments.of(float.class, "-13.9", (Object) (0 - 13.9f)),
			Arguments.of(Integer.class, "100", (Object) 100),
			Arguments.of(int.class, "10", (Object) 10),
			Arguments.of(Boolean.class, "true", (Object) true),
			Arguments.of(boolean.class, "false", (Object) false)
		);
	}

	@Test
	void getChangeVectors_studentsDifferingInFirstName_returnsOneVectorWithChangedName() throws JsonProcessingException {
		var student1 = new UndergraduateStudent(studentInstanceIri, "John", "Spartan");
		var student2 = new UndergraduateStudent(studentInstanceIri, "Dave", "Spartan");

		var vectors = strategy.getChangeVectors(student1, student2);
		assertEquals(1, vectors.size());
		var vector = vectors.toArray(JsonChangeVector[]::new)[0];
		assertEquals(studentClassIri, vector.getObjectType());
		assertEquals(studentInstanceIri, vector.getObjectId());
		assertEquals("http://uob.iodt.ibm.com/univ-bench-dl.owl#firstName", vector.getAttributeName());
		// note that we are working with a JsonChangeVector, not a ChangeVector here!
		assertEquals(mapper.writeValueAsString(student1.firstName), vector.getPreviousValue());
		assertEquals("java.lang.String", vector.getAttributeType());
		//assertNotNull(vector.getId());
		assertTrue(Instant.now().compareTo(vector.getTimestamp()) > 0);
	}

	@Test
	void getChangeVectors_unchangedStudent_returnsEmptyVectorCollection() {
		var student = new UndergraduateStudent(studentInstanceIri, "John", "Spartan");
		var vectors = strategy.getChangeVectors(student, student);
		assertEquals(0, vectors.size());
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

	// FIXME: this may be semantically wrong
	@Test
	void checkClassSupported_classWithoutOWLAnnotation_throwsClassNotAuditedException() {
		AuditedCourse course = new AuditedCourse();

		assertThrows(ClassNotAuditedException.class, () -> strategy.checkClassSupported(course.getClass()));
	}

	@Test
	void checkClassSupported_classWithoutAuditedAnnotation_throwsClassNotAuditedException() {
		OwlCourse course = new OwlCourse();

		assertThrows(ClassNotAuditedException.class, () -> strategy.checkClassSupported(course.getClass()));
	}

	@Audited
	static class AuditedCourse {
		private URI uri;
		private String name;
	}

	@OWLClass(iri = "http://uob.iodt.ibm.com/univ-bench-dl.owl#Course")
	static class OwlCourse {
		@Id
		private URI uri;

		@OWLDataProperty(iri = "http://uob.iodt.ibm.com/univ-bench-dl.owl#name")
		private String name;
	}
}
