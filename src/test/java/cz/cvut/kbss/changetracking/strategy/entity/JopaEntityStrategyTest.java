package cz.cvut.kbss.changetracking.strategy.entity;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.changetracking.exception.ClassNotAuditedException;
import cz.cvut.kbss.changetracking.model.JsonChangeVector;
import cz.cvut.kbss.jopa.loaders.PersistenceUnitClassFinder;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.time.Instant;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class JopaEntityStrategyTest {
	static final String studentClassIri = "http://uob.iodt.ibm.com/univ-bench-dl.owl#UndergraduateStudent";
	static final String studentInstanceIri = "http://www.oni.unsc.org/spartanII/John117";
	static EntityStrategy<Object> strategy;

	final ObjectMapper mapper = new ObjectMapper();

	@BeforeAll
	static void prepareStrategy() {
		final var config = new Configuration();
		config.set(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.changetracking.strategy.entity");
		var metamodel = new MetamodelImpl(config);
		metamodel.build(new PersistenceUnitClassFinder());
		strategy = new JopaEntityStrategy(metamodel);
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
