package cz.cvut.kbss.changetracking.strategy.entity;

import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.changetracking.exception.ClassNotAuditedException;
import cz.cvut.kbss.changetracking.model.ChangeVector;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.time.Instant;
import java.util.HashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class JopaEntityStrategyTest {
	static final String studentClassIri = "http://uob.iodt.ibm.com/univ-bench-dl.owl#UndergraduateStudent";
	static final String studentInstanceIri = "http://www.oni.unsc.org/spartanII/John117";
	static Metamodel mmMock;
	static EntityStrategy<Object> strategy;

	static Attribute<?, ?> getAttribute(String name) throws NoSuchFieldException {
		var field = UndergraduateStudent.class.getDeclaredField(name);
		var attr = mock(Attribute.class);
		when(attr.getJavaField()).thenReturn(field);
		when(attr.getName()).thenReturn(name);
		when(attr.getIRI()).thenReturn(IRI.create(field.getAnnotation(OWLDataProperty.class).iri()));
		return attr;
	}

	@BeforeAll
	static void prepareEMMock() throws IllegalAccessException, NoSuchFieldException {
		var jFieldId = mock(Field.class);
		when(jFieldId.get(any())).thenReturn(studentInstanceIri);
		when(jFieldId.canAccess(any())).thenReturn(true);

		var idSpec = mock(Identifier.class);
		when(idSpec.getJavaField()).thenReturn(jFieldId);

		var attributeSet = new HashSet<>(List.of(getAttribute("firstName"), getAttribute("lastName")));

		var studentEntityType = mock(EntityType.class);
		when(studentEntityType.getIRI()).thenReturn(IRI.create(studentClassIri));
		when(studentEntityType.getIdentifier()).thenReturn(idSpec);
		when(studentEntityType.getAttributes()).thenReturn(attributeSet);

		mmMock = mock(Metamodel.class);
		when(mmMock.entity(UndergraduateStudent.class)).thenReturn(studentEntityType);

		strategy = new JopaEntityStrategy(mmMock);
	}

	@Test
	void getChangeVectors_studentsDifferingInFirstName_returnsOneVectorWithChangedName() {
		var student1 = new UndergraduateStudent(studentInstanceIri, "John", "Spartan");
		var student2 = new UndergraduateStudent(studentInstanceIri, "Dave", "Spartan");

		var vectors = strategy.getChangeVectors(student1, student2);
		assertEquals(1, vectors.size());
		var vector = vectors.toArray(ChangeVector[]::new)[0];
		assertEquals(studentClassIri, vector.getObjectType());
		assertEquals(studentInstanceIri, vector.getObjectId());
		assertEquals("http://uob.iodt.ibm.com/univ-bench-dl.owl#firstName", vector.getAttributeName());
		assertEquals(student1.firstName, vector.getPreviousValue());
		assertEquals("java.lang.String", vector.getAttributeType());
		//assertNotNull(vector.getId());
		assertTrue(Instant.now().compareTo(vector.getTimestamp()) > 0);
	}

	@Test
	void getChangeVectors_unchangedStudent_returnsEmptyVectorCollection() {
		var strat = new JopaEntityStrategy(mmMock);
		var student = new UndergraduateStudent(studentInstanceIri, "John", "Spartan");
		var vectors = strat.getChangeVectors(student, student);
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

	@OWLClass(iri = studentClassIri)
	public static class UndergraduateStudent {

		@Id
		private final URI uri;

		@OWLDataProperty(iri = "http://uob.iodt.ibm.com/univ-bench-dl.owl#firstName")
		private final String firstName;

		@OWLDataProperty(iri = "http://uob.iodt.ibm.com/univ-bench-dl.owl#lastName")
		private final String lastName;

		public UndergraduateStudent(String uri, String firstName, String lastName) {
			this.uri = URI.create(uri);
			this.firstName = firstName;
			this.lastName = lastName;
		}
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
