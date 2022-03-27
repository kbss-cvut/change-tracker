package cz.cvut.kbss.changetracking.strategy.entity;

import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cvut.kbss.changetracking.TestIRIs;
import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.changetracking.exception.ClassNotAuditedException;
import cz.cvut.kbss.changetracking.exception.IdNotMatchingException;
import cz.cvut.kbss.changetracking.exception.ObjectsNotCompatibleException;
import cz.cvut.kbss.changetracking.model.*;
import cz.cvut.kbss.jopa.loaders.PersistenceUnitClassFinder;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.time.Instant;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

public class JopaEntityStrategyTest {
	static final String studentInstanceIri = "http://www.oni.unsc.org/spartanII/John117";
	static final String homeInstanceIri = "http://127.0.0.1/instance/SydneyHouse";
	static final String superheroInstanceIri = "http://127.0.0.1/instance/Jacob";
	static EntityStrategy<FieldSpecification<?, ?>> strategy;

	final ObjectMapper mapper = new ObjectMapper();

	@BeforeAll
	static void prepareStrategy() {
		final var config = new Configuration();
		config.set(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.changetracking.model");
		var metamodel = new MetamodelImpl(config);
		metamodel.build(new PersistenceUnitClassFinder());
		strategy = new JopaEntityStrategy(metamodel);
	}

	static ChangeVector getVector(Collection<ChangeVector> vectors, int index) {
		return vectors.toArray(ChangeVector[]::new)[index];
	}

	/**
	 * Perform all common assertions on a change vector specified by its properties and then return the matched vector.
	 * This method cannot directly access vectors in the collection by converting it into an array and then using an
	 * index because the order is unstable (due to underlying implementation in JOPA).
	 */
	static ChangeVector vectorAssert(
		Collection<ChangeVector> vectors,
		String objectType,
		String objectId,
		String attributeName,
		Object previousValue
	) {
		var vectorOptional = vectors.stream().filter(vector ->
			Objects.equals(objectType, vector.getObjectType())
			&& Objects.equals(objectId, vector.getObjectId())
			&& Objects.equals(attributeName, vector.getAttributeName())
			&& Objects.equals(previousValue, vector.getPreviousValue())
		).findFirst();
		assertTrue(vectorOptional.isPresent());
		var vector = vectorOptional.get();
		assertTrue(Instant.now().compareTo(vector.getTimestamp()) > 0);
		return vector;
	}

	@Test
	void getChangeVectors_studentsDifferingInFirstName_returnsOneVectorWithChangedName() {
		var student1 = new UndergraduateStudent(studentInstanceIri, "John", "Spartan");
		var student2 = new UndergraduateStudent(studentInstanceIri, "Dave", "Spartan");

		var vectors = strategy.getChangeVectors(student1, student2, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_STUDENT,
			studentInstanceIri,
			TestIRIs.PROPERTY_FIRST_NAME,
			student1.firstName
		);
	}

	@Test
	void getChangeVectors_unchangedStudent_returnsEmptyVectorCollection() {
		var student = new UndergraduateStudent(studentInstanceIri, "John", "Spartan");
		var vectors = strategy.getChangeVectors(student, student, true);
		assertEquals(0, vectors.size());
	}

	@Test
	void getChangeVectors_superAndSubClassWithNoDifferenceInSharedAttributes_returnsNoVectors() {
		var home = new Home(homeInstanceIri, "Sydney");
		var house = new House(homeInstanceIri, "Sydney", 4);

		var vectors = strategy.getChangeVectors(home, house, true);
		assertEquals(0, vectors.size());
	}

	@Test
	void getChangeVectors_superAndSubClassChangedSharedAttribute_returnsOneVector() {
		var home = new Home(homeInstanceIri, "Sydney");
		var house = new House(homeInstanceIri, "Los Angeles", 8);

		var vectors = strategy.getChangeVectors(home, house, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_HOME,
			homeInstanceIri,
			TestIRIs.PROPERTY_CITY,
			home.getCity()
		);
	}

	@Test
	void getChangeVectors_subAndSuperClassWithNoDifferenceInSharedAttributes_returnsNoVectors() {
		var home = new Home(homeInstanceIri, "Sydney");
		var house = new House(homeInstanceIri, "Sydney", 15);

		var vectors = strategy.getChangeVectors(house, home, true);
		assertEquals(0, vectors.size());
	}

	@Test
	void getChangeVectors_subAndSuperClassChangedSharedAttribute_returnsOneVector() {
		var home = new Home(homeInstanceIri, "Sydney");
		var house = new House(homeInstanceIri, "Los Angeles", 16);

		var vectors = strategy.getChangeVectors(house, home, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_HOME,
			homeInstanceIri,
			TestIRIs.PROPERTY_CITY,
			house.getCity()
		);
	}

	@Test
	void getChangeVectors_twoDifferentSubclassesEqualInSharedAttribute_returnsEmpty() {
		var house = new House(homeInstanceIri, "Sydney", 23);
		var apartment = new Apartment(homeInstanceIri, "Sydney", true);

		var vectors = strategy.getChangeVectors(house, apartment, true);
		assertTrue(vectors.isEmpty());
	}

	@Test
	void getChangeVectors_twoDifferentSubclassesDifferingInSharedAttribute_returnsOneVector() {
		var house = new House(homeInstanceIri, "Sydney", 23);
		var apartment = new Apartment(homeInstanceIri, "Los Angeles", true);

		var vectors = strategy.getChangeVectors(house, apartment, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_HOME,
			homeInstanceIri,
			TestIRIs.PROPERTY_CITY,
			house.getCity()
		);
	}

	@Test
	void getChangeVectors_superAndSubclassDifferingInSuperAttribute_returnsOneVector() {
		var ap = new Apartment(homeInstanceIri, "Sydney", true);
		var loft = new Loft(homeInstanceIri, "Sydney", false, 6);

		var vectors = strategy.getChangeVectors(ap, loft, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_APARTMENT,
			homeInstanceIri,
			TestIRIs.PROPERTY_HAS_BALCONY,
			ap.getHasBalcony()
		);
	}

	@Test
	void getChangeVectors_superAndSubclassDifferingInEvenHigherAncestorsAttribute_returnsOneVector() {
		var ap = new Apartment(homeInstanceIri, "Sydney", true);
		var loft = new Loft(homeInstanceIri, "Los Angeles", true, 6);

		var vectors = strategy.getChangeVectors(ap, loft, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_APARTMENT,
			homeInstanceIri,
			TestIRIs.PROPERTY_CITY,
			ap.getCity()
		);
	}

	@Test
	void getChangeVectors_superAndSubclassDifferingInBothSupersAndEvenHigherAncestorsAttribute_returnsOneVector() {
		var ap = new Apartment(homeInstanceIri, "Sydney", true);
		var loft = new Loft(homeInstanceIri, "Los Angeles", false, 6);

		var vectors = strategy.getChangeVectors(ap, loft, true);
		assertEquals(2, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_APARTMENT,
			homeInstanceIri,
			TestIRIs.PROPERTY_CITY,
			ap.getCity()
		);
		vectorAssert(
			vectors,
			TestIRIs.CLASS_APARTMENT,
			homeInstanceIri,
			TestIRIs.PROPERTY_HAS_BALCONY,
			ap.getHasBalcony()
		);
	}

	@Test
	void getChangeVectors_twoDifferentClassesWithNoCommonAncestor_throwsObjectsNotCompatibleException() {
		var home = new Home(homeInstanceIri, "Sydney");
		var student = new UndergraduateStudent(homeInstanceIri, "James", "LaFleur");

		assertThrows(ObjectsNotCompatibleException.class, () -> strategy.getChangeVectors(home, student, true));
	}

	@Test
	void getChangeVectors_propertySpecificationEmptyAndEmptyAndNoDataPropertyChanges_noVectors() {
		var hero1 = new Superhero(superheroInstanceIri, "Jacob", new HashMap<>());
		var hero2 = new Superhero(superheroInstanceIri, "Jacob", new HashMap<>());

		var vectors = strategy.getChangeVectors(hero1, hero2, true);
		assertEquals(0, vectors.size());
	}

	@Test
	void getChangeVectors_propertySpecificationEmptyAndNullAndNoDataPropertyChanges_noVectors() {
		var hero1 = new Superhero(superheroInstanceIri, "Jacob", new HashMap<>());
		var hero2 = new Superhero(superheroInstanceIri, "Jacob", null);

		var vectors = strategy.getChangeVectors(hero1, hero2, true);
		assertEquals(0, vectors.size());
	}

	@Test
	void getChangeVectors_propertySpecificationNullAndEmptyAndNoDataPropertyChanges_noVectors() {
		var hero1 = new Superhero(superheroInstanceIri, "Jacob", null);
		var hero2 = new Superhero(superheroInstanceIri, "Jacob", new HashMap<>());

		var vectors = strategy.getChangeVectors(hero1, hero2, true);
		assertEquals(0, vectors.size());
	}

	@Test
	void getChangeVectors_propertySpecificationEmptyAndEmptyAndOneDataPropertyChange_dataPropertyVectorOnly() {
		var hero1 = new Superhero(superheroInstanceIri, "Jacob", new HashMap<>());
		var hero2 = new Superhero(superheroInstanceIri, "Jake", new HashMap<>());

		var vectors = strategy.getChangeVectors(hero1, hero2, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_SUPERHERO,
			superheroInstanceIri,
			TestIRIs.PROPERTY_FIRST_NAME,
			"Jacob"
		);
	}

	@Test
	void getChangeVectors_propertySpecificationOnePropertyOnlyInOld_oneVectorWithPreviousValue() {
		var props = new HashMap<String, Set<String>>();
		props.put(TestIRIs.PROPERTY_GOOD_GUY, Collections.singleton(Boolean.TRUE.toString()));
		var hero1 = new Superhero(superheroInstanceIri, "Jacob", props);
		var hero2 = new Superhero(superheroInstanceIri, "Jacob", new HashMap<>());

		var vectors = strategy.getChangeVectors(hero1, hero2, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_SUPERHERO,
			superheroInstanceIri,
			TestIRIs.PROPERTY_GOOD_GUY,
			// TODO: make it not require the toString()
			Collections.singleton(Boolean.TRUE.toString())
		);
	}

	@Test
	void getChangeVectors_propertySpecificationOnePropertyOnlyInNew_oneVectorWithNull() {
		var props = new HashMap<String, Set<String>>();
		props.put(TestIRIs.PROPERTY_GOOD_GUY, Collections.singleton(Boolean.TRUE.toString()));
		var hero1 = new Superhero(superheroInstanceIri, "Jacob", new HashMap<>());
		var hero2 = new Superhero(superheroInstanceIri, "Jacob", props);

		var vectors = strategy.getChangeVectors(hero1, hero2, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_SUPERHERO,
			superheroInstanceIri,
			TestIRIs.PROPERTY_GOOD_GUY,
			null
		);
	}

	@Test
	void getChangeVectors_propertySpecificationOnePropertyDiffering_oneVectorWithPreviousValue() {
		var props1 = new HashMap<String, Set<String>>();
		props1.put(TestIRIs.PROPERTY_GOOD_GUY, Collections.singleton(Boolean.FALSE.toString()));
		var props2 = new HashMap<String, Set<String>>();
		props2.put(TestIRIs.PROPERTY_GOOD_GUY, Collections.singleton(Boolean.TRUE.toString()));
		var hero1 = new Superhero(superheroInstanceIri, "Jacob", props1);
		var hero2 = new Superhero(superheroInstanceIri, "Jacob", props2);

		var vectors = strategy.getChangeVectors(hero1, hero2, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_SUPERHERO,
			superheroInstanceIri,
			TestIRIs.PROPERTY_GOOD_GUY,
			// TODO: make it not require the toString()
			Collections.singleton(Boolean.FALSE.toString())
		);
	}

	@Test
	void getChangeVectors_propertySpecificationOnePropertyAndOneDataPropDiffering_twoVectorsWithPreviousValues() {
		var props1 = new HashMap<String, Set<String>>();
		props1.put(TestIRIs.PROPERTY_GOOD_GUY, Collections.singleton(Boolean.FALSE.toString()));
		var props2 = new HashMap<String, Set<String>>();
		props2.put(TestIRIs.PROPERTY_GOOD_GUY, Collections.singleton(Boolean.TRUE.toString()));
		var hero1 = new Superhero(superheroInstanceIri, "Jacob", props1);
		var hero2 = new Superhero(superheroInstanceIri, "Jake", props2);

		var vectors = strategy.getChangeVectors(hero1, hero2, true);
		assertEquals(2, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_SUPERHERO,
			superheroInstanceIri,
			TestIRIs.PROPERTY_FIRST_NAME,
			"Jacob"
		);
		vectorAssert(
			vectors,
			TestIRIs.CLASS_SUPERHERO,
			superheroInstanceIri,
			TestIRIs.PROPERTY_GOOD_GUY,
			// TODO: make it not require the toString()
			Collections.singleton(Boolean.FALSE.toString())
		);
	}

	@Test
	void getChangeVectors_differentIdsAndRequireSameIdTrue_throwsIdNotMatchingException() {
		var home1 = new Home(homeInstanceIri + "1", "Sydney");
		var home2 = new Home(homeInstanceIri + "2", "Sydney");

		assertThrows(IdNotMatchingException.class, () -> strategy.getChangeVectors(home1, home2, true));
	}

	@Test
	void getChangeVectors_differentIdsAndRequireSameIdFalse_doesNotThrow() {
		var home1 = new Home(homeInstanceIri + "1", "Sydney");
		var home2 = new Home(homeInstanceIri + "2", "Sydney");

		assertDoesNotThrow(() -> strategy.getChangeVectors(home1, home2, false));
	}

	@Test
	void getChangeVectors_addedType_returnsVectorWithOldTypesSet() {
		var types1 = new HashSet<>(Collections.singleton(TestIRIs.TYPE_MAN));
		var student1 = new TypedStudent(studentInstanceIri, types1);

		var types2 = new HashSet<>(types1);
		types2.add(TestIRIs.TYPE_STUDENT);
		var student2 = new TypedStudent(studentInstanceIri, types2);

		var vecs = strategy.getChangeVectors(student1, student2, true);

		assertEquals(1, vecs.size());
		vectorAssert(vecs, TestIRIs.CLASS_STUDENT, studentInstanceIri, RDF.TYPE, types1);
	}

	@Test
	void getChangeVectors_removedType_returnsVectorWithTheOldTypesSet() {
		var types1 = new HashSet<>(List.of(TestIRIs.TYPE_MAN, TestIRIs.TYPE_STUDENT));
		var student1 = new TypedStudent(studentInstanceIri, types1);

		var types2 = new HashSet<>(types1);
		types2.remove(TestIRIs.TYPE_STUDENT);
		var student2 = new TypedStudent(studentInstanceIri, types2);

		var vecs = strategy.getChangeVectors(student1, student2, true);

		assertEquals(1, vecs.size());
		vectorAssert(vecs, TestIRIs.CLASS_STUDENT, studentInstanceIri, RDF.TYPE, types1);
	}

	@Test
	void getChangeVectors_unchangedTypes_returnsEmptyVectorCollection() {
		var types = new HashSet<>(List.of(TestIRIs.TYPE_MAN, TestIRIs.TYPE_STUDENT));
		var student1 = new TypedStudent(studentInstanceIri, types);
		var student2 = new TypedStudent(studentInstanceIri, types);

		var vecs = strategy.getChangeVectors(student1, student2, true);

		assertTrue(vecs.isEmpty());
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
