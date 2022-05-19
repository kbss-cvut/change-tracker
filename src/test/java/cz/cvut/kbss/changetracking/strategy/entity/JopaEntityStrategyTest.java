package cz.cvut.kbss.changetracking.strategy.entity;

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
	static BaseEntityStrategy<FieldSpecification<?, ?>> strategy;

	@BeforeAll
	static void prepareStrategy() {
		final var config = new Configuration();
		config.set(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.changetracking.model");
		var metamodel = new MetamodelImpl(config);
		metamodel.build(new PersistenceUnitClassFinder());
		strategy = new JopaEntityStrategy(metamodel);
	}

	/**
	 * Perform all common assertions on a change vector specified by its properties and then return the matched vector.
	 * This method cannot directly access vectors in the collection by converting it into an array and then using an
	 * index because the order is unstable (due to underlying implementation in JOPA).
	 */
	static ChangeVector<?> vectorAssert(
		Collection<ChangeVector<?>> vectors,
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
		var student1 = new UndergraduateStudent(TestIRIs.INSTANCE_STUDENT, "John", "Spartan");
		var student2 = new UndergraduateStudent(TestIRIs.INSTANCE_STUDENT, "Dave", "Spartan");

		var vectors = strategy.getChangeVectors(student1, student2, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_STUDENT,
			TestIRIs.INSTANCE_STUDENT,
			TestIRIs.PROPERTY_FIRST_NAME,
			student1.firstName
		);
	}

	@Test
	void getChangeVectors_unchangedStudent_returnsEmptyVectorCollection() {
		var student = new UndergraduateStudent(TestIRIs.INSTANCE_STUDENT, "John", "Spartan");
		var vectors = strategy.getChangeVectors(student, student, true);
		assertEquals(0, vectors.size());
	}

	@Test
	void getChangeVectors_superAndSubClassWithNoDifferenceInSharedAttributes_returnsNoVectors() {
		var home = new Home(TestIRIs.INSTANCE_HOME, "Sydney");
		var house = new House(TestIRIs.INSTANCE_HOME, "Sydney", 4);

		var vectors = strategy.getChangeVectors(home, house, true);
		assertEquals(0, vectors.size());
	}

	@Test
	void getChangeVectors_superAndSubClassChangedSharedAttribute_returnsOneVector() {
		var home = new Home(TestIRIs.INSTANCE_HOME, "Sydney");
		var house = new House(TestIRIs.INSTANCE_HOME, "Los Angeles", 8);

		var vectors = strategy.getChangeVectors(home, house, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_HOME,
			TestIRIs.INSTANCE_HOME,
			TestIRIs.PROPERTY_CITY,
			home.getCity()
		);
	}

	@Test
	void getChangeVectors_subAndSuperClassWithNoDifferenceInSharedAttributes_returnsNoVectors() {
		var home = new Home(TestIRIs.INSTANCE_HOME, "Sydney");
		var house = new House(TestIRIs.INSTANCE_HOME, "Sydney", 15);

		var vectors = strategy.getChangeVectors(house, home, true);
		assertEquals(0, vectors.size());
	}

	@Test
	void getChangeVectors_subAndSuperClassChangedSharedAttribute_returnsOneVector() {
		var home = new Home(TestIRIs.INSTANCE_HOME, "Sydney");
		var house = new House(TestIRIs.INSTANCE_HOME, "Los Angeles", 16);

		var vectors = strategy.getChangeVectors(house, home, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_HOME,
			TestIRIs.INSTANCE_HOME,
			TestIRIs.PROPERTY_CITY,
			house.getCity()
		);
	}

	@Test
	void getChangeVectors_twoDifferentSubclassesEqualInSharedAttribute_returnsEmpty() {
		var house = new House(TestIRIs.INSTANCE_HOME, "Sydney", 23);
		var apartment = new Apartment(TestIRIs.INSTANCE_HOME, "Sydney", true);

		var vectors = strategy.getChangeVectors(house, apartment, true);
		assertTrue(vectors.isEmpty());
	}

	@Test
	void getChangeVectors_twoDifferentSubclassesDifferingInSharedAttribute_returnsOneVector() {
		var house = new House(TestIRIs.INSTANCE_HOME, "Sydney", 23);
		var apartment = new Apartment(TestIRIs.INSTANCE_HOME, "Los Angeles", true);

		var vectors = strategy.getChangeVectors(house, apartment, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_HOME,
			TestIRIs.INSTANCE_HOME,
			TestIRIs.PROPERTY_CITY,
			house.getCity()
		);
	}

	@Test
	void getChangeVectors_superAndSubclassDifferingInSuperAttribute_returnsOneVector() {
		var ap = new Apartment(TestIRIs.INSTANCE_HOME, "Sydney", true);
		var loft = new Loft(TestIRIs.INSTANCE_HOME, "Sydney", false, 6);

		var vectors = strategy.getChangeVectors(ap, loft, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_APARTMENT,
			TestIRIs.INSTANCE_HOME,
			TestIRIs.PROPERTY_HAS_BALCONY,
			ap.getHasBalcony()
		);
	}

	@Test
	void getChangeVectors_superAndSubclassDifferingInEvenHigherAncestorsAttribute_returnsOneVector() {
		var ap = new Apartment(TestIRIs.INSTANCE_HOME, "Sydney", true);
		var loft = new Loft(TestIRIs.INSTANCE_HOME, "Los Angeles", true, 6);

		var vectors = strategy.getChangeVectors(ap, loft, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_APARTMENT,
			TestIRIs.INSTANCE_HOME,
			TestIRIs.PROPERTY_CITY,
			ap.getCity()
		);
	}

	@Test
	void getChangeVectors_superAndSubclassDifferingInBothSupersAndEvenHigherAncestorsAttribute_returnsOneVector() {
		var ap = new Apartment(TestIRIs.INSTANCE_HOME, "Sydney", true);
		var loft = new Loft(TestIRIs.INSTANCE_HOME, "Los Angeles", false, 6);

		var vectors = strategy.getChangeVectors(ap, loft, true);
		assertEquals(2, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_APARTMENT,
			TestIRIs.INSTANCE_HOME,
			TestIRIs.PROPERTY_CITY,
			ap.getCity()
		);
		vectorAssert(
			vectors,
			TestIRIs.CLASS_APARTMENT,
			TestIRIs.INSTANCE_HOME,
			TestIRIs.PROPERTY_HAS_BALCONY,
			ap.getHasBalcony()
		);
	}

	@Test
	void getChangeVectors_twoDifferentClassesWithNoCommonAncestor_throwsObjectsNotCompatibleException() {
		var home = new Home(TestIRIs.INSTANCE_HOME, "Sydney");
		var student = new UndergraduateStudent(TestIRIs.INSTANCE_HOME, "James", "LaFleur");

		assertThrows(ObjectsNotCompatibleException.class, () -> strategy.getChangeVectors(home, student, true));
	}

	@Test
	void getChangeVectors_propertySpecificationEmptyAndEmptyAndNoDataPropertyChanges_noVectors() {
		var hero1 = new Superhero(TestIRIs.INSTANCE_SUPERHERO, "Jacob", new HashMap<>());
		var hero2 = new Superhero(TestIRIs.INSTANCE_SUPERHERO, "Jacob", new HashMap<>());

		var vectors = strategy.getChangeVectors(hero1, hero2, true);
		assertEquals(0, vectors.size());
	}

	@Test
	void getChangeVectors_propertySpecificationEmptyAndNullAndNoDataPropertyChanges_noVectors() {
		var hero1 = new Superhero(TestIRIs.INSTANCE_SUPERHERO, "Jacob", new HashMap<>());
		var hero2 = new Superhero(TestIRIs.INSTANCE_SUPERHERO, "Jacob", null);

		var vectors = strategy.getChangeVectors(hero1, hero2, true);
		assertEquals(0, vectors.size());
	}

	@Test
	void getChangeVectors_propertySpecificationNullAndEmptyAndNoDataPropertyChanges_noVectors() {
		var hero1 = new Superhero(TestIRIs.INSTANCE_SUPERHERO, "Jacob", null);
		var hero2 = new Superhero(TestIRIs.INSTANCE_SUPERHERO, "Jacob", new HashMap<>());

		var vectors = strategy.getChangeVectors(hero1, hero2, true);
		assertEquals(0, vectors.size());
	}

	@Test
	void getChangeVectors_propertySpecificationEmptyAndEmptyAndOneDataPropertyChange_dataPropertyVectorOnly() {
		var hero1 = new Superhero(TestIRIs.INSTANCE_SUPERHERO, "Jacob", new HashMap<>());
		var hero2 = new Superhero(TestIRIs.INSTANCE_SUPERHERO, "Jake", new HashMap<>());

		var vectors = strategy.getChangeVectors(hero1, hero2, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_SUPERHERO,
			TestIRIs.INSTANCE_SUPERHERO,
			TestIRIs.PROPERTY_FIRST_NAME,
			"Jacob"
		);
	}

	@Test
	void getChangeVectors_propertySpecificationOnePropertyOnlyInOld_oneVectorWithPreviousValue() {
		var props = new HashMap<String, Set<String>>();
		props.put(TestIRIs.PROPERTY_GOOD_GUY, Collections.singleton(Boolean.TRUE.toString()));
		var hero1 = new Superhero(TestIRIs.INSTANCE_SUPERHERO, "Jacob", props);
		var hero2 = new Superhero(TestIRIs.INSTANCE_SUPERHERO, "Jacob", new HashMap<>());

		var vectors = strategy.getChangeVectors(hero1, hero2, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_SUPERHERO,
			TestIRIs.INSTANCE_SUPERHERO,
			TestIRIs.PROPERTY_GOOD_GUY,
			// TODO: make it not require the toString()
			Collections.singleton(Boolean.TRUE.toString())
		);
	}

	@Test
	void getChangeVectors_propertySpecificationOnePropertyOnlyInNew_oneVectorWithNull() {
		var props = new HashMap<String, Set<String>>();
		props.put(TestIRIs.PROPERTY_GOOD_GUY, Collections.singleton(Boolean.TRUE.toString()));
		var hero1 = new Superhero(TestIRIs.INSTANCE_SUPERHERO, "Jacob", new HashMap<>());
		var hero2 = new Superhero(TestIRIs.INSTANCE_SUPERHERO, "Jacob", props);

		var vectors = strategy.getChangeVectors(hero1, hero2, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_SUPERHERO,
			TestIRIs.INSTANCE_SUPERHERO,
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
		var hero1 = new Superhero(TestIRIs.INSTANCE_SUPERHERO, "Jacob", props1);
		var hero2 = new Superhero(TestIRIs.INSTANCE_SUPERHERO, "Jacob", props2);

		var vectors = strategy.getChangeVectors(hero1, hero2, true);
		assertEquals(1, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_SUPERHERO,
			TestIRIs.INSTANCE_SUPERHERO,
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
		var hero1 = new Superhero(TestIRIs.INSTANCE_SUPERHERO, "Jacob", props1);
		var hero2 = new Superhero(TestIRIs.INSTANCE_SUPERHERO, "Jake", props2);

		var vectors = strategy.getChangeVectors(hero1, hero2, true);
		assertEquals(2, vectors.size());
		vectorAssert(
			vectors,
			TestIRIs.CLASS_SUPERHERO,
			TestIRIs.INSTANCE_SUPERHERO,
			TestIRIs.PROPERTY_FIRST_NAME,
			"Jacob"
		);
		vectorAssert(
			vectors,
			TestIRIs.CLASS_SUPERHERO,
			TestIRIs.INSTANCE_SUPERHERO,
			TestIRIs.PROPERTY_GOOD_GUY,
			// TODO: make it not require the toString()
			Collections.singleton(Boolean.FALSE.toString())
		);
	}

	@Test
	void getChangeVectors_differentIdsAndRequireSameIdTrue_throwsIdNotMatchingException() {
		var home1 = new Home(TestIRIs.INSTANCE_HOME + "1", "Sydney");
		var home2 = new Home(TestIRIs.INSTANCE_HOME + "2", "Sydney");

		assertThrows(IdNotMatchingException.class, () -> strategy.getChangeVectors(home1, home2, true));
	}

	@Test
	void getChangeVectors_differentIdsAndRequireSameIdFalse_doesNotThrow() {
		var home1 = new Home(TestIRIs.INSTANCE_HOME + "1", "Sydney");
		var home2 = new Home(TestIRIs.INSTANCE_HOME + "2", "Sydney");

		assertDoesNotThrow(() -> strategy.getChangeVectors(home1, home2, false));
	}

	@Test
	void getChangeVectors_addedType_returnsVectorWithOldTypesSet() {
		var types1 = new HashSet<>(Collections.singleton(TestIRIs.TYPE_MAN));
		var student1 = new TypedStudent(TestIRIs.INSTANCE_STUDENT, types1);

		var types2 = new HashSet<>(types1);
		types2.add(TestIRIs.TYPE_STUDENT);
		var student2 = new TypedStudent(TestIRIs.INSTANCE_STUDENT, types2);

		var vecs = strategy.getChangeVectors(student1, student2, true);

		assertEquals(1, vecs.size());
		vectorAssert(vecs, TestIRIs.CLASS_STUDENT, TestIRIs.INSTANCE_STUDENT, RDF.TYPE, types1);
	}

	@Test
	void getChangeVectors_removedType_returnsVectorWithTheOldTypesSet() {
		var types1 = new HashSet<>(List.of(TestIRIs.TYPE_MAN, TestIRIs.TYPE_STUDENT));
		var student1 = new TypedStudent(TestIRIs.INSTANCE_STUDENT, types1);

		var types2 = new HashSet<>(types1);
		types2.remove(TestIRIs.TYPE_STUDENT);
		var student2 = new TypedStudent(TestIRIs.INSTANCE_STUDENT, types2);

		var vecs = strategy.getChangeVectors(student1, student2, true);

		assertEquals(1, vecs.size());
		vectorAssert(vecs, TestIRIs.CLASS_STUDENT, TestIRIs.INSTANCE_STUDENT, RDF.TYPE, types1);
	}

	@Test
	void getChangeVectors_unchangedTypes_returnsEmptyVectorCollection() {
		var types = new HashSet<>(List.of(TestIRIs.TYPE_MAN, TestIRIs.TYPE_STUDENT));
		var student1 = new TypedStudent(TestIRIs.INSTANCE_STUDENT, types);
		var student2 = new TypedStudent(TestIRIs.INSTANCE_STUDENT, types);

		var vecs = strategy.getChangeVectors(student1, student2, true);

		assertTrue(vecs.isEmpty());
	}

	@Test
	void getChangeVectors_unchangedObjectProperty_noVectors() {
		var mother = new Person(TestIRIs.INSTANCE_MOTHER, "Austen", null);
		var child1 = new Person(TestIRIs.INSTANCE_CHILD, "Austen", mother);
		var child2 = new Person(TestIRIs.INSTANCE_CHILD, "Austen", mother);
		mother.addChildren(child1, child2);

		var vecs = strategy.getChangeVectors(child1, child2, true);

		assertTrue(vecs.isEmpty());
	}

	@Test
	void getChangeVectors_changedObjectProperty_vectorWithIdentifier() {
		var motherIri1 = TestIRIs.INSTANCE_MOTHER + "1";
		var motherIri2 = TestIRIs.INSTANCE_MOTHER + "2";

		var mother1 = new Person(motherIri1, "Littleton", null);
		var mother2 = new Person(motherIri2, "Littleton", null);
		var child1 = new Person(TestIRIs.INSTANCE_CHILD, "Littleton", mother1);
		var child2 = new Person(TestIRIs.INSTANCE_CHILD, "Littleton", mother2);

		var vecs = strategy.getChangeVectors(child1, child2, true);

		vectorAssert(
			vecs,
			TestIRIs.CLASS_PERSON,
			TestIRIs.INSTANCE_CHILD,
			TestIRIs.PROPERTY_OBJECT_HAS_MOTHER,
			URI.create(motherIri1)
		);
	}

	@Test
	void getChangeVectors_changedObjectPropertySet_vectorWithIdentifier() {
		var childInstanceIri1 = TestIRIs.INSTANCE_CHILD + "1";
		var childInstanceIri2 = TestIRIs.INSTANCE_CHILD + "2";

		var mother1 = new Person(TestIRIs.INSTANCE_MOTHER, "Shephard", null);
		var child1 = new Person(childInstanceIri1, "Not Shephard", mother1);
		var child2 = new Person(childInstanceIri2, "Not Shephard", mother1);

		mother1.addChildren(child1);
		var mother2 = new Person(mother1);
		mother2.addChildren(child2);

		var vecs = strategy.getChangeVectors(mother1, mother2, true);

		vectorAssert(
			vecs,
			TestIRIs.CLASS_PERSON,
			TestIRIs.INSTANCE_MOTHER,
			TestIRIs.PROPERTY_OBJECT_HAS_CHILD,
			Set.of(URI.create(childInstanceIri1))
		);
	}

	@Test
	void getChangeVectors_unchangedURIObjectProperty_noVectors() {
		var carMother1 = new Car(TestIRIs.INSTANCE_CAR, TestIRIs.INSTANCE_MOTHER);
		var carMother2 = new Car(TestIRIs.INSTANCE_CAR, TestIRIs.INSTANCE_MOTHER);

		var vecs = strategy.getChangeVectors(carMother1, carMother2, true);

		assertTrue(vecs.isEmpty());
	}

	@Test
	void getChangeVectors_changedURIObjectProperty_vectorWithIdentifier() {
		var carMother = new Car(TestIRIs.INSTANCE_CAR, TestIRIs.INSTANCE_MOTHER);
		var carChild = new Car(TestIRIs.INSTANCE_CAR, TestIRIs.INSTANCE_CHILD);

		var vecs = strategy.getChangeVectors(carMother, carChild, true);

		vectorAssert(
			vecs,
			TestIRIs.CLASS_CAR,
			TestIRIs.INSTANCE_CAR,
			TestIRIs.PROPERTY_OBJECT_HAS_OWNER,
			URI.create(TestIRIs.INSTANCE_MOTHER)
		);
	}

	@Test
	void getChangeVectors_changedInferredPropertyOnly_noVectors() {
		var hero1 = new Hero(TestIRIs.INSTANCE_SUPERHERO, "Ron", "LaFlamme", true);
		var hero2 = new Hero(TestIRIs.INSTANCE_SUPERHERO, "Ron", "LaFlamme", false);

		var vecs = strategy.getChangeVectors(hero1, hero2, true);

		assertTrue(vecs.isEmpty());
	}


	@Test
	void getChangeVectors_changedIgnoredPropertyOnly_noVectors() {
		var hero1 = new Hero(TestIRIs.INSTANCE_SUPERHERO, "Ron", "LaFlamme", true);
		var hero2 = new Hero(TestIRIs.INSTANCE_SUPERHERO, "Ron", "Livingston", true);

		var vecs = strategy.getChangeVectors(hero1, hero2, true);

		assertTrue(vecs.isEmpty());
	}

	@Test
	void getChangeVectors_changedInferredIgnoredAndTrackedProperties_oneVector() {
		var hero1 = new Hero(TestIRIs.INSTANCE_SUPERHERO, "Ron", "LaFlamme", false);
		var hero2 = new Hero(TestIRIs.INSTANCE_SUPERHERO, "Ralph", "Livingston", true);

		var vecs = strategy.getChangeVectors(hero1, hero2, true);

		assertEquals(1, vecs.size());
		vectorAssert(vecs, TestIRIs.CLASS_SUPERHERO, TestIRIs.INSTANCE_SUPERHERO, TestIRIs.PROPERTY_FIRST_NAME, "Ron");
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
