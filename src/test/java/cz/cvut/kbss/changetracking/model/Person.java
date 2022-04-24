package cz.cvut.kbss.changetracking.model;

import cz.cvut.kbss.changetracking.TestIRIs;
import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@OWLClass(iri = TestIRIs.CLASS_PERSON)
@Audited
public class Person {
	public Person(String uri, String lastName, Person mother) {
		this.uri = URI.create(uri);
		this.lastName = lastName;
		this.mother = mother;
		this.children = new HashSet<>();
	}

	public Person(Person person) {
		this.uri = person.uri;
		this.lastName = person.lastName;
		this.mother = person.mother;
		this.children = new HashSet<>(person.children);
	}

	public Person() {
	}

	@Id(generated = true)
	protected URI uri;

	@OWLDataProperty(iri = TestIRIs.PROPERTY_LAST_NAME)
	protected String lastName;

	@OWLObjectProperty(iri = TestIRIs.PROPERTY_OBJECT_HAS_MOTHER)
	@ParticipationConstraints({
		@ParticipationConstraint(owlObjectIRI = TestIRIs.CLASS_PERSON, min = 1, max = 1)
	})
	protected Person mother;

	@OWLObjectProperty(iri = TestIRIs.PROPERTY_OBJECT_HAS_CHILD)
	protected Set<Person> children;

	public void addChildren(Person... person) {
		children.addAll(List.of(person));
	}
}
