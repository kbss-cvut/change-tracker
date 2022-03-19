package cz.cvut.kbss.changetracking.model;

import cz.cvut.kbss.changetracking.TestIRIs;
import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.net.URI;

@OWLClass(iri = TestIRIs.CLASS_STUDENT)
@Audited
public class UndergraduateStudent {

	@Id
	public URI uri;

	@OWLDataProperty(iri = TestIRIs.PROPERTY_FIRST_NAME)
	public String firstName;

	@OWLDataProperty(iri = TestIRIs.PROPERTY_LAST_NAME)
	public String lastName;

	public UndergraduateStudent(String uri, String firstName, String lastName) {
		this.uri = URI.create(uri);
		this.firstName = firstName;
		this.lastName = lastName;
	}

	public UndergraduateStudent() {
	}
}
