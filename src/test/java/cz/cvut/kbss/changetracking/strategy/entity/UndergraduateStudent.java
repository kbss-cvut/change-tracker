package cz.cvut.kbss.changetracking.strategy.entity;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.net.URI;

@OWLClass(iri = JopaEntityStrategyTest.studentClassIri)
public class UndergraduateStudent {

	@Id
	public URI uri;

	@OWLDataProperty(iri = "http://uob.iodt.ibm.com/univ-bench-dl.owl#firstName")
	public String firstName;

	@OWLDataProperty(iri = "http://uob.iodt.ibm.com/univ-bench-dl.owl#lastName")
	public String lastName;

	public UndergraduateStudent(String uri, String firstName, String lastName) {
		this.uri = URI.create(uri);
		this.firstName = firstName;
		this.lastName = lastName;
	}

	public UndergraduateStudent() {
	}
}
