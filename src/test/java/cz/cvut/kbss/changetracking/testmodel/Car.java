package cz.cvut.kbss.changetracking.testmodel;

import cz.cvut.kbss.changetracking.TestIRIs;
import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;

@OWLClass(iri = TestIRIs.CLASS_CAR)
@Audited
public class Car {
	@Id(generated = true)
	protected URI uri;

	@OWLObjectProperty(iri = TestIRIs.PROPERTY_OBJECT_HAS_OWNER)
	@ParticipationConstraints({
		@ParticipationConstraint(owlObjectIRI = TestIRIs.CLASS_PERSON)
	})
	protected URI owner;

	public Car() {
	}

	public Car(String uri, String owner) {
		this.uri = URI.create(uri);
		this.owner = URI.create(owner);
	}
}
