package cz.cvut.kbss.changetracking.model;

import cz.cvut.kbss.changetracking.TestIRIs;
import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.Inferred;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.io.Serializable;
import java.net.URI;

@OWLClass(iri = TestIRIs.CLASS_SUPERHERO)
@Audited
public class Hero implements Serializable {
	@Id
	private URI uri;

	@OWLDataProperty(iri = TestIRIs.PROPERTY_FIRST_NAME)
	private String firstName;

	@OWLDataProperty(iri = TestIRIs.PROPERTY_GOOD_GUY)
	@Inferred
	private Boolean goodGuy;

	public Hero(String uri, String firstName, boolean goodGuy) {
		this.uri = URI.create(uri);
		this.firstName = firstName;
		this.goodGuy = goodGuy;
	}

	public Hero() {
	}

	public URI getUri() {
		return uri;
	}

	public String getFirstName() {
		return firstName;
	}

	public boolean isGoodGuy() {
		return goodGuy;
	}
}
