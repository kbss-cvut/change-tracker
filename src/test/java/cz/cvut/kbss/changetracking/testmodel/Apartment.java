package cz.cvut.kbss.changetracking.testmodel;

import cz.cvut.kbss.changetracking.TestIRIs;
import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

@OWLClass(iri = TestIRIs.CLASS_APARTMENT)
@Audited
public class Apartment extends Home {

	@OWLDataProperty(iri = TestIRIs.PROPERTY_HAS_BALCONY)
	protected Boolean hasBalcony;

	public Apartment(String uri, String city, Boolean hasBalcony) {
		super(uri, city);
		this.hasBalcony = hasBalcony;
	}

	public Apartment() {
	}

	public Boolean getHasBalcony() {
		return hasBalcony;
	}

	public void setHasBalcony(Boolean hasBalcony) {
		this.hasBalcony = hasBalcony;
	}

	@Override
	public String toString() {
		return "Apartment{" +
			"hasBalcony=" + hasBalcony +
			", uri=" + uri +
			", city='" + city + '\'' +
			'}';
	}
}
