package cz.cvut.kbss.changetracking.testmodel;

import cz.cvut.kbss.changetracking.TestIRIs;
import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

@OWLClass(iri = TestIRIs.CLASS_LOFT)
@Audited
public class Loft extends Apartment {
	@OWLDataProperty(iri = TestIRIs.PROPERTY_RAILINGS)
	private Integer railings;

	public Loft(String uri, String city, Boolean hasBalcony, Integer railings) {
		super(uri, city, hasBalcony);
		this.railings = railings;
	}

	public Loft() {
	}

	public Integer getRailings() {
		return railings;
	}

	public void setRailings(Integer railings) {
		this.railings = railings;
	}

	@Override
	public String toString() {
		return "Loft{" +
			"hasBalcony=" + hasBalcony +
			", uri=" + uri +
			", city='" + city + '\'' +
			", railings=" + railings +
			'}';
	}
}
