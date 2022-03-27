package cz.cvut.kbss.changetracking.model;

import cz.cvut.kbss.changetracking.TestIRIs;
import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

@OWLClass(iri = TestIRIs.CLASS_HOUSE)
@Audited
public class House extends Home {

	@OWLDataProperty(iri = TestIRIs.PROPERTY_FLOORS)
	private Integer floors;

	public House(String uri, String city, Integer floors) {
		super(uri, city);
		this.floors = floors;
	}

	public House() {
	}

	public Integer getFloors() {
		return floors;
	}

	public void setFloors(Integer floors) {
		this.floors = floors;
	}

	@Override
	public String toString() {
		return "House{" +
			"uri=" + uri +
			", city='" + city + '\'' +
			", floors='" + floors + '\'' +
			'}';
	}
}
