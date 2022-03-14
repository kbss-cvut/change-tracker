package cz.cvut.kbss.changetracking.model;

import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

@OWLClass(iri = House.HOUSE_CLASS_IRI)
@Audited
public class House extends Home {
	public static final String HOUSE_CLASS_IRI = "http://127.0.0.1/owl#House";

	@OWLDataProperty(iri = "http://127.0.0.1/owl#floors")
	private Integer floors;

	public House(String uri, String name, Integer floors) {
		super(uri, name);
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
