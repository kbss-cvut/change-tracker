package cz.cvut.kbss.changetracking.model;

import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.net.URI;

@OWLClass(iri = Home.HOME_CLASS_IRI)
@Audited
public class Home {
	public static final String HOME_CLASS_IRI = "http://127.0.0.1/owl#Home";
	@Id(generated = true)
	protected URI uri;

	@OWLDataProperty(iri = "http://127.0.0.1/owl#city")
	protected String city;

	public Home(String uri, String city) {
		this.uri = URI.create(uri);
		this.city = city;
	}

	public Home() {
	}

	public URI getUri() {
		return uri;
	}

	public void setUri(URI uri) {
		this.uri = uri;
	}

	public String getCity() {
		return city;
	}

	public void setCity(String city) {
		this.city = city;
	}

	@Override
	public String toString() {
		return "Home{" + "uri=" + uri + ", city='" + city + '\'' + '}';
	}
}
