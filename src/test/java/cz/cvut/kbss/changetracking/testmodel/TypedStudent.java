package cz.cvut.kbss.changetracking.testmodel;

import cz.cvut.kbss.changetracking.TestIRIs;
import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.Types;

import java.net.URI;
import java.util.Set;

@OWLClass(iri = TestIRIs.CLASS_STUDENT)
@Audited
public class TypedStudent {
	@Id
	public URI uri;

	@Types
	private Set<String> types;

	public TypedStudent() {
	}

	public TypedStudent(String uri, Set<String> types) {
		this.uri = URI.create(uri);
		this.types = types;
	}

	public URI getUri() {
		return uri;
	}

	public Set<String> getTypes() {
		return types;
	}
}
