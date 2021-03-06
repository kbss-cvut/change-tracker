/**
 * Copyright (C) 2019 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.changetracking.testmodel;

import cz.cvut.kbss.changetracking.TestIRIs;
import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.Properties;

import java.io.Serializable;
import java.net.URI;
import java.util.Map;
import java.util.Set;

@OWLClass(iri = TestIRIs.CLASS_SUPERHERO)
@Audited
public class Superhero implements Serializable {
	@Id
	private URI uri;

	@OWLDataProperty(iri = TestIRIs.PROPERTY_FIRST_NAME)
	private String firstName;

	/**
	 * Contains properties not mapped by the object model.
	 */
	@Properties
	private Map<String, Set<String>> properties;

	public Superhero(String uri, String firstName, Map<String, Set<String>> properties) {
		this.uri = URI.create(uri);
		this.firstName = firstName;
		this.properties = properties;
	}

	public Superhero() {
	}

	public URI getUri() {
		return uri;
	}

	public String getFirstName() {
		return firstName;
	}

	public Map<String, Set<String>> getProperties() {
		return properties;
	}
}
