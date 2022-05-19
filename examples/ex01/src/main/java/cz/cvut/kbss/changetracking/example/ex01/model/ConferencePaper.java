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
package cz.cvut.kbss.changetracking.example.ex01.model;

import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.Properties;

import java.net.URI;
import java.util.Map;
import java.util.Set;

@OWLClass(iri = "http://127.0.0.1/owl#ConferencePaper")
@Audited
public class ConferencePaper {
	@Id(generated = true)
	private URI uri;
	@OWLDataProperty(iri = "http://127.0.0.1/owl#name")
	private String name;
	@Properties
	private Map<String, Set<String>> properties;


	public ConferencePaper(String uriString, String name, Map<String, Set<String>> properties) {
		this.uri = URI.create(uriString);
		this.name = name;
		this.properties = properties;
	}

	public ConferencePaper() {
	}

	public ConferencePaper(ConferencePaper toCopy) {
		this.uri = toCopy.getUri();
		this.name = toCopy.getName();
		this.properties = toCopy.getProperties();
	}

	public URI getUri() {
		return uri;
	}

	public void setUri(URI uri) {
		this.uri = uri;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Map<String, Set<String>> getProperties() {
		return properties;
	}

	public void setProperties(Map<String, Set<String>> properties) {
		this.properties = properties;
	}

	@Override
	public String toString() {
		return "ConferencePaper{" +
			"uri=" + uri +
			", name='" + name + '\'' +
			", properties=" + properties +
			'}';
	}
}
