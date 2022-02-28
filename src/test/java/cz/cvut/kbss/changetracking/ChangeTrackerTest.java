package cz.cvut.kbss.changetracking;

import cz.cvut.kbss.changetracking.annotation.Audited;
import cz.cvut.kbss.changetracking.exception.ObjectsNotCompatibleException;
import cz.cvut.kbss.changetracking.strategy.entity.EntityStrategy;
import cz.cvut.kbss.changetracking.strategy.storage.JpaStorageStrategy;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import org.junit.jupiter.api.Test;

import java.net.URI;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;

public class ChangeTrackerTest {
	static EntityStrategy<?> entityStrategy = mock(EntityStrategy.class);
	static ChangeTracker changeTracker = new ChangeTracker(entityStrategy, mock(JpaStorageStrategy.class));


	@Test
	void compare_twoDifferentClasses_throwsObjectsNotCompatibleException() {
		OwlAuditedCourse course = new OwlAuditedCourse();
		OwlAuditedPaper paper = new OwlAuditedPaper();

		assertThrows(ObjectsNotCompatibleException.class, () -> changeTracker.compare(course, paper));
	}

	@OWLClass(iri = "http://uob.iodt.ibm.com/univ-bench-dl.owl#Course")
	@Audited
	static class OwlAuditedCourse {
		@Id
		private URI uri;

		@OWLDataProperty(iri = "http://uob.iodt.ibm.com/univ-bench-dl.owl#name")
		private String name;
	}

	@OWLClass(iri = "http://uob.iodt.ibm.com/univ-bench-dl.owl#ConferencePaper")
	@Audited
	static class OwlAuditedPaper {
		@Id
		private URI uri;

		@OWLDataProperty(iri = "http://uob.iodt.ibm.com/univ-bench-dl.owl#name")
		private String name;
	}
}
