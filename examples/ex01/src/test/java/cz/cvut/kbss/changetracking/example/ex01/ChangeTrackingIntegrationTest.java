package cz.cvut.kbss.changetracking.example.ex01;

import cz.cvut.kbss.changetracking.example.ex01.model.ConferencePaper;
import cz.cvut.kbss.jopa.model.EntityManager;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@SpringBootTest
@EntityScan("cz.cvut.kbss.changetracking.model")
public class ChangeTrackingIntegrationTest {
	static final String IS_NICE_TO_READ = "http://127.0.0.1/owl#isNiceToRead";

	@Autowired PaperTrackingService service;
	@Autowired EntityManager jopaEm;

	@Test
	void x() {
		var props = new HashMap<String, Set<String>>();
		props.put(IS_NICE_TO_READ, Collections.singleton(Boolean.FALSE.toString()));
		var paper1 = new ConferencePaper(
			"http://www.conference001.org/2021/paper1",
			"Technology behing JOPA",
			Map.copyOf(props)
		);
		jopaEm.persist(paper1);

		assertTrue(service.getChangesForPaper(paper1).isEmpty());

		// try to find changes where the are none
		service.mergeAndCreateVectors(paper1);
		assertTrue(service.getChangesForPaper(paper1).isEmpty());

		// make a change in properties
		props.replace("http://127.0.0.1/owl#isNiceToRead", Collections.singleton(Boolean.TRUE.toString()));
		service.mergeAndCreateVectors(paper1);
		assertEquals(1, service.getChangesForPaper(paper1).size());



	}
}
