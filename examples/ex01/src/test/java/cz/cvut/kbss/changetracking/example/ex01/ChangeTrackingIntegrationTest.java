package cz.cvut.kbss.changetracking.example.ex01;

import cz.cvut.kbss.changetracking.example.ex01.model.ConferencePaper;
import cz.cvut.kbss.changetracking.model.ChangeVector;
import cz.cvut.kbss.jopa.model.EntityManager;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallbackWithoutResult;
import org.springframework.transaction.support.TransactionTemplate;

import java.time.Instant;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Due to JOPA and JPA both 'exposing' a transactional interface, including a transaction manager, we have to
 * carefully manage the transaction manager currently in use. Here, we are demonstrating a relatively simple
 * solution, which consists of defining two {@link PlatformTransactionManager} beans, one for each persistence
 * platform. Operations requiring a transaction are then executed in a lambda with the correct transaction manager
 * bound to it (executing the lambda in a {@link TransactionTemplate}, which is created with the appropriate manager).
 */
@SpringBootTest(classes = {TestPersistenceConfig.class})
@EntityScan("cz.cvut.kbss.changetracking.model")
public class ChangeTrackingIntegrationTest {
	static final String IS_NICE_TO_READ = "http://127.0.0.1/owl#isNiceToRead";

	@Autowired PaperTrackingService service;
	@Autowired @Qualifier("entityManager") EntityManager jopaEm;

	@Autowired
	@Qualifier("transactionManager")
	PlatformTransactionManager jopaTxManager;

	@Autowired
	@Qualifier("jpaTxManager")
	PlatformTransactionManager jpaTxManager;

	void tx(PlatformTransactionManager manager, Runnable runnable) {
		new TransactionTemplate(manager).execute(new TransactionCallbackWithoutResult() {
			@Override
			protected void doInTransactionWithoutResult(@NotNull TransactionStatus status) {
				runnable.run();
			}
		});
	}

	@Test
	void exampleLifecycleTest() {
		var props1 = new HashMap<String, Set<String>>();
		props1.put(IS_NICE_TO_READ, Collections.singleton(Boolean.FALSE.toString()));
		var paper = new ConferencePaper(
			"http://www.conference001.org/2021/paper1",
			"Technology behing JOPA",
			Map.copyOf(props1)
		);
		tx(jopaTxManager, () -> jopaEm.persist(paper));

		assertTrue(service.getChangesForPaper(paper).isEmpty());

		// try to find changes where the are none
		tx(jpaTxManager, () -> service.saveChanges(paper));
		assertTrue(service.getChangesForPaper(paper).isEmpty());

		// detach the entity
		jopaEm.detach(paper);

		// make a change in properties - may come in the form of a client request, for example
		var props2 = new HashMap<>(props1);
		props2.replace(IS_NICE_TO_READ, Collections.singleton(Boolean.TRUE.toString()));
		paper.setProperties(props2);
		tx(jpaTxManager, () -> service.saveChanges(paper));
		tx(jopaTxManager, () -> jopaEm.merge(paper));

		var changes1 = service.getChangesForPaper(paper);
		assertEquals(1, changes1.size());
		var vector1 = changes1.get(0);
		assertEquals(SecurityUtils.getCurrentUserName(), vector1.getAuthorId());
		assertTrue(vector1.getTimestamp().isBefore(Instant.now()));
		assertEquals(IS_NICE_TO_READ, vector1.getAttributeName());

		// now, replace the document with a new instance without properties
		var paper2 = new ConferencePaper("http://www.conference001.org/2021/paper1", "Technology behind JOPA", null);
		tx(jpaTxManager, () -> service.saveChanges(paper2));
		tx(jopaTxManager, () -> jopaEm.merge(paper2));

		var changes2 = service.getChangesForPaper(paper);
		assertEquals(3, changes2.size());
		assertEquals(vector1, changes2.get(2)); // will be last - timestamp desc order
		var vector2a = changes2.get(0);
		var vector2b = changes2.get(1);
		assertTrue(vector2a.getTimestamp().isAfter(vector1.getTimestamp()));
		ChangeVector<?> nameVector, propVector;
		if (vector2a.getAttributeName().equals("http://127.0.0.1/owl#name")) {
			// the vector order is not guaranteed as long as they're part of one update
			nameVector = vector2a;
			propVector = vector2b;
		} else {
			nameVector = vector2b;
			propVector = vector2a;
		}
		assertEquals("Technology behing JOPA", nameVector.getPreviousValue());
		assertEquals(Collections.singleton(Boolean.TRUE.toString()), propVector.getPreviousValue());
		assertEquals(IS_NICE_TO_READ, propVector.getAttributeName());

		// now check if empty value was saved successfully for the prop
		var paper3 = new ConferencePaper(paper2);
		var props3 = new HashMap<String, Set<String>>();
		props3.put(IS_NICE_TO_READ, Collections.singleton(Boolean.TRUE.toString()));
		paper3.setProperties(props3);
		tx(jpaTxManager, () -> service.saveChanges(paper3));
		tx(jopaTxManager, () -> jopaEm.merge(paper3));

		var changes3 = service.getChangesForPaper(paper);
		assertEquals(4, changes3.size());
		assertEquals(IS_NICE_TO_READ, changes3.get(0).getAttributeName());
		assertNull(changes3.get(0).getPreviousValue());
	}
}
