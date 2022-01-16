package cz.cvut.kbss.changetracking.example.ex01;

import cz.cvut.kbss.changetracking.ChangeTracker;
import cz.cvut.kbss.changetracking.example.ex01.model.ConferencePaper;
import cz.cvut.kbss.changetracking.strategy.entity.JopaEntityStrategy;
import cz.cvut.kbss.changetracking.strategy.storage.JpaStorageStrategy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.PreDestroy;
import javax.persistence.EntityManager;
import java.time.Instant;

@Component
@EntityScan("cz.cvut.kbss.changetracking.model")
public class ExampleRunner implements CommandLineRunner {
	private final ChangeTracker changeTracker;


	@Autowired
	public ExampleRunner(ChangeTracker changeTracker) {
		this.changeTracker = changeTracker;
	}

	@Bean
	public static ChangeTracker changeTracker(EntityManager jpaEm) {
		var jopaEm = JopaPersistenceFactory.createEntityManager();
		return new ChangeTracker(new JopaEntityStrategy(jopaEm.getMetamodel()), new JpaStorageStrategy(jpaEm));
	}

	@Override
	@Transactional
	public void run(String... args) {
		// TODO: JOPA integration

		var paper1 = new ConferencePaper("http://www.conference001.org/2021/paper1", "Technology behing JOPA");
		var paper2 = new ConferencePaper(paper1);
		paper2.setName("Technology behind JOPA");
		changeTracker.compareAndSave(paper1, paper2);

		System.out.println("done saving");

		var vector = changeTracker.getChangesSince(Instant.EPOCH).get(0);

		System.out.println(vector.getPreviousValue());
	}

	@PreDestroy
	public void shutdown() {
		JopaPersistenceFactory.close();
	}
}
