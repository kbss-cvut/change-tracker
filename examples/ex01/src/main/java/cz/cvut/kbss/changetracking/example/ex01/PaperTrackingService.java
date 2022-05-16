package cz.cvut.kbss.changetracking.example.ex01;

import cz.cvut.kbss.changetracking.ChangeTracker;
import cz.cvut.kbss.changetracking.example.ex01.model.ConferencePaper;
import cz.cvut.kbss.changetracking.model.ChangeVector;
import cz.cvut.kbss.changetracking.strategy.entity.JopaEntityStrategy;
import cz.cvut.kbss.changetracking.strategy.storage.JpaStorageStrategy;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.List;
import java.util.Objects;

@Service
public class PaperTrackingService {
	private final ChangeTracker tracker;
	private final EntityManager jopaEm;

	@Autowired
	public PaperTrackingService(EntityManager jopaEm, javax.persistence.EntityManager jpaEm) {
		this.jopaEm = jopaEm;
		this.tracker = new ChangeTracker(new JopaEntityStrategy(jopaEm.getMetamodel()), new JpaStorageStrategy(jpaEm));
	}

	/**
	 * Save both the (existing) entity and any change vectors created by diffing it against its current database-stored
	 * revision.
	 */
	@Transactional
	public void mergeAndCreateVectors(ConferencePaper newer) {
		var older = Objects.requireNonNull(jopaEm.find(ConferencePaper.class, newer.getUri()));
		jopaEm.merge(newer);
		tracker.compareAndSave(older, newer, SecurityUtils.getCurrentUserName());
	}

	// this method probably wouldn't be used in a real application
	public Collection<ChangeVector<?>> getChangeVectors(ConferencePaper older, ConferencePaper newer) {
		return tracker.compare(older, newer, SecurityUtils.getCurrentUserName());
	}

	@Transactional(readOnly = true)
	public List<ChangeVector<?>> getChangesForPaper(ConferencePaper paper) {
		return tracker.getAllForObject(
			ConferencePaper.class.getAnnotation(OWLClass.class).iri(),
			paper.getUri().toString()
		);
	}
}
