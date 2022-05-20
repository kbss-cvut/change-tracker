 package cz.cvut.kbss.changetracking.example.ex01;

 import com.github.ledsoft.jopa.spring.transaction.DelegatingEntityManager;
 import com.github.ledsoft.jopa.spring.transaction.JopaTransactionManager;
 import cz.cvut.kbss.jopa.model.EntityManagerFactory;
 import org.springframework.boot.test.context.TestConfiguration;
 import org.springframework.context.annotation.Bean;
 import org.springframework.orm.jpa.JpaTransactionManager;
 import org.springframework.transaction.PlatformTransactionManager;
 import org.springframework.transaction.annotation.EnableTransactionManagement;

@TestConfiguration
@EnableTransactionManagement
public class TestPersistenceConfig {
	@Bean("delegatingJopaEM")
	public DelegatingEntityManager entityManager() {
		return new DelegatingEntityManager();
	}

	@Bean
	public PlatformTransactionManager transactionManager(EntityManagerFactory emf, DelegatingEntityManager emProxy) {
		return new JopaTransactionManager(emf, emProxy);
	}

	@Bean("jpaTxManager")
	public PlatformTransactionManager jpaTxManager() {
		return new JpaTransactionManager();
	}
}
