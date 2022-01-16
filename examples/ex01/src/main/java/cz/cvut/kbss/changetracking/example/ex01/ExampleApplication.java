package cz.cvut.kbss.changetracking.example.ex01;

import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import java.util.Collections;

@SpringBootApplication
public class ExampleApplication {
	public static void main(String[] args) {
		System.out.println("initing jpf");
		JopaPersistenceFactory.init(Collections.singletonMap(
			JOPAPersistenceProperties.SCAN_PACKAGE,
			"cz.cvut.kbss.changetracking.example.ex01.model"
		));
		SpringApplication.run(ExampleApplication.class, args);
	}
}
