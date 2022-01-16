ChangeTracking
===

This is a domain object change tracking library. It is primarily intended to be used with
[JOPA](https://github.com/kbss-cvut/jopa) entities.

Usage
===

Requirements:
- Java 11+
- Maven 3.6+ recommended

1. Instantiate the change tracker with the default (shipped) strategies as follows:
```java
var tracker = new ChangeTracker(
	new JopaEntityStrategy(jopaMetamodel),
	new JpaStorageStrategy(jpaEntityManager)
);
```
2. When saving an entity, first get its old version and then call `tracker.compareAndSave(old, newer)`. Changes
   between the two revisions of the entity will be saved as change vectors in the database (interfaced with using
   the jpaEntityManager passed to JpaStorageStrategy). Don't forget to also save the entity itself!
3. When looking for changes, use one of the following methods:
    - `tracker.getAllForObject(String objectType, String objectId)`
    - `tracker.getChangesSince(Instant timestamp)`
    - `tracker.getChangesOfTypeSince(Instant timestamp, String objectType)`

Implementing custom strategies
===

Refer to the bundled Javadoc.

Compatibility
===

The JPA storage strategy has been tested with Spring Boot-provided EntityManagers running on top of PostgreSQL and
MariaDB. While MariaDB works out of the box, PostgreSQL requires
`spring.datasource.hikari.data-source-properties.stringtype` to be set to `unspecified` in `application.properties`.
This has to do with the internal mapping of unknown value types to JSON, which is in turn presented as a string to
the JPA provider. The solution stems from [a StackOverflow answer from 2017](https://stackoverflow.com/a/47550896).
