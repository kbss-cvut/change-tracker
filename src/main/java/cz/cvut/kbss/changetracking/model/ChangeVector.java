package cz.cvut.kbss.changetracking.model;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import java.time.Instant;

/**
 * Change vector entity class, representing a single change to an audited object at a given timestamp.
 */
@Entity
public class ChangeVector {
  @Id
  @GeneratedValue
  private Long id;

  private Instant timestamp;

  private Object previousValue;

  private String attributeName;

  private String objectType;

  private String objectId;

  public ChangeVector(String objectType, String objectId, String attributeName, Object previousValue) {
    this.objectType = objectType;
    this.previousValue = previousValue;
    this.attributeName = attributeName;
    this.objectId = objectId;
    this.timestamp = Instant.now();
  }

  public ChangeVector() {
  }

  public Long getId() {
    return id;
  }

  public Instant getTimestamp() {
    return timestamp;
  }

  public Object getPreviousValue() {
    return previousValue;
  }

  public String getAttributeName() {
    return attributeName;
  }

  public String getObjectType() {
    return objectType;
  }

  public String getObjectId() {
    return objectId;
  }
}
