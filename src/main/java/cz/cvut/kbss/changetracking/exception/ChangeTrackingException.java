package cz.cvut.kbss.changetracking.exception;

/**
 * {link {@link ChangeTrackingException} is the change tracker's specific {@link RuntimeException}.
 * <p>
 * All exceptions thrown by the change tracker should inherit from this class.
 */
public class ChangeTrackingException extends RuntimeException {
  public ChangeTrackingException(String s, Throwable cause) {
    super(s, cause);
  }

  public ChangeTrackingException(String s) {
    super(s);
  }
}
