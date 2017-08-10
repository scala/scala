package scala.reflect.internal.util;

/**
 * Represents a container with a boolean value that tells the compiler whether
 * an option is enabled or not. This class is used for configuration purposes
 * (see scala.reflect.internal.util.Statistics).
 */
class BooleanContainer {
  private final boolean value;

  public BooleanContainer(boolean value) {
    this.value = value;
  }
  
  public boolean isEnabledNow() {
    return value;
  }

  protected final static class TrueContainer extends BooleanContainer {
    TrueContainer() {
        super(true);
    }
  }

  protected final static class FalseContainer extends BooleanContainer {
    FalseContainer() {
        super(false);
    }
  }
}