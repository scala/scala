/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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