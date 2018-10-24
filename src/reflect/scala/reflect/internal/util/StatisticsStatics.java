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

import scala.reflect.internal.util.AlmostFinalValue;
import java.lang.invoke.MethodHandle;

/**
 * Represents all the simulated statics for Statistics.
 * 
 * Its implementation delegates to {@link scala.reflect.internal.util.AlmostFinalValue},
 * which helps performance (see docs to find out why).
 */
public final class StatisticsStatics {
  private static final AlmostFinalValue COLD_STATS = new AlmostFinalValue() {
    @Override
    protected boolean initialValue() {
        return false;
    }
  };

  private static final AlmostFinalValue HOT_STATS = new AlmostFinalValue() {
    @Override
    protected boolean initialValue() {
        return false;
    }
  };

  private static final MethodHandle COLD_STATS_GETTER = COLD_STATS.createGetter();
  private static final MethodHandle HOT_STATS_GETTER = HOT_STATS.createGetter();
  
  public static boolean areSomeColdStatsEnabled() throws Throwable {
    return (boolean) COLD_STATS_GETTER.invokeExact();
  }

  public static boolean areSomeHotStatsEnabled() throws Throwable {
    return (boolean) HOT_STATS_GETTER.invokeExact();
  }

  public static void enableColdStats() throws Throwable {
    if (!areSomeColdStatsEnabled())
      COLD_STATS.setValue(true);
  }

  public static void disableColdStats() {
    COLD_STATS.setValue(false);
  }

  public static void enableHotStats() throws Throwable {
    if (!areSomeHotStatsEnabled())
      HOT_STATS.setValue(true);
  }

  public static void disableHotStats() {
    HOT_STATS.setValue(false);
  }
}
