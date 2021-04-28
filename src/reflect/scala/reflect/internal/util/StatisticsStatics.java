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

import java.lang.invoke.MethodHandle;

/**
 * Represents all the simulated statics for Statistics.
 * 
 * Its implementation delegates to {@link scala.reflect.internal.util.AlmostFinalValue},
 * which helps performance (see docs to find out why).
 */
public final class StatisticsStatics {
  private static final AlmostFinalValue COLD_STATS = new AlmostFinalValue();
  private static final AlmostFinalValue  HOT_STATS = new AlmostFinalValue();
  private static final AlmostFinalValue DEBUG      = new AlmostFinalValue();
  private static final AlmostFinalValue DEVELOPER  = new AlmostFinalValue();

  private static final MethodHandle COLD_STATS_GETTER = COLD_STATS.invoker;
  private static final MethodHandle  HOT_STATS_GETTER =  HOT_STATS.invoker;
  private static final MethodHandle      DEBUG_GETTER =      DEBUG.invoker;
  private static final MethodHandle  DEVELOPER_GETTER =  DEVELOPER.invoker;

  public static boolean areSomeColdStatsEnabled() throws Throwable { return (boolean) COLD_STATS_GETTER.invokeExact(); }
  public static boolean areSomeHotStatsEnabled()  throws Throwable { return (boolean)  HOT_STATS_GETTER.invokeExact(); }
  public static boolean isDebug()                 throws Throwable { return (boolean)      DEBUG_GETTER.invokeExact(); }
  public static boolean isDeveloper()             throws Throwable { return (boolean)  DEVELOPER_GETTER.invokeExact(); }

  public static void enableColdStatsAndDeoptimize() { COLD_STATS.toggleOnAndDeoptimize(); }
  public static void  enableHotStatsAndDeoptimize() {  HOT_STATS.toggleOnAndDeoptimize(); }
  public static void     enableDebugAndDeoptimize() {      DEBUG.toggleOnAndDeoptimize(); }
  public static void enableDeveloperAndDeoptimize() {  DEVELOPER.toggleOnAndDeoptimize(); }
}
