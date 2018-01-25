package scala.reflect.internal.util;

import scala.reflect.internal.util.AlmostFinalValue;
import java.lang.invoke.MethodHandle;

/**
 * Represents all the simulated statics for Statistics.
 * 
 * Its implementation delegates to {@link scala.reflect.internal.util.AlmostFinalValue},
 * which helps performance (see docs to find out why).
 */
public final class StatisticsStatics extends BooleanContainer {
  public StatisticsStatics(boolean value) {
    super(value);
  }

  private static final AlmostFinalValue<BooleanContainer> COLD_STATS = new AlmostFinalValue<BooleanContainer>() {
    @Override
    protected BooleanContainer initialValue() {
        return new FalseContainer();
    }
  };

  private static final AlmostFinalValue<BooleanContainer> HOT_STATS = new AlmostFinalValue<BooleanContainer>() {
    @Override
    protected BooleanContainer initialValue() {
        return new FalseContainer();
    }
  };

  private static final MethodHandle COLD_STATS_GETTER = COLD_STATS.createGetter();
  private static final MethodHandle HOT_STATS_GETTER = HOT_STATS.createGetter();
  
  public static boolean areSomeColdStatsEnabled() {
    try {
      return ((BooleanContainer)(Object) COLD_STATS_GETTER.invokeExact()).isEnabledNow();
    } catch (Throwable e) {
      throw new AssertionError(e.getMessage(), e);
    }
  }

  public static boolean areSomeHotStatsEnabled() {
    try {
      return ((BooleanContainer)(Object) HOT_STATS_GETTER.invokeExact()).isEnabledNow();
    } catch (Throwable e) {
      throw new AssertionError(e.getMessage(), e);
    }
  }

  public static void enableColdStats() {
    if (!areSomeColdStatsEnabled())
      COLD_STATS.setValue(new TrueContainer());
  }

  public static void disableColdStats() {
    COLD_STATS.setValue(new FalseContainer());
  }

  public static void enableHotStats() {
    if (!areSomeHotStatsEnabled())
      HOT_STATS.setValue(new TrueContainer());
  }

  public static void disableHotStats() {
    HOT_STATS.setValue(new FalseContainer());
  }
}