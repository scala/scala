package scala.reflect.internal.util;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.invoke.MutableCallSite;
import java.lang.invoke.SwitchPoint;

/**
 * Represents a value that is wrapped with JVM machinery to allow the JVM
 * to speculate on its content and effectively optimize it as if it was final.
 * 
 * This file has been drawn from JSR292 cookbook created by Rémi Forax.
 * https://code.google.com/archive/p/jsr292-cookbook/. The explanation of the strategy
 * can be found in https://community.oracle.com/blogs/forax/2011/12/17/jsr-292-goodness-almost-static-final-field.
 * 
 * Before copying this file to the repository, I tried to adapt the most important
 * parts of this implementation and special case it for `Statistics`, but that
 * caused an important performance penalty (~10%). This performance penalty is
 * due to the fact that using `static`s for the method handles and all the other
 * fields is extremely important for the JVM to correctly optimize the code, and
 * we cannot do that if we make `Statistics` an object extending `MutableCallSite`
 * in Scala. We instead rely on the Java implementation that uses a boxed representation.
 */
public class AlmostFinalValue {
  private final AlmostFinalCallSite callsite =
      new AlmostFinalCallSite(this);
  
  protected boolean initialValue() {
    return false;
  }
  
  public MethodHandle createGetter() {
    return callsite.dynamicInvoker();
  }
  
  public void setValue(boolean value) {
    callsite.setValue(value);
  }
  
  private static class AlmostFinalCallSite extends MutableCallSite {
    private Boolean value;
    private SwitchPoint switchPoint;
    private final AlmostFinalValue volatileFinalValue;
    private final MethodHandle fallback;
    private final Object lock;
    
    private static final Boolean NONE = null;
    private static final MethodHandle FALLBACK;
    static {
      try {
        FALLBACK = MethodHandles.lookup().findVirtual(AlmostFinalCallSite.class, "fallback",
            MethodType.methodType(Boolean.TYPE));
      } catch (NoSuchMethodException|IllegalAccessException e) {
        throw new AssertionError(e.getMessage(), e);
      }
    }
    
    AlmostFinalCallSite(AlmostFinalValue volatileFinalValue) {
      super(MethodType.methodType(Boolean.TYPE));
      Object lock = new Object();
      MethodHandle fallback = FALLBACK.bindTo(this);
      synchronized(lock) {
        value = null;
        switchPoint = new SwitchPoint();
        setTarget(fallback);
      }
      this.volatileFinalValue = volatileFinalValue;
      this.lock = lock;
      this.fallback = fallback;
    }

    boolean fallback() {
      synchronized(lock) {
        Boolean value = this.value;
        if (value == NONE) {
          value = volatileFinalValue.initialValue();
        }
        MethodHandle target = switchPoint.guardWithTest(MethodHandles.constant(Boolean.TYPE, value), fallback);
        setTarget(target);
        return value;
      }
    }
    
    void setValue(boolean value) {
      synchronized(lock) {
        SwitchPoint switchPoint = this.switchPoint;
        this.value = value;
        this.switchPoint = new SwitchPoint();
        SwitchPoint.invalidateAll(new SwitchPoint[] {switchPoint});
      }
    }
  }
}