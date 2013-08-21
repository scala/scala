/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Grzegorz Kossakowski
 */

package scala.tools.partest.javaagent;

import java.lang.instrument.Instrumentation;
import java.lang.instrument.UnmodifiableClassException;

/**
 * Profiling agent that instruments byte-code to insert calls to
 * {@link scala.tools.partest.instrumented.Profiler#methodCalled(String, String, String)}
 * by using ASM library for byte-code manipulation.
 */
public class ProfilingAgent {
        public static void premain(String args, Instrumentation inst) throws UnmodifiableClassException {
          // NOTE: we are adding transformer that won't be applied to classes that are already loaded
          // This should be ok because premain should be executed before main is executed so Scala library
          // and the test-case itself won't be loaded yet. We rely here on the fact that ASMTransformer does
          // not depend on Scala library. In case our assumptions are wrong we can always insert call to
          // inst.retransformClasses.
          inst.addTransformer(new ASMTransformer(), false);
        }
}
