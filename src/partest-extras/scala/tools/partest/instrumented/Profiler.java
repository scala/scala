/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Grzegorz Kossakowski
 */

package scala.tools.partest.instrumented;

import java.util.HashMap;
import java.util.Map;

/**
 * A simple profiler class that counts method invocations. It is being used in byte-code instrumentation by inserting
 * call to {@link Profiler#methodCalled(String, String, String)} at the beginning of every instrumented class.
 *
 * WARNING: This class is INTERNAL implementation detail and should never be used directly. It's made public only
 * because it must be universally accessible for instrumentation needs. If you want to profile your test use
 * {@link Instrumentation} instead.
 */
public class Profiler {

        private static boolean isProfiling = false;
        private static Map<MethodCallTrace, Integer> counts = new HashMap<MethodCallTrace, Integer>();

        static public class MethodCallTrace {
          final String className;
          final String methodName;
          final String methodDescriptor;

          public MethodCallTrace(final String className, final String methodName, final String methodDescriptor) {
            this.className = className;
            this.methodName = methodName;
            this.methodDescriptor = methodDescriptor;
          }

          @Override
          public boolean equals(Object obj) {
            if (!(obj instanceof MethodCallTrace)) {
              return false;
            } else {
              MethodCallTrace that = (MethodCallTrace) obj;
              return that.className.equals(className) && that.methodName.equals(methodName) && that.methodDescriptor.equals(methodDescriptor);
            }
          }
          @Override
          public int hashCode() {
            return className.hashCode() ^ methodName.hashCode() ^ methodDescriptor.hashCode();
          }
        }

        public static void startProfiling() {
                isProfiling = true;
        }

        public static void stopProfiling() {
                isProfiling = false;
        }

        public static boolean isProfiling() {
          return isProfiling;
        }

        public static void resetProfiling() {
          counts = new HashMap<MethodCallTrace, Integer>();
        }

        public static void methodCalled(final String className, final String methodName, final String methodDescriptor) {
                if (isProfiling) {
                  MethodCallTrace trace = new MethodCallTrace(className, methodName, methodDescriptor);
                        Integer counter = counts.get(trace);
                        if (counter == null) {
                                counts.put(trace, 1);
                        } else {
                                counts.put(trace, counter+1);
                        }
                }
        }

        public static Map<MethodCallTrace, Integer> getStatistics() {
          return new HashMap<MethodCallTrace, Integer>(counts);
        }

}
