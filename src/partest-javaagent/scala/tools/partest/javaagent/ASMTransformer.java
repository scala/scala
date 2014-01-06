/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Grzegorz Kossakowski
 */

package scala.tools.partest.javaagent;

import java.lang.instrument.ClassFileTransformer;
import java.security.ProtectionDomain;

import scala.tools.asm.ClassReader;
import scala.tools.asm.ClassWriter;

public class ASMTransformer implements ClassFileTransformer {

  private boolean shouldTransform(String className) {
    return
        // do not instrument instrumentation logic (in order to avoid infinite recursion)
        !className.startsWith("scala/tools/partest/instrumented/") &&
        !className.startsWith("scala/tools/partest/javaagent/") &&
        // we instrument all classes from empty package
        (!className.contains("/") ||
        // we instrument all classes from scala package
        className.startsWith("scala/") ||
        // we instrument all classes from `instrumented` package
        className.startsWith("instrumented/"));
  }

        public byte[] transform(final ClassLoader classLoader, final String className, Class<?> classBeingRedefined, ProtectionDomain protectionDomain, byte[] classfileBuffer) {
          if (shouldTransform(className)) {
            ClassWriter writer = new ClassWriter(ClassWriter.COMPUTE_MAXS) {
              @Override protected String getCommonSuperClass(final String type1, final String type2) {
                // Since we are not recomputing stack frame map, this should never be called we override this method because
                // default implementation uses reflection for implementation and might try to load the class that we are
                // currently processing. That leads to weird results like swallowed exceptions and classes being not
                // transformed.
                throw new RuntimeException("Unexpected call to getCommonSuperClass(" + type1 + ", " + type2 +
                    ") while transforming " + className);
              }
            };
                ProfilerVisitor visitor = new ProfilerVisitor(writer);
                ClassReader reader = new ClassReader(classfileBuffer);
                reader.accept(visitor, 0);
                return writer.toByteArray();
          } else {
            return classfileBuffer;
          }
        }
}
