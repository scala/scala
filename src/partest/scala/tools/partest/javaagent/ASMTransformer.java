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
	
	public byte[] transform(final ClassLoader classLoader, String className, Class<?> classBeingRedefined, ProtectionDomain protectionDomain, byte[] classfileBuffer) {
	  if (shouldTransform(className)) {
  		ClassWriter writer = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS) {
                        // this is copied verbatim from the superclass,
                        // except that we use the outer class loader
                        @Override protected String getCommonSuperClass(final String type1, final String type2) {
                            Class<?> c, d;
                            try {
                                c = Class.forName(type1.replace('/', '.'), false, classLoader);
                                d = Class.forName(type2.replace('/', '.'), false, classLoader);
                            } catch (Exception e) {
                                throw new RuntimeException(e.toString());
                            }
                            if (c.isAssignableFrom(d)) {
                                return type1;
                            }
                            if (d.isAssignableFrom(c)) {
                                return type2;
                            }
                            if (c.isInterface() || d.isInterface()) {
                                return "java/lang/Object";
                            } else {
                                do {
                                    c = c.getSuperclass();
                                } while (!c.isAssignableFrom(d));
                                return c.getName().replace('.', '/');
                            }
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
