//> using jvm 11+
//> using options -release:11

import java.lang.invoke._, MethodType.methodType

object Test {
  def main(args: Array[String]): Unit = {
    val l = MethodHandles.lookup()
    val mhCL = l.findStatic(classOf[ClassLoader], "getPlatformClassLoader", methodType(classOf[ClassLoader]))
    // `invoke` and `invokeExact` are both signature polymorphic
    assert(null != (mhCL.invoke(): ClassLoader))
    assert(null != (mhCL.invoke().asInstanceOf[ClassLoader]: ClassLoader))
    assert(null != (mhCL.invokeExact(): ClassLoader))
    // I've commented out this part of the Dotty test because here in Scala 2,
    // we didn't implement specifying a signature polymorphic method's return type
    // via `asInstanceOf`; we only implemented specifying it via type ascription
    // assert(null != (mhCL.invokeExact().asInstanceOf[ClassLoader]: ClassLoader))
  }
}
