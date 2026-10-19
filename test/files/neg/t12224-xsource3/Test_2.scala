//> using options -Xsource:3
// The Java interface re-declares the default abstractly; javac requires an implementation, and
// without one the JVM throws AbstractMethodError.
class ChildScala extends BaseIntermediateJava_1 {
  override def getText: String = ""
}
