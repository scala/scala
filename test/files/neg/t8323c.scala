object Test {
  final val a = "Bippy"
  final val b = "Dingo"
  def f(x: a.type) = x
  def f(x: b.type) = x
  def main(args: Array[String]): Unit = ()
}
// Exception in thread "main" java.lang.ClassFormatError: Duplicate method name&signature in class file Test
//   at java.lang.ClassLoader.defineClass1(Native Method)
//   at java.lang.ClassLoader.defineClass(ClassLoader.java:800)
//   at java.security.SecureClassLoader.defineClass(SecureClassLoader.java:142)
//   at java.net.URLClassLoader.defineClass(URLClassLoader.java:449)
