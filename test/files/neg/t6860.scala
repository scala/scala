class Bippy[T](val value: T) extends annotation.StaticAnnotation

class A {
  def f1: Int @Bippy("hi") = 1
  def f2: Int @Bippy[String]("hi") = 2

  @throws("what do I throw?") def f3 = throw new RuntimeException
  @throws[RuntimeException]("that's good to know!") def f4 = throw new RuntimeException
}
