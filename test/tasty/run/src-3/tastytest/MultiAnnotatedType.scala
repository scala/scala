package tastytest

object MultiAnnotatedType {

  final class annotA extends scala.annotation.Annotation
  final class annotB extends scala.annotation.Annotation
  final class annotC extends scala.annotation.Annotation

  // test multiple nested repeated annotations
  val listOfStrings = List("foo"): @annotA @annotB @annotC

  def id[A](a: A @annotA @annotB @annotC): A @annotA @annotB @annotC = a

}
