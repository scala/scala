//> using options -Xlint:strict-unsealed-patmat -Werror
sealed trait A
final case class B() extends A
final case class C() extends A

object x extends App {

  def matcher[A1 <: A](a1: A1) = a1 match {
    case x @ (_: B | _: C) => println("B")
  }

}
