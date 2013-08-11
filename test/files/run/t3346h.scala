import scala.language.implicitConversions

object Test extends App {
  trait Fundep[T, U] { def u(t: T): U }
  class C { def y = "x" }
  implicit val FundepStringC = new Fundep[String, C]{ def u(t: String) = new C }
  implicit def foo[T, U](x: T)(implicit y: Fundep[T, U]): U = y.u(x)
  println("x".y)
}