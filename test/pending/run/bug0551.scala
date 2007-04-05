case class A(x: int)
class B(x: int) extends A(x)
object Test extends Application {
  Console.println(A(1) == new B(1))
}
