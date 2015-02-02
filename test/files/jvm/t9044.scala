trait A
trait B
object Test extends A with B with App {
  val is = Test.getClass.getInterfaces.mkString(", ")
  assert(is == "interface A, interface B, interface scala.App", is)
}
