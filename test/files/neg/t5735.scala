abstract class Base {
  def a: String = "one"
}
class Clazz extends Base {
  def a(x: Int): Int = 2
  val z: Int = a
}
