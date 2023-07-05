trait U {
  def a: Int = 0
  trait X
}
package object p extends U {
  def b: Int = 0
  trait Y
}
