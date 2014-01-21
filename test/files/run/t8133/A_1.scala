//  a.scala
package object pkg {
  class AnyOps(val x: Any) extends AnyVal
  def AnyOps(x: Any) = new AnyOps(x)
}
