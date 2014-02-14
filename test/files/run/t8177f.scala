trait Thing { type A; var p: A = _ }
class A[T](final val x: Thing { type A = T }) {
  type Q = T

  def x1: T   = x.p
  def x2: Q   = x.p
  def x3: x.A = x.p
}
// all result types should be inferred as Int
class B extends A[Int](null) {
  def y1 = x1
  def y2 = x2
  val y3 = x3 // before SI-8177, this lead to a signature that erased to java.lang.Object
}


object Test extends App {
  val methods = classOf[B].getDeclaredMethods.sortBy(_.getName)
  assert(methods.forall(_.toGenericString.startsWith("public int")))
}
