package tastytest

object TypeRefIns {
  type Id[A] >: A
  val a: Array[? >: Id[Int]] = Array(1, 2)
  val b = a(0) // inferred TYPEREFin
}
