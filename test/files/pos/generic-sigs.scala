import language.existentials

object A {
  def f1 = List(classOf[Int], classOf[String])
  def f2 = List(classOf[String], classOf[Int])
  def f3(x: Class[_ <: Int]) = x
  def f4(x: Class[_ <: String with Int]) = x
  def f5(x: Class[_ <: Int with String]) = x

  class Bippy[T]
  def f6(x: Int) = new Bippy[t forSome { type t <: Int }]
  def f7(x: T forSome { type T <: Float }) = x
  def f8(x: T forSome { type T <: Unit }) = x
  def f9(x: T forSome { type T <: runtime.BoxedUnit }) = x
  def f10(x: Int) = new Bippy[t forSome { type t <: Unit }]
  def f11(x: Int) = new Bippy[t forSome { type t >: Null }]

  class Boppy[+T1,-T2]
  def g1 = new Boppy[t forSome { type t <: Int }, u forSome { type u <: String }]
}
