trait outer[cc[x], t] {
  type M[x]
  type V
}

trait Coll[+A]
trait List[+A] extends Coll[A]
trait Set[A] extends Coll[A]
trait Stream[+A] extends Coll[A]
trait Vector[A] extends Coll[A]

trait Foo[A] {
  type M1[CC[x]] = outer[CC, A]#V
  type M2[CC[x]] = M1[CC]
  type M3[CC[x]] = outer[CC, A]#M[A]
  type M4[CC[x]] = M3[CC]

  def f1: M1[List]
  def f2: M2[Set]
  def f3: M3[Stream]
  def f4: M4[Vector]

  def g12 = List(f1, f2).head
  def g13 = List(f1, f3).head
  def g14 = List(f1, f4).head
  def g23 = List(f2, f3).head
  def g24 = List(f2, f4).head
  def g34 = List(f3, f4).head
}

trait Bar extends Foo[Int] {
  class Bippy {
    def g12 = List(f1, f2).head
    def g13 = List(f1, f3).head
    def g14 = List(f1, f4).head
    def g23 = List(f2, f3).head
    def g24 = List(f2, f4).head
    def g34 = List(f3, f4).head
  }
}
