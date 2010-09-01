trait Test[A] {
  def m( a: A ): A
  def specified(a:A):A = a
}

abstract class T2[A] extends Test[A]