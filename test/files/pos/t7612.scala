object Test {
  trait Fili[A]
  trait Kili[M] {
    def fili: Fili[M]
  }

  trait A extends Kili[A] {
    def fili: Fili[A]
  }

  trait Ori[M] extends Kili[M] {
    val ori: Fili[M]
    def fili: ori.type
  }
  trait B extends Ori[B]

  def foo(a: A, b: B) = if (true) a else b
}
