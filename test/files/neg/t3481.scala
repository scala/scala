object t3481 {
  object ex1 {
    trait A[T] { type B = T }
    def f[T <: A[_]](a: T#B) = 1
    f[A[Int]]("hello")
  }

  object ex2 {
    trait A { type T; def m(t: T) = t.toString }
    class B[T2] extends A { type T = T2 }
    def f[T <: B[_]](a: T#T, b: T) = b.m(a)
    f("Hello", new B[Int])
  }

  object ex3 {
    class B[T] { type T2 = T; def m(t: T2) = t.toString }
    val b: B[_] = new B[Int]
    b.m("Hello")
  }

  object ex4 {
    abstract class B[T] { type T2 = T; def m(t: T2): Any }
    object Test {
      val b: B[_] = sys.error("")
      b.m("Hello")
    }
  }
}