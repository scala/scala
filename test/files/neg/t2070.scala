object t2070 {
  trait A {
    type T[X]
    def f(x : T[Int]) = x
  }

  object B extends A {
    trait T[X[_]]
  }
}
