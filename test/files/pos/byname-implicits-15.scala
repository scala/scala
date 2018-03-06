object Test {
  trait Generic[T] {
    type Repr
  }

  object Generic {
    type Aux[T, R] = Generic[T] { type Repr = R }
    implicit def genTuple3[T, U, V]: Aux[(T, U, V), (T, (U, (V, Unit)))] = ???
    implicit def genTuple5[T, U, V, W, X]: Aux[(T, U, V, W, X), (T, (U, (V, (W, (X, Unit)))))] = ???
  }

  trait Show[T]
  object Show {
    implicit val showUnit: Show[Unit] = ???
    implicit val showInt: Show[Int] = ???
    implicit def showPair[T, U](implicit st: Show[T], su: Show[U]): Show[(T, U)] = ???
    implicit def showGen[T, R](implicit gen: Generic.Aux[T, R], sr: => Show[R]): Show[T] = ???
  }

  type I5 = (Int, Int, Int, Int, Int)

  // Demonstrates that the bynamity of sr suppresses the false positive divergence test
  // which would otherwise see 5 nested pairs dominating 3 nested pairs.
  implicitly[Show[(I5, I5, I5)]]
  implicitly[Show[(Int, I5, Int)]]
  implicitly[Show[(I5, (I5, I5, I5), Int)]]
}
