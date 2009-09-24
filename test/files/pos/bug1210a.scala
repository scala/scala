// both styles of abstraction should behave the same
// related to 1210 because that bug broke the OO version below
trait OO {
  abstract class Test { self =>
    type T

    val v: Test {type T = self.T} = self.v.v
  }
}

trait FP {
  abstract class Test[T] {
    val v: Test[T] = v.v
  }
}
