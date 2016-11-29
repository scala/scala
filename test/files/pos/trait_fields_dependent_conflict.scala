// derived from test/files/pos/S5.scala

// compile with -uniqid to see a hint of the trouble
trait N {
  // the symbol for self does not get rebound when synthesizing members in C
  val self: N = ???
  val n: self.type = self
}

abstract class M {
  val self: N
  val n: self.type
}

abstract class MConflict extends N {
  val self: N
  val n: self.type
}

class C extends M with N
