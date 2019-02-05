
class C {
  val `_` = 42   // ok
}

class D {
  val `_` = 42
  val `_` = 17   // not ok
}
