class Base[Coll] {
  trait Transformed[S] {
    lazy val underlying: Coll = ???
  }
}

class Derived extends Base[String] {
  class C extends Transformed[Any]
}

