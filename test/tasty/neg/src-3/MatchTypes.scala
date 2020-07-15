package tastytest

object MatchTypes {

  type Elem[X] = X match {
    case List[t] => t
    case Array[t] => t
  }

  class Foo[Coll, X <: Coll match { case List[t] => t case Array[t] => t }] {
    def foo(x: X): X = x
  }

}
