package a {
  // method before classes
  trait Foo {
    def crash(x: Dingus[_]): Unit = x match { case m: Bippy[tv] => () }

    class Dingus[T]
    class Bippy[CC[X] <: Seq[X]]() extends Dingus[CC[Int]]
  }
}

package b {
  // classes before method
  trait Foo {
    class Dingus[T]
    class Bippy[CC[X] <: Seq[X]]() extends Dingus[CC[Int]]

    def crash(x: Dingus[_]): Unit = x match { case m: Bippy[tv] => () }
  }
}


/*
// With crash below the classes:
% scalac -Dscalac.debug.tvar ./a.scala
[    create] ?_$1                     ( In Foo#crash )
[   setInst] tv[Int]                  ( In Foo#crash, _$1=tv[Int] )
[    create] tv[Int]                  ( In Foo#crash )
[     clone] tv[Int]                  ( Foo#crash )

// With crash above the classes:
% scalac -Dscalac.debug.tvar ./a.scala
[    create] ?tv                      ( In Foo#crash )
./a.scala:2: error: Invalid type application in TypeVar: List(), List(Int)
  def crash(x: Dingus[_]): Unit = x match { case m: Bippy[tv] => () }
                                                  ^
one error found
*/
