// Annotated with various errors I induced while working on pattern type inference.

package p0 {
  import scala.collection.LinearSeq

  case class Thing[A](value: A)
  case class Cell[A](thing: Thing[A], value: A)
  trait Base { def constructors: List[Int] }

  trait Foo[A] {
    // inference-test.scala:3: error: type mismatch;
    //  found   : scala.collection.TraversableOnce[A] with scala.collection.LinearSeq[_]
    //  required: scala.collection.LinearSeq[A]
    //     case xs: scala.collection.LinearSeq[_] => xs
    //                                               ^
    def f1(xs: TraversableOnce[A]): LinearSeq[A] = xs match { case xs: LinearSeq[_] => xs }
    def f2(xs: LinearSeq[A]): TraversableOnce[A] = xs match { case xs: TraversableOnce[_] => xs }

    // Matching IntThing causes both x.value and y.value to be known as Ints
    def f3[a](x: Cell[a]): (Int, Int) = x match {
      case y: Cell[b] => y.thing match { case IntThing => x.value -> y.value }
      // case y: Cell[b] => y.thing match { case _: OtherThing[c] => x.value -> y.value }
      // test/files/pos/inference-test.scala:15: error: type mismatch;
      //  found   : (a, b)
      //  required: Int
      //     case y: Cell[b] => y.thing match { case _: OtherThing[c] => x.value -> y.value }
      //                                                                        ^
    }
    object IntThing extends Thing(5)
    class OtherThing[A](value: A) extends Thing[A](value)
  }

  trait Foo2 {
    def f(x: Base) = x match { case x: AnyRef => x.constructors }
    // test/files/pos/inference-test.scala:31: error: value constructors is not a member of Object
    //   def f(x: Base) = x match { case x: AnyRef => x.constructors }
    //                                                  ^
    // one error found
  }
}

// The successful compilation of p1 and p2 are mortal enemies,
// the battle being waged in inferTypedPattern.

// SI-7472
package p1 {
  sealed trait Bar[S] {
    def system: S
  }

  trait Foo { def bar(): Unit }

  trait FooBar extends Bar[Foo]

  // Example:
  trait Join[S] {
    val bar: Bar[S]

    def f1 = bar match { case fb: FooBar => fb.system.bar() } // error
    def f2 = bar match { case fb: FooBar => (fb: FooBar).system.bar() }
  }

  object Test extends Join[Foo] {
    val bar = new FooBar {
      object system extends Foo {
        def bar(): Unit = ()
      }
    }

    def main(args: Array[String]): Unit = {
      f1
      f2
    }
  }
}

// Test case backstopping reversion of a905d0e7e4
// Formerly pos/patmat-extract-tparam.scala
package p2 {
  trait Bip[T] { def h: T }
  trait BoolBip extends Bip[Boolean]

  class A {
    def g(x: Boolean): Unit = ()
    def f(xs: List[Bip[_]]) = xs foreach { case x: BoolBip => g(x.h) }
  }

  class B {
    def g(x: Boolean): Unit = ()
    def g(x: Int): Unit = ()
    def f(xs: List[Bip[_]]) = xs foreach { case x: BoolBip => g(x.h) }
  }
}
// inference-test.scala:21:
//       case y: Cell[b] => y.thing match { case IntThing => x.value -> y.value }
//               ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  p0.Cell[a]
//         pt  p0.Cell[a]
//      pattp  p0.Cell[b]
//   pattp+pt  p0.Cell[b]
//   pt+pattp  p0.Cell[a]
//     result  p0.Cell[a]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 =:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt =:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp ~:= result
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }