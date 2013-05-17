class A {
  trait Container[+T]
  trait Template[+CC[X] <: Container[X]]

  class C1[T] extends Template[C1] with Container[T]
  class C2[T] extends Template[C2] with Container[T]

  /** Target expression:
   *    List(new C1[String], new C2[String])
   */

  // Here's what would ideally be inferred.
  //
  // scala> :type List[Template[Container] with Container[String]](new C1[String], new C2[String])
  // List[Template[Container] with Container[java.lang.String]]
  //
  // Here's what it does infer.
  //
  // scala> :type List(new C1[String], new C2[String])
  // <console>:8: error: type mismatch;
  //  found   : C1[String]
  //  required: Container[String] with Template[Container[Any] with Template[Container[Any] with Template[Any] with ScalaObject] with ScalaObject] with ScalaObject
  //        List(new C1[String], new C2[String])
  //             ^
  //
  // Simplified, the inferred type is:
  //
  //   List[Container[String] with Template[Container[Any] with Template[Container[Any] with Template[Any]]]
  //
  // *** Update 2/24/2012
  //
  // Hey, now there are polytypes in the inferred type.
  // Not sure if that is progress or regress.
  //
  // test/pending/pos/those-kinds-are-high.scala:36: error: type mismatch;
  //  found   : C1[String]
  //  required: ScalaObject with Container[String] with Template[ScalaObject with Container with Template[ScalaObject with Container with Template[[X]Container[X]]]]
  //   def fFail = List(new C1[String], new C2[String])
  //                    ^
  // test/pending/pos/those-kinds-are-high.scala:36: error: type mismatch;
  //  found   : C2[String]
  //  required: ScalaObject with Container[String] with Template[ScalaObject with Container with Template[ScalaObject with Container with Template[[X]Container[X]]]]
  //   def fFail = List(new C1[String], new C2[String])
  //                                    ^
  // two errors found

  /** Working version explicitly typed.
   */
  def fExplicit = List[Template[Container] with Container[String]](new C1[String], new C2[String])

  // nope
  def fFail = List(new C1[String], new C2[String])
}


trait Other {
  trait GenBar[+A]
  trait Bar[+A] extends GenBar[A]
  trait Templ[+A, +CC[X] <: GenBar[X]]

  abstract class CC1[+A] extends Templ[A, CC1] with Bar[A]
  abstract class CC2[+A] extends Templ[A, CC2] with Bar[A]

  // Compiles
  class A1 {
    abstract class BarFactory[CC[X] <: Bar[X]]

    def f(x: Boolean) = if (x) (null: BarFactory[CC1]) else (null: BarFactory[CC2])
  }

  // Fails - only difference is CC covariant.
  class A2 {
    abstract class BarFactory[+CC[X] <: Bar[X]]

    def f(x: Boolean) = if (x) (null: BarFactory[CC1]) else (null: BarFactory[CC2])
    // c.scala:23: error: kinds of the type arguments (Bar with Templ[Any,Bar]) do not conform to the expected kinds of the type parameters (type CC) in class BarFactory.
    // Bar with Templ[Any,Bar]'s type parameters do not match type CC's expected parameters:
    // <empty> has no type parameters, but type CC has one
    //   def f(x: Boolean) = if (x) (null: BarFactory[CC1]) else (null: BarFactory[CC2])
    //       ^
    // one error found
  }

  // Compiles - CC contravariant.
  class A3 {
    abstract class BarFactory[-CC[X] <: Bar[X]] // with Templ[X, CC]]

    def f(x: Boolean) = if (x) (null: BarFactory[CC1]) else (null: BarFactory[CC2])
    // c.scala:23: error: kinds of the type arguments (Bar with Templ[Any,Bar]) do not conform to the expected kinds of the type parameters (type CC) in class BarFactory.
    // Bar with Templ[Any,Bar]'s type parameters do not match type CC's expected parameters:
    // <empty> has no type parameters, but type CC has one
    //   def f(x: Boolean) = if (x) (null: BarFactory[CC1]) else (null: BarFactory[CC2])
    //       ^
    // one error found
  }
}
