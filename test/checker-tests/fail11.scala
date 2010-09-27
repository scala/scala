// Constructor rewriting does not preserve ordering.
// Oddly the error is not issued without the thrown exception, even
// though the same reordering takes place.
class Foo(x: Int) {
  def this(x1: Int, x2: Int) = {
    this(x1 + x2)
    throw new Exception
  }
}

// Here it is at lambda lift:
//
// [[syntax trees at end of lambdalift]]// Scala source: fail11.scala
// package <empty> {
//   class Foo extends java.lang.Object with ScalaObject {
//     <paramaccessor> private[this] val x: Int = _;
//     def this(x: Int): Foo = {
//       Foo.super.this();
//       ()
//     };
//     def this(x1: Int, x2: Int): Foo = {
//       Foo.this.this(x1.+(x2));
//       throw new java.lang.Exception();
//       ()
//     }
//   }
// }

//
// % scalac -d /tmp -Xprint:constr -Ycheck-debug -Ycheck:all test/checker-tests/fail11.scala
//
// TypeStack init: REFERENCE(type AnyRef)
// [Not checkable: parser]
// [Not checkable: namer]
// [Not checkable: packageobjects]
// [Now checking: typer]
// [Now checking: superaccessors]
// [Now checking: pickler]
// [Now checking: refchecks]
// [Now checking: selectiveanf]
// [Now checking: liftcode]
// [Now checking: selectivecps]
// [Now checking: uncurry]
// [Now checking: tailcalls]
// [Not checkable: specialize]
// [Not checkable: explicitouter]
// [Now checking: erasure]
// [Now checking: lazyvals]
// [Now checking: lambdalift]
// [[syntax trees at end of constructors]]// Scala source: fail11.scala
// package <empty> {
//   class Foo extends java.lang.Object with ScalaObject {
//     def this(x1: Int, x2: Int): Foo = {
//       Foo.this.this(x1.+(x2));
//       throw new java.lang.Exception();
//       ()
//     };
//     def this(x: Int): Foo = {
//       Foo.super.this();
//       ()
//     }
//   }
// }
//
// [Now checking: constructors]
// test/checker-tests/fail11.scala:4: error:
// **** ERROR DURING INTERNAL CHECKING ****
// called constructor's definition must precede calling constructor's definition
//     this(x1 + x2)
//     ^
// one error found
