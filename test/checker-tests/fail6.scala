// BoxedUnit/Unit confusion involving while.
//
// Apply( // sym=method while$1, tpe=Unit, tpe.sym=class Unit, tpe.sym.owner=package scala
//   Ident("while$1"), // sym=method while$1, sym.owner=method f, sym.tpe=()Unit, tpe=()Unit, tpe.sym=<none>,
class Erasure {
  def f(b: Boolean) = {    
    if (b) "abc"
    else while (b) ()
  }
}

// % work/check all -Xprint:erasure work/fail6.scala  
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
// [[syntax trees at end of erasure]]// Scala source: fail6.scala
// package <empty> {
//   class Erasure extends java.lang.Object with ScalaObject {
//     def this(): Erasure = {
//       Erasure.super.this();
//       ()
//     };
//     def f(b: Boolean): java.lang.Object = if (b)
//       "abc"
//     else
//       while$1(){
//         if (b)
//           {
//             ();
//             while$1()
//           }
//         else
//           ();
//         scala.runtime.BoxedUnit.UNIT
//       }
//   }
// }
// 
// [Now checking: erasure]
// work/fail6.scala:4: error: 
// **** ERROR DURING INTERNAL CHECKING ****
// type mismatch;
//  found   : scala.runtime.BoxedUnit
//  required: Unit
//     else while (b) ()
//          ^
// one error found
// 
// 
