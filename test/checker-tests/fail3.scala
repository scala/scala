object Obby {
  val Set = scala.collection.immutable.Set
}

// % work/check all -uniqid -Xprint:constructors work/fail3.scala
// TypeStack init: REFERENCE(type AnyRef#2783)
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
// [[syntax trees at end of constructors]]// Scala source: fail3.scala
// package <empty>#3 {
//   final class Obby#9240 extends java.lang.Object#2488 with ScalaObject#1481 {
//     private[this] val Set#9246: object scala.collection.immutable.Set#9713 = _;
//     <stable> <accessor> def Set#9245(): object scala.collection.immutable.Set#9713 = Obby#9240.this.Set#9246;
//     def this#9244(): object Obby#9240 = {
//       Obby#9240.super.this#5850();
//       Obby#9240.this.Set#9246 = scala#23.collection#2221.immutable#8875.Set#9712;
//       ()
//     }
//   }
// }
//
// [Now checking: constructors]
// work/fail3.scala:2: error:
// **** ERROR DURING INTERNAL CHECKING ****
// value Set#9246 in object Obby#9240 cannot be accessed in object Obby#9240
//  because of an internal error (no accessible symbol):
// sym = value Set#9246
// underlying(sym) = value Set#9246
// pre = object Obby#9240
// site = Obby#9240.this
// tree = Obby#9240.this.Set#9246
// sym.accessBoundary(sym.owner) = object Obby#9240
// sym.ownerChain = List(value Set#9246, object Obby#9240, package <empty>#4, package <root>#2)
// sym.owner.thisType = object Obby#9240
// context.owner = package <empty>#4
// context.outer.enclClass.owner = package <empty>#4
//   val Set = scala.collection.immutable.Set
//       ^
// one error found
