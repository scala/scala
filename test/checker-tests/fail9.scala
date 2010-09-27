// More from constructors
class Dong {
  def y1 = Dong.x1
  def y2 = Dong.x2
  def y3 = Dong.x3
  def y4 = Dong.x4
  def y5 = Dong.x5
  def y6 = Dong.x6
}

object Dong {
  private val x1 = 1
  private def x2 = 2
  private lazy val x3 = 3
  private[Dong] val x4 = 4
  private[Dong] val x5 = 5
  private[Dong] val x6 = 6
}

// % work/check all -Xprint:constru work/fail9.scala
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
// [[syntax trees at end of constructors]]// Scala source: fail9.scala
// package <empty> {
//   class Dong extends java.lang.Object with ScalaObject {
//     def y1(): Int = Dong.Dong$$x1();
//     def y2(): Int = Dong.Dong$$x2();
//     def y3(): Int = Dong.Dong$$x3();
//     def y4(): Int = Dong.x4();
//     def y5(): Int = Dong.x5();
//     def y6(): Int = Dong.x6();
//     def this(): Dong = {
//       Dong.super.this();
//       ()
//     }
//   };
//   final class Dong extends java.lang.Object with ScalaObject {
//     private[this] val Dong$$x1: Int = _;
//     final <stable> <accessor> def Dong$$x1(): Int = Dong.this.Dong$$x1;
//     final def Dong$$x2(): Int = 2;
//     lazy private[this] var Dong$$x3: Int = _;
//     final <stable> <accessor> lazy def Dong$$x3(): Int = {
//       Dong.this.Dong$$x3 = 3;
//       Dong.this.Dong$$x3
//     };
//     private[this] val x4: Int = _;
//     <stable> <accessor> private[Dong] def x4(): Int = Dong.this.x4;
//     private[this] val x5: Int = _;
//     <stable> <accessor> private[Dong] def x5(): Int = Dong.this.x5;
//     private[this] val x6: Int = _;
//     <stable> <accessor> private[Dong] def x6(): Int = Dong.this.x6;
//     def this(): object Dong = {
//       Dong.super.this();
//       Dong.this.Dong$$x1 = 1;
//       Dong.this.x4 = 4;
//       Dong.this.x5 = 5;
//       Dong.this.x6 = 6;
//       ()
//     }
//   }
// }
//
// [Now checking: constructors]
// work/fail9.scala:12: error:
// **** ERROR DURING INTERNAL CHECKING ****
// value Dong$$x1 in object Dong cannot be accessed in object Dong
//  because of an internal error (no accessible symbol):
// sym = value Dong$$x1
// underlying(sym) = value Dong$$x1
// pre = object Dong
// site = Dong.this
// tree = Dong.this.Dong$$x1
// sym.accessBoundary(sym.owner) = object Dong
// sym.ownerChain = List(value Dong$$x1, object Dong, package <empty>, package <root>)
// sym.owner.thisType = object Dong
// context.owner = package <empty>
// context.outer.enclClass.owner = package <empty>
//   private val x1 = 1
//               ^
// work/fail9.scala:15: error:
// **** ERROR DURING INTERNAL CHECKING ****
// value x4 in object Dong cannot be accessed in object Dong
//  because of an internal error (no accessible symbol):
// sym = value x4
// underlying(sym) = value x4
// pre = object Dong
// site = Dong.this
// tree = Dong.this.x4
// sym.accessBoundary(sym.owner) = object Dong
// sym.ownerChain = List(value x4, object Dong, package <empty>, package <root>)
// sym.owner.thisType = object Dong
// context.owner = package <empty>
// context.outer.enclClass.owner = package <empty>
//   private[Dong] val x4 = 4
//                     ^
// work/fail9.scala:16: error:
// **** ERROR DURING INTERNAL CHECKING ****
// value x5 in object Dong cannot be accessed in object Dong
//  because of an internal error (no accessible symbol):
// sym = value x5
// underlying(sym) = value x5
// pre = object Dong
// site = Dong.this
// tree = Dong.this.x5
// sym.accessBoundary(sym.owner) = object Dong
// sym.ownerChain = List(value x5, object Dong, package <empty>, package <root>)
// sym.owner.thisType = object Dong
// context.owner = package <empty>
// context.outer.enclClass.owner = package <empty>
//   private[Dong] val x5 = 5
//                     ^
// work/fail9.scala:17: error:
// **** ERROR DURING INTERNAL CHECKING ****
// value x6 in object Dong cannot be accessed in object Dong
//  because of an internal error (no accessible symbol):
// sym = value x6
// underlying(sym) = value x6
// pre = object Dong
// site = Dong.this
// tree = Dong.this.x6
// sym.accessBoundary(sym.owner) = object Dong
// sym.ownerChain = List(value x6, object Dong, package <empty>, package <root>)
// sym.owner.thisType = object Dong
// context.owner = package <empty>
// context.outer.enclClass.owner = package <empty>
//   private[Dong] val x6 = 6
//                     ^
// four errors found
