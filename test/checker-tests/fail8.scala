// reverse of fail9
class Ding {
  private val x1 = 1
  private def x2 = 2
  private lazy val x3 = 3
  private[Ding] val x4 = 4
  private[Ding] val x5 = 5
  private[Ding] val x6 = 6
}

object Ding {
  def y1 = new Ding x1
  def y2 = new Ding x2
  def y3 = new Ding x3
  def y4 = new Ding x4
  def y5 = new Ding x5
  def y6 = new Ding x6
}

// % work/check all -Xprint:constru work/fail8.scala
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
// [[syntax trees at end of constructors]]// Scala source: fail8.scala
// package <empty> {
//   class Ding extends java.lang.Object with ScalaObject {
//     private[this] val Ding$$x1: Int = _;
//     final <stable> <accessor> def Ding$$x1(): Int = Ding.this.Ding$$x1;
//     final def Ding$$x2(): Int = 2;
//     lazy private[this] var Ding$$x3: Int = _;
//     final <stable> <accessor> lazy def Ding$$x3(): Int = {
//       Ding.this.Ding$$x3 = 3;
//       Ding.this.Ding$$x3
//     };
//     private[this] val Ding$$x4: Int = _;
//     <stable> <accessor> private[Ding] def Ding$$x4(): Int = Ding.this.Ding$$x4;
//     private[this] val Ding$$x5: Int = _;
//     <stable> <accessor> private[Ding] def Ding$$x5(): Int = Ding.this.Ding$$x5;
//     private[this] val Ding$$x6: Int = _;
//     <stable> <accessor> private[Ding] def Ding$$x6(): Int = Ding.this.Ding$$x6;
//     def this(): Ding = {
//       Ding.super.this();
//       Ding.this.Ding$$x1 = 1;
//       Ding.this.Ding$$x4 = 4;
//       Ding.this.Ding$$x5 = 5;
//       Ding.this.Ding$$x6 = 6;
//       ()
//     }
//   };
//   final class Ding extends java.lang.Object with ScalaObject {
//     def y1(): Int = new Ding().Ding$$x1();
//     def y2(): Int = new Ding().Ding$$x2();
//     def y3(): Int = new Ding().Ding$$x3();
//     def y4(): Int = new Ding().Ding$$x4();
//     def y5(): Int = new Ding().Ding$$x5();
//     def y6(): Int = new Ding().Ding$$x6();
//     def this(): object Ding = {
//       Ding.super.this();
//       ()
//     }
//   }
// }
//
// [Now checking: constructors]
// work/fail8.scala:3: error:
// **** ERROR DURING INTERNAL CHECKING ****
// value Ding$$x1 in class Ding cannot be accessed in Ding
//  because of an internal error (no accessible symbol):
// sym = value Ding$$x1
// underlying(sym) = value Ding$$x1
// pre = Ding
// site = Ding.this
// tree = Ding.this.Ding$$x1
// sym.accessBoundary(sym.owner) = class Ding
// sym.ownerChain = List(value Ding$$x1, class Ding, package <empty>, package <root>)
// sym.owner.thisType = Ding
// context.owner = package <empty>
// context.outer.enclClass.owner = package <empty>
//   private val x1 = 1
//               ^
// work/fail8.scala:6: error:
// **** ERROR DURING INTERNAL CHECKING ****
// value Ding$$x4 in class Ding cannot be accessed in Ding
//  because of an internal error (no accessible symbol):
// sym = value Ding$$x4
// underlying(sym) = value Ding$$x4
// pre = Ding
// site = Ding.this
// tree = Ding.this.Ding$$x4
// sym.accessBoundary(sym.owner) = class Ding
// sym.ownerChain = List(value Ding$$x4, class Ding, package <empty>, package <root>)
// sym.owner.thisType = Ding
// context.owner = package <empty>
// context.outer.enclClass.owner = package <empty>
//   private[Ding] val x4 = 4
//                     ^
// work/fail8.scala:7: error:
// **** ERROR DURING INTERNAL CHECKING ****
// value Ding$$x5 in class Ding cannot be accessed in Ding
//  because of an internal error (no accessible symbol):
// sym = value Ding$$x5
// underlying(sym) = value Ding$$x5
// pre = Ding
// site = Ding.this
// tree = Ding.this.Ding$$x5
// sym.accessBoundary(sym.owner) = class Ding
// sym.ownerChain = List(value Ding$$x5, class Ding, package <empty>, package <root>)
// sym.owner.thisType = Ding
// context.owner = package <empty>
// context.outer.enclClass.owner = package <empty>
//   private[Ding] val x5 = 5
//                     ^
// work/fail8.scala:8: error:
// **** ERROR DURING INTERNAL CHECKING ****
// value Ding$$x6 in class Ding cannot be accessed in Ding
//  because of an internal error (no accessible symbol):
// sym = value Ding$$x6
// underlying(sym) = value Ding$$x6
// pre = Ding
// site = Ding.this
// tree = Ding.this.Ding$$x6
// sym.accessBoundary(sym.owner) = class Ding
// sym.ownerChain = List(value Ding$$x6, class Ding, package <empty>, package <root>)
// sym.owner.thisType = Ding
// context.owner = package <empty>
// context.outer.enclClass.owner = package <empty>
//   private[Ding] val x6 = 6
//                     ^
// four errors found
