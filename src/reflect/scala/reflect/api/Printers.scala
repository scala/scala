package scala.reflect
package api

import java.io.{ PrintWriter, StringWriter }

/** A slice of [[scala.reflect.api.Universe the Scala reflection cake]] that defines prettyprinting functionality.
 *
 *  === Examples (trees) ===
 *
 *  {{{
 *  scala> import scala.reflect.runtime.universe._
 *  import scala.reflect.runtime.universe._
 *
 *  scala> def tree = reify{ final class C { def x = 2 } }.tree
 *  tree: reflect.runtime.universe.Tree
 *
 *  // show displays prettified representation of reflection artifacts
 *  // which is typically close to Scala code, but sometimes not quite
 *  // (e.g. here the constructor is shown in a desugared way)
 *  scala> show(tree)
 *  res0: String =
 *  {
 *    final class C extends AnyRef {
 *      def <init>() = {
 *        super.<init>();
 *        ()
 *      };
 *      def x = 2
 *    };
 *    ()
 *  }
 *
 *  // showRaw displays internal structure of a given reflection object
 *  // trees and types (type examples are shown below) are case classes
 *  // so they are shown in a form that's almost copy/pasteable
 *  //
 *  // almost copy/pasteable, but not completely - that's because of symbols
 *  // there's no good way to get a roundtrip-surviving representation of symbols
 *  // in general case, therefore only symbol names are shown (e.g. take a look at AnyRef)
 *  //
 *  // in such a representation, it's impossible to distinguish Idents/Selects
 *  // that have underlying symbols vs ones that don't have symbols, because in both cases
 *  // only names will be printed
 *  //
 *  // to overcome this limitation, use `printIds` and `printKinds` - optional parameters
 *  // of the `showRaw` method (an example is shown below)
 *  scala> showRaw(tree)
 *  res1: String = Block(List(
 *    ClassDef(Modifiers(FINAL), newTypeName("C"), List(), Template(
 *      List(Ident(newTypeName("AnyRef"))),
 *      emptyValDef,
 *      List(
 *        DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(),
 *          Block(List(
 *            Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())),
 *            Literal(Constant(())))),
 *        DefDef(Modifiers(), newTermName("x"), List(), List(), TypeTree(),
 *          Literal(Constant(2))))))),
 *    Literal(Constant(())))
 *
 *  scala> import scala.tools.reflect.ToolBox // requires scala-compiler.jar
 *  import scala.tools.reflect.ToolBox
 *
 *  scala> import scala.reflect.runtime.{currentMirror => cm}
 *  import scala.reflect.runtime.{currentMirror=>cm}
 *
 *  // showRaw can also print types next to the artifacts being inspected
 *  // provide true for a `printTypes` arguments to achieve this effect
 *  scala> showRaw(cm.mkToolBox().typeCheck(tree), printTypes = true)
 *  res2: String = Block[1](List(
 *    ClassDef[2](Modifiers(FINAL), newTypeName("C"), List(), Template[3](
 *      List(Ident[4](newTypeName("AnyRef"))),
 *      emptyValDef,
 *      List(
 *        DefDef[2](Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree[3](),
 *          Block[1](List(
 *            Apply[4](Select[5](Super[6](This[3](newTypeName("C")), tpnme.EMPTY), ...))),
 *            Literal[1](Constant(())))),
 *        DefDef[2](Modifiers(), newTermName("x"), List(), List(), TypeTree[7](),
 *          Literal[8](Constant(2))))))),
 *    Literal[1](Constant(())))
 *  [1] TypeRef(ThisType(scala), scala.Unit, List())
 *  [2] NoType
 *  [3] TypeRef(NoPrefix, newTypeName("C"), List())
 *  [4] TypeRef(ThisType(java.lang), java.lang.Object, List())
 *  [5] MethodType(List(), TypeRef(ThisType(java.lang), java.lang.Object, List()))
 *  [6] SuperType(ThisType(newTypeName("C")), TypeRef(... java.lang.Object ...))
 *  [7] TypeRef(ThisType(scala), scala.Int, List())
 *  [8] ConstantType(Constant(2))
 *  }}}
 *
 *  === Examples (types) ===
 *
 *  {{{
 *  scala> import scala.reflect.runtime.universe._
 *  import scala.reflect.runtime.universe._
 *
 *  scala> def tpe = typeOf[{ def x: Int; val y: List[Int] }]
 *  tpe: reflect.runtime.universe.Type
 *
 *  // show has already been discussed above
 *  scala> show(tpe)
 *  res0: String = scala.AnyRef{def x: Int; val y: scala.List[Int]}
 *
 *  // showRaw has already been discussed above
 *  scala> showRaw(tpe)
 *  res1: String = RefinedType(
 *    List(TypeRef(ThisType(scala), newTypeName("AnyRef"), List())),
 *    Scope(
 *      newTermName("x"),
 *      newTermName("y")))
 *
 *  // when `printIds` and/or `printKinds` arguments are provided for showRaw
 *  // the prettyprinter reveals underlying symbols and their flavors
 *  //
 *  // courtesy of `printKinds` we can see four different symbols: a package class `scala`,
 *  // a type alias `AnyRef`, a method named `x` and a getter named `y`.
 *  //
 *  // thanks to `printIds` we can see unique identifiers of symbols
 *  // so that it becomes possible to distinguish, say, `scala.collection.immutable.List`
 *  // from `scala.collection.mutable.List` (this also helps in rare cases
 *  // when the same reflection entity is represented by multiple symbols, but let's
 *  // not speak of these horrors here)
 *  scala> showRaw(tpe, printIds = true, printKinds = true)
 *  res2: String = RefinedType(
 *    List(TypeRef(ThisType(scala#2043#PK), newTypeName("AnyRef")#691#TPE, List())),
 *    Scope(
 *      newTermName("x")#2540#METH,
 *      newTermName("y")#2541#GET))
 *  }}}
 */
trait Printers { self: Universe =>

  protected trait TreePrinter {
    def print(args: Any*)
    protected var printTypes = false
    protected var printIds = false
    protected var printKinds = false
    protected var printMirrors = false
    def withTypes: this.type = { printTypes = true; this }
    def withoutTypes: this.type = { printTypes = false; this }
    def withIds: this.type = { printIds = true; this }
    def withoutIds: this.type = { printIds = false; this }
    def withKinds: this.type = { printKinds = true; this }
    def withoutKinds: this.type = { printKinds = false; this }
    def withMirrors: this.type = { printMirrors = true; this }
    def withoutMirrors: this.type = { printMirrors = false; this }
  }

  case class BooleanFlag(val value: Option[Boolean])
  object BooleanFlag {
    import scala.language.implicitConversions
    implicit def booleanToBooleanFlag(value: Boolean): BooleanFlag = BooleanFlag(Some(value))
    implicit def optionToBooleanFlag(value: Option[Boolean]): BooleanFlag = BooleanFlag(value)
  }

  protected def render(what: Any, mkPrinter: PrintWriter => TreePrinter, printTypes: BooleanFlag = None, printIds: BooleanFlag = None, printKinds: BooleanFlag = None, printMirrors: BooleanFlag = None): String = {
    val buffer = new StringWriter()
    val writer = new PrintWriter(buffer)
    var printer = mkPrinter(writer)
    printTypes.value.map(printTypes => if (printTypes) printer.withTypes else printer.withoutTypes)
    printIds.value.map(printIds => if (printIds) printer.withIds else printer.withoutIds)
    printKinds.value.map(printKinds => if (printKinds) printer.withKinds else printer.withoutKinds)
    printMirrors.value.map(printMirrors => if (printMirrors) printer.withMirrors else printer.withoutMirrors)
    printer.print(what)
    writer.flush()
    buffer.toString
  }

  /** By default trees are printed with `show` */
  override protected def treeToString(tree: Tree) = show(tree)

  /** Renders a prettified representation of a reflection artifact.
   *  Typically it looks very close to the Scala code it represents.
   */
  def show(any: Any, printTypes: BooleanFlag = None, printIds: BooleanFlag = None, printKinds: BooleanFlag = None, printMirrors: BooleanFlag = None): String =
    render(any, newTreePrinter(_), printTypes, printIds, printKinds, printMirrors)

  /** Hook to define what `show(...)` means.
   */
  protected def newTreePrinter(out: PrintWriter): TreePrinter

  /** Renders internal structure of a reflection artifact.
   */
  def showRaw(any: Any, printTypes: BooleanFlag = None, printIds: BooleanFlag = None, printKinds: BooleanFlag = None, printMirrors: BooleanFlag = None): String =
    render(any, newRawTreePrinter(_), printTypes, printIds, printKinds, printMirrors)

  /** Hook to define what `showRaw(...)` means.
   */
  protected def newRawTreePrinter(out: PrintWriter): TreePrinter

  /** Renders a prettified representation of a name.
   */
  def show(name: Name): String

  /** Renders internal structure of a name.
   */
  def showRaw(name: Name): String = name.toString

  /** Renders a prettified representation of a flag set.
   */
  def show(flags: FlagSet): String

  /** Renders internal structure of a flag set.
   */
  def showRaw(flags: FlagSet): String = flags.toString
}
