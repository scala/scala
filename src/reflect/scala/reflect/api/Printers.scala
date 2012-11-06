package scala.reflect
package api

import java.io.{ PrintWriter, StringWriter }

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 * Utilities for nicely printing [[scala.reflect.api.Trees]] and [[scala.reflect.api.Types]].
 *
 * === Printing Trees ===
 * The method `show` displays the "prettified" representation of reflection artifacts.
 * This representation provides one with the desugared Java representation of Scala code.
 * For example:
 *
 * {{{
 *  scala> import scala.reflect.runtime.universe._
 *  import scala.reflect.runtime.universe._
 *
 *  scala> def tree = reify{ final class C { def x = 2 } }.tree
 *  tree: reflect.runtime.universe.Tree
 *
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
 * }}}
 *
 * The method `showRaw` displays internal structure of a given reflection object
 * as a Scala abstract syntax tree (AST), the representation that the Scala typechecker
 * operates on.
 *
 * Note, that while this representation appears to generate correct trees that one
 * might think would be possible to use in a macro implementation, this is not usually
 * the case. Symbols aren't fully represented (only their names are). Thus, this method
 * is best-suited for use simply inspecting ASTs given some valid Scala code.
 * {{{
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
 * }}}
 *
 * The method `showRaw` can also print [[scala.reflect.api.Types]] next to the artifacts
 * being inspected
 * {{{
 *  scala> import scala.tools.reflect.ToolBox // requires scala-compiler.jar
 *  import scala.tools.reflect.ToolBox
 *
 *  scala> import scala.reflect.runtime.{currentMirror => cm}
 *  import scala.reflect.runtime.{currentMirror=>cm}
 *
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
 *  === Printing Types ===
 *
 * The method `show`
 *  {{{
 *  scala> import scala.reflect.runtime.universe._
 *  import scala.reflect.runtime.universe._
 *
 *  scala> def tpe = typeOf[{ def x: Int; val y: List[Int] }]
 *  tpe: reflect.runtime.universe.Type
 *
 *  scala> show(tpe)
 *  res0: String = scala.AnyRef{def x: Int; val y: scala.List[Int]}
 *  }}}
 *
 * Like the method `showRaw` for [[scala.reflect.api.Trees]], `showRaw`
 * for [[scala.reflect.api.Types]] provides a visualization of the Scala
 * AST operated on by the Scala typechecker.
 * {{{
 *  // showRaw has already been discussed above
 *  scala> showRaw(tpe)
 *  res1: String = RefinedType(
 *    List(TypeRef(ThisType(scala), newTypeName("AnyRef"), List())),
 *    Scope(
 *      newTermName("x"),
 *      newTermName("y")))
 * }}}
 *
 * `printIds` and/or `printKinds` can additionally be supplied as arguments in a call to
 * `showRaw` which additionally shows the unique identifiers of symbols.
 *
 * {{{
 *  scala> showRaw(tpe, printIds = true, printKinds = true)
 *  res2: String = RefinedType(
 *    List(TypeRef(ThisType(scala#2043#PK), newTypeName("AnyRef")#691#TPE, List())),
 *    Scope(
 *      newTermName("x")#2540#METH,
 *      newTermName("y")#2541#GET))
 * }}}
 *
 * For more details about `Printer`s and other aspects of Scala reflection, see the
 * [[http://docs.scala-lang.org/overviews/reflection/overview.html Reflection Guide]]
 *
 *  @group ReflectionAPI
 */
trait Printers { self: Universe =>

  /** @group Printers */
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

  /** @group Printers */
  case class BooleanFlag(val value: Option[Boolean])
  /** @group Printers */
  object BooleanFlag {
    import scala.language.implicitConversions
    implicit def booleanToBooleanFlag(value: Boolean): BooleanFlag = BooleanFlag(Some(value))
    implicit def optionToBooleanFlag(value: Option[Boolean]): BooleanFlag = BooleanFlag(value)
  }

  /** @group Printers */
  protected def render(what: Any, mkPrinter: PrintWriter => TreePrinter, printTypes: BooleanFlag = None, printIds: BooleanFlag = None, printKinds: BooleanFlag = None, printMirrors: BooleanFlag = None): String = {
    val buffer = new StringWriter()
    val writer = new PrintWriter(buffer)
    val printer = mkPrinter(writer)
    printTypes.value.map(printTypes => if (printTypes) printer.withTypes else printer.withoutTypes)
    printIds.value.map(printIds => if (printIds) printer.withIds else printer.withoutIds)
    printKinds.value.map(printKinds => if (printKinds) printer.withKinds else printer.withoutKinds)
    printMirrors.value.map(printMirrors => if (printMirrors) printer.withMirrors else printer.withoutMirrors)
    printer.print(what)
    writer.flush()
    buffer.toString
  }

  /** By default trees are printed with `show`
   * @group Printers
   */
  override protected def treeToString(tree: Tree) = show(tree)

  /** Renders a representation of a reflection artifact
   * as desugared Java code.
   *
   * @group Printers
   */
  def show(any: Any, printTypes: BooleanFlag = None, printIds: BooleanFlag = None, printKinds: BooleanFlag = None, printMirrors: BooleanFlag = None): String =
    render(any, newTreePrinter(_), printTypes, printIds, printKinds, printMirrors)

  /** Hook to define what `show(...)` means.
   * @group Printers
   */
  protected def newTreePrinter(out: PrintWriter): TreePrinter

  /** Renders internal structure of a reflection artifact as the
   * visualization of a Scala syntax tree.
   *
   * @group Printers
   */
  def showRaw(any: Any, printTypes: BooleanFlag = None, printIds: BooleanFlag = None, printKinds: BooleanFlag = None, printMirrors: BooleanFlag = None): String =
    render(any, newRawTreePrinter(_), printTypes, printIds, printKinds, printMirrors)

  /** Hook to define what `showRaw(...)` means.
   * @group Printers
   */
  protected def newRawTreePrinter(out: PrintWriter): TreePrinter

  /** Renders a prettified representation of a name.
   * @group Printers
   */
  def show(name: Name): String

  /** Renders internal structure of a name.
   * @group Printers
   */
  def showRaw(name: Name): String = name.toString

  /** Renders a prettified representation of a flag set.
   * @group Printers
   */
  def show(flags: FlagSet): String

  /** Renders internal structure of a flag set.
   * @group Printers
   */
  def showRaw(flags: FlagSet): String = flags.toString
}
