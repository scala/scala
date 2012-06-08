package scala.tools
package reflect

import scala.reflect.api.Universe
import scala.reflect.base.MirrorOf

trait ToolBox[U <: Universe] {

  /** Underlying universe of a ToolBox
   */
  val u: U

  /** Underlying mirror of a ToolBox
   */
  val mirror: MirrorOf[u.type]

  /** Front end of the toolbox.
   *
   *  Accumulates and displays warnings and errors, can drop to interactive mode (if supported).
   *  The latter can be useful to study the typechecker or to debug complex macros.
   */
  def frontEnd: FrontEnd

  /** Typechecks a tree using this ToolBox.
   *  This populates symbols and types of the tree and possibly transforms it to reflect certain desugarings.
   *
   *  If the tree has unresolved type variables (represented as instances of ``FreeTypeSymbol'' symbols),
   *  then they might, might be partially or might not be specified in the ``freeTypes'' parameter.
   *
   *  If ``silent'' is false, ``TypeError'' will be thrown in case of a typecheck error.
   *  If ``silent'' is true, the typecheck is silent and will return ``EmptyTree'' if an error occurs.
   *  Such errors don't vanish and can be inspected by turning on -Ydebug.
   *
   *  Typechecking can be steered with the following optional parameters:
   *    ``withImplicitViewsDisabled'' recursively prohibits implicit views (though, implicit vals will still be looked up and filled in), default value is false
   *    ``withMacrosDisabled'' recursively prohibits macro expansions and macro-based implicits, default value is false
   */
  def typeCheck(tree: u.Tree, pt: u.Type = u.WildcardType, freeTypes: Map[u.FreeTypeSymbol, u.Type] = Map[u.FreeTypeSymbol, u.Type](), silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): u.Tree


  /** Infers an implicit value of the expected type ``pt'' in the macro callsite context.
   *
   *  If ``silent'' is false, ``TypeError'' will be thrown in case of an inference error.
   *  If ``silent'' is true, the typecheck is silent and will return ``EmptyTree'' if an error occurs.
   *  Such errors don't vanish and can be inspected by turning on -Xlog-implicits.
   *  Unlike in ``typeCheck'', ``silent'' is true by default.
   */
  def inferImplicitValue(pt: u.Type, silent: Boolean = true, withMacrosDisabled: Boolean = false): u.Tree

  /** Infers an implicit view from the provided tree ``tree'' from the type ``from'' to the type ``to'' in the macro callsite context.
   *
   *  Otional parameter, ``reportAmbiguous`` controls whether ambiguous implicit errors should be reported.
   *  If we search for a view simply to find out whether one type is coercible to another, it might be desirable to set this flag to ``false''.
   *
   *  If ``silent'' is false, ``TypeError'' will be thrown in case of an inference error.
   *  If ``silent'' is true, the typecheck is silent and will return ``EmptyTree'' if an error occurs.
   *  Such errors don't vanish and can be inspected by turning on -Xlog-implicits.
   *  Unlike in ``typeCheck'', ``silent'' is true by default.
   */
  def inferImplicitView(tree: u.Tree, from: u.Type, to: u.Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, reportAmbiguous: Boolean = true): u.Tree

  /** Recursively resets symbols and types in a given tree.
   *
   *  Note that this does not revert the tree to its pre-typer shape.
   *  For more info, read up https://issues.scala-lang.org/browse/SI-5464.
   */
  def resetAllAttrs(tree: u.Tree): u.Tree

  /** Recursively resets locally defined symbols and types in a given tree.
   *
   *  Note that this does not revert the tree to its pre-typer shape.
   *  For more info, read up https://issues.scala-lang.org/browse/SI-5464.
   */
  def resetLocalAttrs(tree: u.Tree): u.Tree

  /** .. */
  def parseExpr(code: String): u.Tree

  /** Compiles and runs a tree using this ToolBox.
   *
   *  If the tree has unresolved type variables (represented as instances of ``FreeTypeSymbol'' symbols),
   *  then they all have to be specified in the ``freeTypes'' parameter or an error occurs.
   *
   *  This spawns the compiler at the Namer phase, and pipelines the tree through that compiler.
   *  Currently ``runExpr'' does not accept trees that already typechecked, because typechecking isn't idempotent.
   *  For more info, take a look at https://issues.scala-lang.org/browse/SI-5464.
   */
  def runExpr(tree: u.Tree, freeTypes: Map[u.FreeTypeSymbol, u.Type] = Map[u.FreeTypeSymbol, u.Type]()): Any
}

/** Represents an error during toolboxing
 */
case class ToolBoxError(val message: String, val cause: Throwable = null) extends Throwable(message, cause)
