/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * Author: Paul Phillips
 */

package scala.tools.nsc
package matching

import util.Position
import collection._
import mutable.BitSet
import immutable.IntMap
import MatchUtil._
import annotation.elidable

trait Patterns extends ast.TreeDSL {
  self: transform.ExplicitOuter =>

  import global.{ typer => _, _ }
  import definitions._
  import CODE._

  val NoPattern = Pattern(EmptyTree)

  class ConstructorPattern(override val tree: Apply) extends Pattern(tree) {
    private val Apply(fn, args) = tree

    def isCaseClass = fn.isType
    def isCaseObject = args == Nil && !fn.isType
    def isFunction = !isCaseObject && !isCaseObject
  }

  class LiteralPattern(override val tree: Literal) extends Pattern(tree)
  class IdentPattern(override val tree: Ident) extends Pattern(tree)
  class ObjectPattern(override val tree: Apply) extends Pattern(tree)

  class TypedPattern(override val tree: Typed) extends Pattern(tree)
  class UnapplyPattern(override val tree: UnApply) extends Pattern(tree)
  class SeqPattern(override val tree: UnApply) extends Pattern(tree)

  object Pattern {
    def apply(tree: Tree): Pattern = tree match {
      case x: Apply => new ConstructorPattern(x)
      case _        => new MiscPattern(tree)
    }
    // def apply(x: Tree, preGuard: Tree): Pattern = new Pattern(x, preGuard)
    def unapply(other: Pattern): Option[Tree] = Some(other.tree)
  }

  class MiscPattern(tree: Tree) extends Pattern(tree) { }

  sealed abstract class Pattern(val tree: Tree, val preGuard: Tree) {
    // type T <: Tree
    // val tree: T

    def this(tree: Tree) = this(tree, null)

    def    sym  = tree.symbol
    def    tpe  = tree.tpe
    def prefix  = tpe.prefix
    def isEmpty = tree.isEmpty

    def isSymValid = (sym != null) && (sym != NoSymbol)

    def setType(tpe: Type): this.type = {
      tree setType tpe
      this
    }
    lazy val stripped = strip(tree)._1
    lazy val boundVariables = strip(tree)._2
    lazy val unbound: Pattern = copy(stripped)

    def mkSingleton = tpe match {
      case st: SingleType => st
      case _              => singleType(prefix, sym)
    }

    final def isBind              = cond(tree)     { case x: Bind => true }
    final def isDefault           = cond(stripped) { case EmptyTree | WILD() => true }
    final def isStar              = cond(stripped) { case Star(q) => Pattern(q).isDefault }
    final def isAlternative       = cond(stripped) { case Alternative(_) => true }
    final def isRightIgnoring     = cond(stripped) { case ArrayValue(_, xs) if !xs.isEmpty => Pattern(xs.last).isStar }

    /** returns true if pattern tests an object */
    final def isObjectTest(head: Type) =
      isSymValid && prefix.isStable && (head =:= mkSingleton)

    /** Helpers **/
    private def strip(t: Tree, syms: List[Symbol] = Nil): (Tree, List[Symbol]) = t match {
      case b @ Bind(_, pat) => strip(pat, b.symbol :: syms)
      case _                => (t, syms)
    }

    /** Standard methods **/
    def copy(
      tree: Tree = this.tree,
      preGuard: Tree = this.preGuard
    ): Pattern = Pattern(tree)  // XXX
    // Pattern(tree, preGuard)

    override def toString() = "Pattern(%s)".format(tree)
    override def equals(other: Any) = other match {
      case Pattern(t) => this.tree == t
      case _          => super.equals(other)
    }
    override def hashCode() = tree.hashCode()
  }
}