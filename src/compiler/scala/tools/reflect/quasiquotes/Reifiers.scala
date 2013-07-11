package scala.tools.reflect
package quasiquotes

import java.lang.UnsupportedOperationException
import scala.reflect.reify.{Reifier => ReflectReifier}
import scala.reflect.internal.Flags._

trait Reifiers { self: Quasiquotes =>
  import global._
  import global.build.SyntacticClassDef
  import global.treeInfo._
  import global.definitions._
  import Cardinality._
  import universeTypes._

  abstract class Reifier extends {
    val global: self.global.type = self.global
    val universe = self.universe
    val reifee = EmptyTree
    val mirror = EmptyTree
    val concrete = false
  } with ReflectReifier {
    lazy val typer = throw new UnsupportedOperationException

    def isReifyingExpressions: Boolean
    def isReifyingPatterns: Boolean = !isReifyingExpressions
    def action = if (isReifyingExpressions) "splice" else "extract"
    def holesHaveTypes = isReifyingExpressions

    def reifyFillingHoles(tree: Tree): Tree = {
      val reified = reifyTree(tree)
      holeMap.unused.foreach { hole =>
        c.abort(holeMap(hole).tree.pos, s"Don't know how to $action here")
      }
      reified
    }

    override def reifyTree(tree: Tree): Tree = {
      val reified =
        reifyTreePlaceholder(tree) orElse
        reifyTreeSyntactically(tree)
      //println(s"reified ${showRaw(tree)} as $reified")
      reified
    }

    def reifyTreePlaceholder(tree: Tree): Tree = tree match {
      case Placeholder(tree, TreeLocation(_), _) if isReifyingExpressions => tree
      case Placeholder(tree, _, NoDot) if isReifyingPatterns => tree
      case Placeholder(tree, _, card @ Dot()) => c.abort(tree.pos, s"Can't $action with $card here")
      case TuplePlaceholder(args) => reifyTuple(args)
      case TupleTypePlaceholder(args) => reifyTupleType(args)
      case CasePlaceholder(tree, location, _) => reifyCase(tree, location)
      case ClassPlaceholder(tree) => reifyClass(tree)
      case _ => EmptyTree
    }

    override def reifyName(name: Name): Tree = name match {
      case Placeholder(tree, location, _) =>
        if (holesHaveTypes && !(location.tpe <:< nameType)) c.abort(tree.pos, s"$nameType expected but ${location.tpe} found")
        tree
      case _ =>
        super.reifyName(name)
    }

    def reifyCase(tree: Tree, location: Location) = {
      if (holesHaveTypes && !(location.tpe <:< caseDefType)) c.abort(tree.pos, s"$caseDefType expected but ${location.tpe} found")
      tree
    }

    def reifyTuple(args: List[Tree]) = args match {
      case Nil => reify(Literal(Constant(())))
      case List(hole @ Placeholder(_, _, NoDot)) => reify(hole)
      case List(Placeholder(_, _, _)) => reifyBuildCall(nme.TupleN, args)
      // in a case we only have one element tuple without
      // any cardinality annotations this means that this is
      // just an expression wrapped in parentheses
      case List(other) => reify(other)
      case _ => reifyBuildCall(nme.TupleN, args)
    }

    def reifyTupleType(args: List[Tree]) = args match {
      case Nil => reify(Select(Ident(nme.scala_), tpnme.Unit))
      case List(hole @ Placeholder(_, _, NoDot)) => reify(hole)
      case List(Placeholder(_, _, _)) => reifyBuildCall(nme.TupleTypeN, args)
      case List(other) => reify(other)
      case _ => reifyBuildCall(nme.TupleTypeN, args)
    }

    def reifyClass(tree: Tree) = {
      val SyntacticClassDef(mods, name, tparams, constrmods, argss, parents, selfval, body) = tree
      reifyBuildCall(nme.SyntacticClassDef, mods, name, tparams, constrmods, argss, parents, selfval, body)
    }

    /** Splits list into a list of groups where subsequent elements are considered
     *  similar by the corresponding function.
     *
     *  Example:
     *
     *    > group(List(1, 1, 0, 0, 1, 0)) { _ == _ }
     *    List(List(1, 1), List(0, 0), List(1), List(0))
     *
     */
    def group[T](lst: List[T])(similar: (T, T) => Boolean) = lst.foldLeft[List[List[T]]](List()) {
      case (Nil, el) => List(List(el))
      case (ll :+ (last @ (lastinit :+ lastel)), el) if similar(lastel, el) => ll :+ (last :+ el)
      case (ll, el) => ll :+ List(el)
    }

    /** Reifies list filling all the valid holeMap.
     *
     *  Reification of non-trivial list is done in two steps:
     *
     *  1. split the list into groups where every placeholder is always
     *     put in a group of it's own and all subsquent non-holeMap are
     *     grouped together; element is considered to be a placeholder if it's
     *     in the domain of the fill function;
     *
     *  2. fold the groups into a sequence of lists added together with ++ using
     *     fill reification for holeMap and fallback reification for non-holeMap.
     *
     *  Example:
     *
     *    reifyMultiCardinalityList(lst) {
     *      // first we define patterns that extract high-cardinality holeMap (currently ..)
     *      case Placeholder(CorrespondsTo(tree, tpe)) if tpe <:< iterableTreeType => tree
     *    } {
     *      // in the end we define how single elements are reified, typically with default reify call
     *      reify(_)
     *    }
     *
     *  Sample execution of previous concrete list reifier:
     *
     *    > val lst = List(foo, bar, qq$f3948f9s$1)
     *    > reifyMultiCardinalityList(lst) { ... } { ... }
     *    q"List($foo, $bar) ++ ${holeMap(qq$f3948f9s$1).tree}"
     */
    def reifyMultiCardinalityList[T](xs: List[T])(fill: PartialFunction[T, Tree])(fallback: T => Tree): Tree

    /** Reifies arbitrary list filling ..$x and ...$y holeMap when they are put
     *  in the correct position. Fallbacks to regular reification for non-high cardinality
     *  elements.
     */
    override def reifyList(xs: List[Any]): Tree = reifyMultiCardinalityList(xs) {
      case Placeholder(tree, _, DotDot) => tree
      case CasePlaceholder(tree, _, DotDot) => tree
      case List(Placeholder(tree, _, DotDotDot)) => tree
    } {
      reify(_)
    }

    def reifyAnnotList(annots: List[Tree]): Tree

    def ensureNoExplicitFlags(m: Modifiers, pos: Position) =
      if ((m.flags & ExplicitFlags) != 0L) c.abort(pos, s"Can't $action modifiers together with flags, consider merging flags into modifiers")

    override def mirrorSelect(name: String): Tree =
      Select(universe, TermName(name))

    override def mirrorCall(name: TermName, args: Tree*): Tree =
      Apply(Select(universe, name), args.toList)

    override def mirrorBuildCall(name: TermName, args: Tree*): Tree =
      Apply(Select(Select(universe, nme.build), name), args.toList)
  }

  class ApplyReifier extends Reifier {
    def isReifyingExpressions = true

    override def reifyTreeSyntactically(tree: Tree): Tree = tree match {
      case Block(stats, p @ Placeholder(_, _, _)) => reifyBuildCall(nme.Block, stats :+ p)
      case Apply(f, List(Placeholder(argss, _, DotDotDot))) => reifyCallWithArgss(f, argss)
      case RefTree(qual, SymbolPlaceholder(tree)) => mirrorBuildCall(nme.RefTree, reify(qual), tree)
      case _ => super.reifyTreeSyntactically(tree)
    }

    def reifyCallWithArgss(f: Tree, argss: Tree) = {
      val f1 = reifyTree(f)
      val foldLeftF1 = Apply(TypeApply(Select(argss, nme.foldLeft), List(Select(u, tpnme.Tree))), List(f1))
      val uDotApply = Function(
        List(gen.mkSyntheticParam(nme.x_1), gen.mkSyntheticParam(nme.x_2)),
        Apply(Select(u, nme.Apply), List(Ident(nme.x_1), Ident(nme.x_2))))
      Apply(foldLeftF1, List(uDotApply))
    }

    override def reifyMultiCardinalityList[T](xs: List[T])(fill: PartialFunction[T, Tree])(fallback: T => Tree): Tree = xs match {
      case Nil => mkList(Nil)
      case _ =>
        def reifyGroup(group: List[T]): Tree = group match {
          case List(elem) if fill.isDefinedAt(elem) => fill(elem)
          case elems => mkList(elems.map(fallback))
        }
        val head :: tail = group(xs) { (a, b) => !fill.isDefinedAt(a) && !fill.isDefinedAt(b) }
        tail.foldLeft[Tree](reifyGroup(head)) { (tree, lst) => Apply(Select(tree, nme.PLUSPLUS), List(reifyGroup(lst))) }
    }

    override def reifyAnnotList(annots: List[Tree]): Tree = reifyMultiCardinalityList(annots) {
      case AnnotPlaceholder(tree, _, DotDot, args) =>
        val x: TermName = c.freshName()
        val xToAnnotationCtor = Function(
          List(ValDef(Modifiers(PARAM), x, TypeTree(), EmptyTree)),
          mirrorBuildCall(nme.mkAnnotationCtor, Ident(x), reify(args)))
        Apply(Select(tree, nme.map), List(xToAnnotationCtor))
    } {
      case AnnotPlaceholder(tree, _: TreeLocation, _, args) =>
        mirrorBuildCall(nme.mkAnnotationCtor, tree, reify(args))
      case other => reify(other)
    }

    override def reifyModifiers(m: Modifiers) = {
      val (modsPlaceholders, annots) = m.annotations.partition {
        case ModsPlaceholder(_, _, _) => true
        case _ => false
      }
      val (mods, flags) = modsPlaceholders.map {
        case ModsPlaceholder(tree, location, card) => (tree, location)
      }.partition { case (tree, location) =>
        location match {
          case ModsLocation => true
          case FlagsLocation => false
          case _ => c.abort(tree.pos, s"$flagsType or $modsType expected but ${tree.tpe} found")
        }
      }
      mods match {
        case (tree, _) :: Nil =>
          if (flags.nonEmpty) c.abort(flags(0)._1.pos, "Can't splice flags together with modifiers, consider merging flags into modifiers")
          if (annots.nonEmpty) c.abort(tree.pos, "Can't splice modifiers together with annotations, consider merging annotations into modifiers")
          ensureNoExplicitFlags(m, tree.pos)
          tree
        case _ :: (second, _) :: Nil =>
          c.abort(second.pos, "Can't splice multiple modifiers, consider merging them into a single modifiers instance")
        case _ =>
          val baseFlags = reifyBuildCall(nme.flagsFromBits, m.flags)
          val reifiedFlags = flags.foldLeft[Tree](baseFlags) { case (flag, (tree, _)) => Apply(Select(flag, nme.OR), List(tree)) }
          mirrorFactoryCall(nme.Modifiers, reifiedFlags, reify(m.privateWithin), reifyAnnotList(annots))
      }
    }
  }

  class UnapplyReifier extends Reifier {
    def isReifyingExpressions = false

    override def reifyTreeSyntactically(tree: Tree): Tree = tree match {
      case treeInfo.Applied(fun, Nil, argss) if fun != tree && !tree.isInstanceOf[AppliedTypeTree] =>
        reifyBuildCall(nme.Applied, fun, argss)
      case treeInfo.Applied(fun, targs, argss) if fun != tree & !tree.isInstanceOf[AppliedTypeTree] =>
        mirrorBuildCall(nme.Applied, reifyBuildCall(nme.TypeApplied, fun, targs), reifyList(argss))
      case _ =>
        super.reifyTreeSyntactically(tree)
    }

    override def scalaFactoryCall(name: String, args: Tree*): Tree =
      call("scala." + name, args: _*)

    override def reifyMultiCardinalityList[T](xs: List[T])(fill: PartialFunction[T, Tree])(fallback: T => Tree) = xs match {
      case init :+ last if fill.isDefinedAt(last) =>
        init.foldRight[Tree](fill(last)) { (el, rest) =>
          val cons = Select(Select(Select(Ident(nme.scala_), nme.collection), nme.immutable), nme.CONS)
          Apply(cons, List(fallback(el), rest))
        }
      case _ =>
        mkList(xs.map(fallback))
    }

    override def reifyAnnotList(annots: List[Tree]): Tree = reifyMultiCardinalityList(annots) {
      case AnnotPlaceholder(tree, _, DotDot, Nil) => tree
    } {
      case AnnotPlaceholder(tree, _, NoDot, Nil) => tree
      case AnnotPlaceholder(tree, _, NoDot, args) =>
        val selectCONSTRUCTOR = Apply(Select(u, nme.Select), List(Apply(Select(u, nme.New), List(tree)), Select(Select(u, nme.nmeNme), nme.nmeCONSTRUCTOR)))
        Apply(Select(u, nme.Apply), List(selectCONSTRUCTOR, reify(args)))
      case other =>
        reify(other)
    }

    override def reifyModifiers(m: Modifiers) = {
      val mods = m.annotations.collect { case ModsPlaceholder(tree, _, _) => tree }
      mods match {
        case tree :: Nil =>
          if (m.annotations.length != 1) c.abort(tree.pos, "Can't extract modifiers together with annotations, consider extracting just modifiers")
          ensureNoExplicitFlags(m, tree.pos)
          tree
        case _ :: second :: rest =>
          c.abort(second.pos, "Can't extract multiple modifiers together, consider extracting a single modifiers instance")
        case Nil =>
          mirrorFactoryCall(nme.Modifiers, reifyBuildCall(nme.FlagsAsBits, m.flags),
                                           reify(m.privateWithin), reifyAnnotList(m.annotations))
      }
    }
  }
}