package scala.tools.reflect
package quasiquotes

import java.lang.UnsupportedOperationException
import scala.reflect.reify.{Reifier => ReflectReifier}
import scala.reflect.internal.Flags._

trait Reifiers { self: Quasiquotes =>
  import global._
  import global.build.{Select => _, Ident => _, TypeTree => _, _}
  import global.treeInfo._
  import global.definitions._
  import Cardinality._
  import universeTypes._

  abstract class Reifier(val isReifyingExpressions: Boolean) extends {
    val global: self.global.type = self.global
    val universe = self.universe
    val reifee = EmptyTree
    val mirror = EmptyTree
    val concrete = false
  } with ReflectReifier {
    lazy val typer = throw new UnsupportedOperationException

    def isReifyingPatterns: Boolean = !isReifyingExpressions
    def action = if (isReifyingExpressions) "splice" else "extract"
    def holesHaveTypes = isReifyingExpressions

    /** Map that stores freshly generated names linked to the corresponding names in the reified tree.
     *  This information is used to reify names created by calls to freshTermName and freshTypeName.
     */
    var nameMap = collection.mutable.HashMap.empty[Name, Set[TermName]].withDefault { _ => Set() }

    /** Wraps expressions into:
     *    a sequence of nested withFreshTermName/withFreshTypeName calls which are required
     *    to force regeneration of randomly generated names on every evaluation of quasiquote.
     *
     *  Wraps patterns into:
     *    a call into anonymous class' unapply method required by unapply macro expansion:
     *
     *      new {
     *        def unapply(tree) = tree match {
     *          case pattern if guard => Some(result)
     *          case _ => None
     *        }
     *      }.unapply(<unapply-selector>)
     *
     *    where pattern corresponds to reified tree and guard represents conjunction of equalities
     *    which check that pairs of names in nameMap.values are equal between each other.
     */
    def wrap(tree: Tree) =
      if (isReifyingExpressions) {
        nameMap.foldLeft(tree) {
          case (t, (origname, names)) =>
            assert(names.size == 1)
            val FreshName(prefix) = origname
            val ctor = TermName("withFresh" + (if (origname.isTermName) "TermName" else "TypeName"))
            // q"$u.build.$ctor($prefix) { ${names.head} => $t }"
            Apply(Apply(Select(Select(u, nme.build), ctor), List(Literal(Constant(prefix)))),
              List(Function(List(ValDef(Modifiers(PARAM), names.head, TypeTree(), EmptyTree)), t)))
        }
      } else {
        val freevars = holeMap.toList.map { case (name, _) => Ident(name) }
        val isVarPattern = tree match { case Bind(name, Ident(nme.WILDCARD)) => true case _ => false }
        val cases =
          if(isVarPattern) {
            val Ident(name) :: Nil = freevars
            // cq"$name: $treeType => $SomeModule($name)" :: Nil
            CaseDef(Bind(name, Typed(Ident(nme.WILDCARD), TypeTree(treeType))),
              EmptyTree, Apply(Ident(SomeModule), List(Ident(name)))) :: Nil
          } else {
            val (succ, fail) = freevars match {
              case Nil =>
                // (q"true", q"false")
                (Literal(Constant(true)), Literal(Constant(false)))
              case head :: Nil =>
                // (q"$SomeModule($head)", q"$NoneModule")
                (Apply(Ident(SomeModule), List(head)), Ident(NoneModule))
              case vars =>
                // (q"$SomeModule((..$vars))", q"$NoneModule")
                (Apply(Ident(SomeModule), List(SyntacticTuple(vars))), Ident(NoneModule))
            }
            val guard =
              nameMap.collect { case (_, nameset) if nameset.size >= 2 =>
                nameset.toList.sliding(2).map { case List(n1, n2) =>
                  // q"$n1 == $n2"
                  Apply(Select(Ident(n1), nme.EQ), List(Ident(n2)))
                }
              }.flatten.reduceOption[Tree] { (l, r) =>
                // q"$l && $r"
                Apply(Select(l, nme.ZAND), List(r))
              }.getOrElse { EmptyTree }
            // cq"$tree if $guard => $succ" :: cq"_ => $fail" :: Nil
            CaseDef(tree, guard, succ) :: CaseDef(Ident(nme.WILDCARD), EmptyTree, fail) :: Nil
          }
        // q"new { def unapply(tree: $AnyClass) = { ..${unlifters.preamble()}; tree match { case ..$cases } } }.unapply(..$args)"
        Apply(
          Select(
            SyntacticNew(Nil, Nil, noSelfType, List(
              DefDef(NoMods, nme.unapply, Nil, List(List(ValDef(NoMods, nme.tree, TypeTree(AnyClass.toType), EmptyTree))), TypeTree(),
                SyntacticBlock(unlifters.preamble() :+ Match(Ident(nme.tree), cases))))),
            nme.unapply),
          args)
      }

    def reifyFillingHoles(tree: Tree): Tree = {
      val reified = reifyTree(tree)
      holeMap.unused.foreach { hole =>
        c.abort(holeMap(hole).pos, s"Don't know how to $action here")
      }
      wrap(reified)
    }

    override def reifyTree(tree: Tree): Tree =
      reifyTreePlaceholder(tree) orElse
      reifyTreeSyntactically(tree)

    def reifyTreePlaceholder(tree: Tree): Tree = tree match {
      case Placeholder(hole: ApplyHole) if hole.tpe <:< treeType => hole.tree
      case Placeholder(Hole(tree, NoDot)) if isReifyingPatterns => tree
      case Placeholder(hole @ Hole(_, card @ Dot())) => c.abort(hole.pos, s"Can't $action with $card here")
      case TuplePlaceholder(args) => reifyTuple(args)
      case TupleTypePlaceholder(args) => reifyTupleType(args)
      case FunctionTypePlaceholder(argtpes, restpe) => reifyFunctionType(argtpes, restpe)
      case CasePlaceholder(hole) => hole.tree
      case RefineStatPlaceholder(hole) => reifyRefineStat(hole)
      case EarlyDefPlaceholder(hole) => reifyEarlyDef(hole)
      case PackageStatPlaceholder(hole) => reifyPackageStat(hole)
      // for enumerators are checked not during splicing but during
      // desugaring of the for loop in SyntacticFor & SyntacticForYield
      case ForEnumPlaceholder(hole) => hole.tree
      case _ => EmptyTree
    }

    override def reifyTreeSyntactically(tree: Tree) = tree match {
      case RefTree(qual, SymbolPlaceholder(Hole(tree, _))) if isReifyingExpressions =>
        mirrorBuildCall(nme.RefTree, reify(qual), tree)
      case This(SymbolPlaceholder(Hole(tree, _))) if isReifyingExpressions =>
        mirrorCall(nme.This, tree)
      case SyntacticTraitDef(mods, name, tparams, earlyDefs, parents, selfdef, body) =>
        reifyBuildCall(nme.SyntacticTraitDef, mods, name, tparams, earlyDefs, parents, selfdef, body)
      case SyntacticClassDef(mods, name, tparams, constrmods, vparamss, earlyDefs, parents, selfdef, body) =>
        reifyBuildCall(nme.SyntacticClassDef, mods, name, tparams, constrmods, vparamss,
                                              earlyDefs, parents, selfdef, body)
      case SyntacticPackageObjectDef(name, earlyDefs, parents, selfdef, body) =>
        reifyBuildCall(nme.SyntacticPackageObjectDef, name, earlyDefs, parents, selfdef, body)
      case SyntacticObjectDef(mods, name, earlyDefs, parents, selfdef, body) =>
        reifyBuildCall(nme.SyntacticObjectDef, mods, name, earlyDefs, parents, selfdef, body)
      case SyntacticNew(earlyDefs, parents, selfdef, body) =>
        reifyBuildCall(nme.SyntacticNew, earlyDefs, parents, selfdef, body)
      case SyntacticDefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        reifyBuildCall(nme.SyntacticDefDef, mods, name, tparams, vparamss, tpt, rhs)
      case SyntacticValDef(mods, name, tpt, rhs) if tree != noSelfType =>
        reifyBuildCall(nme.SyntacticValDef, mods, name, tpt, rhs)
      case SyntacticVarDef(mods, name, tpt, rhs) =>
        reifyBuildCall(nme.SyntacticVarDef, mods, name, tpt, rhs)
      case SyntacticValFrom(pat, rhs) =>
        reifyBuildCall(nme.SyntacticValFrom, pat, rhs)
      case SyntacticValEq(pat, rhs) =>
        reifyBuildCall(nme.SyntacticValEq, pat, rhs)
      case SyntacticFilter(cond) =>
        reifyBuildCall(nme.SyntacticFilter, cond)
      case SyntacticFor(enums, body) =>
        reifyBuildCall(nme.SyntacticFor, enums, body)
      case SyntacticForYield(enums, body) =>
        reifyBuildCall(nme.SyntacticForYield, enums, body)
      case SyntacticAssign(lhs, rhs) =>
        reifyBuildCall(nme.SyntacticAssign, lhs, rhs)
      case SyntacticApplied(fun, argss) if argss.nonEmpty =>
        reifyBuildCall(nme.SyntacticApplied, fun, argss)
      case SyntacticTypeApplied(fun, targs) if targs.nonEmpty =>
        reifyBuildCall(nme.SyntacticTypeApplied, fun, targs)
      case SyntacticFunction(args, body) =>
        reifyBuildCall(nme.SyntacticFunction, args, body)
      case SyntacticIdent(name, isBackquoted) =>
        reifyBuildCall(nme.SyntacticIdent, name, isBackquoted)
      case Block(Nil, Placeholder(Hole(tree, DotDot))) =>
        mirrorBuildCall(nme.SyntacticBlock, tree)
      case Block(Nil, other) =>
        reifyTree(other)
      case Block(stats, last) =>
        reifyBuildCall(nme.SyntacticBlock, stats :+ last)
      case Try(block, catches, finalizer) =>
        reifyBuildCall(nme.SyntacticTry, block, catches, finalizer)
      case Match(selector, cases) =>
        reifyBuildCall(nme.SyntacticMatch, selector, cases)
      // parser emits trees with scala package symbol to ensure
      // that some names hygienically point to various scala package
      // members; we need to preserve this symbol to preserve
      // correctness of the trees produced by quasiquotes
      case Select(id @ Ident(nme.scala_), name) if id.symbol == ScalaPackage =>
        reifyBuildCall(nme.ScalaDot, name)
      case _ =>
        super.reifyTreeSyntactically(tree)
    }

    override def reifyName(name: Name): Tree = name match {
      case Placeholder(hole: ApplyHole) =>
        if (!(hole.tpe <:< nameType)) c.abort(hole.pos, s"$nameType expected but ${hole.tpe} found")
        hole.tree
      case Placeholder(hole: UnapplyHole) => hole.treeNoUnlift
      case FreshName(prefix) if prefix != nme.QUASIQUOTE_NAME_PREFIX =>
        def fresh() = c.freshName[TermName](nme.QUASIQUOTE_NAME_PREFIX)
        def introduceName() = { val n = fresh(); nameMap(name) += n; n}
        def result(n: Name) = if (isReifyingExpressions) Ident(n) else Bind(n, Ident(nme.WILDCARD))
        if (isReifyingPatterns) result(introduceName())
        else result(nameMap.get(name).map { _.head }.getOrElse { introduceName() })
      case _ =>
        super.reifyName(name)
    }

    def reifyTuple(args: List[Tree]) = args match {
      case Nil => reify(Literal(Constant(())))
      case List(hole @ Placeholder(Hole(_, NoDot))) => reify(hole)
      case List(Placeholder(_)) => reifyBuildCall(nme.SyntacticTuple, args)
      // in a case we only have one element tuple without
      // any cardinality annotations this means that this is
      // just an expression wrapped in parentheses
      case List(other) => reify(other)
      case _ => reifyBuildCall(nme.SyntacticTuple, args)
    }

    def reifyTupleType(args: List[Tree]) = args match {
      case Nil => reify(Select(Ident(nme.scala_), tpnme.Unit))
      case List(hole @ Placeholder(Hole(_, NoDot))) => reify(hole)
      case List(Placeholder(_)) => reifyBuildCall(nme.SyntacticTupleType, args)
      case List(other) => reify(other)
      case _ => reifyBuildCall(nme.SyntacticTupleType, args)
    }

    def reifyFunctionType(argtpes: List[Tree], restpe: Tree) =
      reifyBuildCall(nme.SyntacticFunctionType, argtpes, restpe)

    def reifyConstructionCheck(name: TermName, hole: Hole) = hole match {
      case _: UnapplyHole => hole.tree
      case _: ApplyHole => mirrorBuildCall(name, hole.tree)
    }

    def reifyRefineStat(hole: Hole) = reifyConstructionCheck(nme.mkRefineStat, hole)

    def reifyEarlyDef(hole: Hole) = reifyConstructionCheck(nme.mkEarlyDef, hole)

    def reifyAnnotation(hole: Hole) = reifyConstructionCheck(nme.mkAnnotation, hole)

    def reifyPackageStat(hole: Hole) = reifyConstructionCheck(nme.mkPackageStat, hole)

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
     *      case Placeholder(IterableType(_, _)) => tree
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
      case Placeholder(Hole(tree, DotDot)) => tree
      case CasePlaceholder(Hole(tree, DotDot)) => tree
      case RefineStatPlaceholder(h @ Hole(_, DotDot)) => reifyRefineStat(h)
      case EarlyDefPlaceholder(h @ Hole(_, DotDot)) => reifyEarlyDef(h)
      case PackageStatPlaceholder(h @ Hole(_, DotDot)) => reifyPackageStat(h)
      case ForEnumPlaceholder(Hole(tree, DotDot)) => tree
      case List(Placeholder(Hole(tree, DotDotDot))) => tree
    } {
      reify(_)
    }

    def reifyAnnotList(annots: List[Tree]): Tree = reifyMultiCardinalityList(annots) {
      case AnnotPlaceholder(h @ Hole(_, DotDot)) => reifyAnnotation(h)
    } {
      case AnnotPlaceholder(h: ApplyHole) if h.tpe <:< treeType => reifyAnnotation(h)
      case AnnotPlaceholder(h: UnapplyHole) if h.cardinality == NoDot => reifyAnnotation(h)
      case other => reify(other)
    }

    // These are explicit flags except those that are used
    // to overload the same tree for two different concepts:
    // - MUTABLE that is used to override ValDef for vars
    // - TRAIT that is used to override ClassDef for traits
    val nonOverloadedExplicitFlags = ExplicitFlags & ~MUTABLE & ~TRAIT

    def ensureNoExplicitFlags(m: Modifiers, pos: Position) = {
      // Traits automatically have ABSTRACT flag assigned to
      // them so in that case it's not an explicit flag
      val flags = if (m.isTrait) m.flags & ~ABSTRACT else m.flags
      if ((flags & nonOverloadedExplicitFlags) != 0L)
        c.abort(pos, s"Can't $action modifiers together with flags, consider merging flags into modifiers")
    }

    override def mirrorSelect(name: String): Tree =
      Select(universe, TermName(name))

    override def mirrorCall(name: TermName, args: Tree*): Tree =
      Apply(Select(universe, name), args.toList)

    override def mirrorBuildCall(name: TermName, args: Tree*): Tree =
      Apply(Select(Select(universe, nme.build), name), args.toList)

    override def scalaFactoryCall(name: String, args: Tree*): Tree =
      call("scala." + name, args: _*)
  }

  class ApplyReifier extends Reifier(isReifyingExpressions = true) {
    def reifyMultiCardinalityList[T](xs: List[T])(fill: PartialFunction[T, Tree])(fallback: T => Tree): Tree =
      if (xs.isEmpty) mkList(Nil)
      else {
        def reifyGroup(group: List[T]): Tree = group match {
          case List(elem) if fill.isDefinedAt(elem) => fill(elem)
          case elems => mkList(elems.map(fallback))
        }
        val head :: tail = group(xs) { (a, b) => !fill.isDefinedAt(a) && !fill.isDefinedAt(b) }
        tail.foldLeft[Tree](reifyGroup(head)) { (tree, lst) => Apply(Select(tree, nme.PLUSPLUS), List(reifyGroup(lst))) }
      }

    override def reifyModifiers(m: Modifiers) =
      if (m == NoMods) super.reifyModifiers(m)
      else {
        val (modsPlaceholders, annots) = m.annotations.partition {
          case ModsPlaceholder(_) => true
          case _ => false
        }
        val (mods, flags) = modsPlaceholders.map {
          case ModsPlaceholder(hole: ApplyHole) => hole
        }.partition { hole =>
          if (hole.tpe <:< modsType) true
          else if (hole.tpe <:< flagsType) false
          else c.abort(hole.pos, s"$flagsType or $modsType expected but ${hole.tpe} found")
        }
        mods match {
          case hole :: Nil =>
            if (flags.nonEmpty) c.abort(flags(0).pos, "Can't splice flags together with modifiers, consider merging flags into modifiers")
            if (annots.nonEmpty) c.abort(hole.pos, "Can't splice modifiers together with annotations, consider merging annotations into modifiers")
            ensureNoExplicitFlags(m, hole.pos)
            hole.tree
          case _ :: hole :: Nil =>
            c.abort(hole.pos, "Can't splice multiple modifiers, consider merging them into a single modifiers instance")
          case _ =>
            val baseFlags = reifyFlags(m.flags)
            val reifiedFlags = flags.foldLeft[Tree](baseFlags) { case (flag, hole) => Apply(Select(flag, nme.OR), List(hole.tree)) }
            mirrorFactoryCall(nme.Modifiers, reifiedFlags, reify(m.privateWithin), reifyAnnotList(annots))
        }
      }

  }
  class UnapplyReifier extends Reifier(isReifyingExpressions = false) {
    def reifyMultiCardinalityList[T](xs: List[T])(fill: PartialFunction[T, Tree])(fallback: T => Tree): Tree = xs match {
      case init :+ last if fill.isDefinedAt(last) =>
        init.foldRight[Tree](fill(last)) { (el, rest) =>
          val cons = Select(Select(Select(Ident(nme.scala_), nme.collection), nme.immutable), nme.CONS)
          Apply(cons, List(fallback(el), rest))
        }
      case _ =>
        mkList(xs.map(fallback))
    }

    override def reifyModifiers(m: Modifiers) =
      if (m == NoMods) super.reifyModifiers(m)
      else {
        val mods = m.annotations.collect { case ModsPlaceholder(hole: UnapplyHole) => hole }
        mods match {
          case hole :: Nil =>
            if (m.annotations.length != 1) c.abort(hole.pos, "Can't extract modifiers together with annotations, consider extracting just modifiers")
            ensureNoExplicitFlags(m, hole.pos)
            hole.treeNoUnlift
          case _ :: hole :: _ =>
            c.abort(hole.pos, "Can't extract multiple modifiers together, consider extracting a single modifiers instance")
          case Nil =>
            mirrorFactoryCall(nme.Modifiers, reifyFlags(m.flags), reify(m.privateWithin), reifyAnnotList(m.annotations))
        }
      }
  }
}
