package scala.tools.reflect
package quasiquotes

import scala.tools.nsc.Global
import scala.reflect.reify.{Reifier => ReflectReifier}
import scala.reflect.macros
import scala.collection.{immutable, mutable}
import scala.reflect.internal.Flags._

trait Reifiers { self: Quasiquotes =>
  import global._
  import global.build.SyntacticClassDef
  import global.treeInfo._
  import global.definitions._
  import Cardinality._

  case class Hole(tree: Tree, cardinality: Cardinality)
  case class Holes(underlying: immutable.ListMap[String, Hole]) {
    val accessed = mutable.Set[String]()
    def keys = underlying.keys
    def contains(key: String) = underlying.contains(key)
    def apply(key: String) = {
      accessed += key
      underlying(key)
    }
    def get(key: String) = {
      accessed += key
      underlying.get(key)
    }
  }

  abstract class Reifier(val universe: Tree, val holes: Holes) extends {
    val global: self.global.type = self.global
    val mirror = EmptyTree
    val typer = null
    val reifee = null
    val concrete = false

  } with ReflectReifier with Types {
    // shortcut
    val u = universe

    /** Extractor that matches simple identity-like trees which
     *  correspond to holes within quasiquote.
     */
    object Placeholder {
      def unapply(tree: Tree): Option[String] = tree match {
        case Ident(PlaceholderName(name)) => Some(name)
        case TypeDef(_, PlaceholderName(name), List(), TypeBoundsTree(EmptyTree, EmptyTree)) => Some(name)
        case ValDef(_, PlaceholderName(name), TypeTree(), EmptyTree) => Some(name)
        case _ => None
      }
    }

    object PlaceholderName {
      def unapply(name: Name): Option[String] =
        if (holes.contains(name.toString)) Some(name.toString)
        else None
    }

    object AnnotPlaceholder {
      def unapply(tree: Tree): Option[(String, List[Tree])] = tree match {
        case Apply(Select(New(Placeholder(name)), nme.CONSTRUCTOR), args) => Some((name, args))
        case _ => None
      }
    }

    object ModsPlaceholder {
      def unapply(tree: Tree): Option[String] = tree match {
        case Apply(Select(New(Ident(tpnme.QUASIQUOTE_MODS)), nme.CONSTRUCTOR), List(Literal(Constant(s: String)))) => Some(s)
        case _ => None
      }
    }

    def reifyFillingHoles(tree: Tree): Tree = {
      val reified = reifyTree(tree)
      (holes.keys.toSet -- holes.accessed).foreach { hole =>
        c.abort(holes(hole).tree.pos, "Don't know how to splice here")
      }
      reified
    }

    override def reifyTree(tree: Tree): Tree = reifyBasicTree(tree)

    // Inline flags are those flags that have direct equivalent in scala code.
    final val inlineFlags = List(
      PRIVATE, PROTECTED, LAZY, IMPLICIT,
      CASE, FINAL, COVARIANT, CONTRAVARIANT,
      OVERRIDE, SEALED)

    def ensureNoInlineFlags(m: Modifiers, pos: Position, action: String) = {
      val flags = m.flags
      inlineFlags.foreach { f =>
        if ((flags & f) != 0L) c.abort(pos, "Can't $action Modifiers together with inline Flags")
      }
    }

    override def mirrorSelect(name: String): Tree =
      Select(universe, TermName(name))

    override def mirrorCall(name: TermName, args: Tree*): Tree =
      Apply(Select(universe, name), args.toList)

    override def mirrorBuildCall(name: TermName, args: Tree*): Tree =
      Apply(Select(Select(universe, nme.build), name), args.toList)
  }

  class ApplyReifier(universe: Tree, holes: Holes) extends Reifier(universe, holes) {
    def isSupportedZeroCardinalityType(tpe: Type): Boolean =
      tpe <:< treeType || tpe <:< nameType || tpe <:< modsType || tpe <:< flagsType || tpe <:< symbolType

    object CorrespondsTo {
      def unapply(name: Name): Option[(Tree, Type)] = unapply(name.toString)

      def unapply(name: String): Option[(Tree, Type)] =
        holes.get(name).flatMap { case Hole(tree, card) =>
          (card, tree.tpe) match {
            case (NoDot, tpe) if isSupportedZeroCardinalityType(tpe) =>
              Some((tree, tpe))
            case (NoDot, LiftableType(lift)) =>
              Some((wrapLift(lift, tree), treeType))
            case (card, iterable) if card != NoDot && iterable <:< iterableType =>
              val (expected, tpe) = extractIterable(iterable)
              if (expected != card)
                c.abort(tree.pos, s"Expected $expected cardinality but got $card")
              tpe match {
                case tpe if tpe <:< treeType =>
                  if (iterable <:< listTreeType || iterable <:< listListTreeType) Some(tree, iterable)
                  else Some((wrapIterableN(tree, card) { t => t }, iterableN(card, tpe)))
                case LiftableType(lift) =>
                  Some((wrapIterableN(tree, card) { t => wrapLift(lift, t) }, iterableN(card, treeType)))
                case tpe =>
                  c.abort(tree.pos, s"Can't splice an Iterable of non-liftable type $tpe")
              }
            case (card, tpe) =>
              val (itCard, itTpe) = extractIterable(tpe)
              if (itCard != NoDot)
                c.abort(tree.pos, s"To splice $tpe use $itCard cardinality or define a Liftable of this type")
              else if (itCard == NoDot && card != NoDot)
                c.abort(tree.pos, s"To splice $tpe use zero cardinality")
              else
                c.abort(tree.pos, s"Can't splice $tpe as it's neither Liftable nor one of natively supported types")
          }
        }

      def wrapLift(lift: Tree, tree: Tree) = {
        val lifted = Apply(lift, List(u, tree))
        val targetType = Select(u, tpnme.Tree)
        TypeApply(Select(lifted, nme.asInstanceOf_), List(targetType))
      }

      def wrapIterableN(tree: Tree, n: Cardinality)(default: Tree => Tree): Tree =
        if (n == NoDot) default(tree)
        else {
          val x: TermName = c.freshName()
          val wrapped = wrapIterableN(Ident(x), n.pred)(default)
          val xToWrapped = Function(List(ValDef(Modifiers(PARAM), x, TypeTree(), EmptyTree)), wrapped)
          Select(Apply(Select(tree, nme.map), List(xToWrapped)), nme.toList)
        }

      object LiftableType {
        def unapply(tpe: Type): Option[Tree] = {
          val liftType = appliedType(liftableType, List(tpe))
          val lift = c.inferImplicitValue(liftType, silent = true)
          if (lift != EmptyTree) Some(lift)
          else None
        }
      }

      def iterableN(n: Cardinality, tpe: Type): Type =
        if (n == NoDot) tpe
        else appliedType(IterableClass.toType, List(iterableN(n.pred, tpe)))

      def extractIterable(tpe: Type): (Cardinality, Type) =
        if (tpe <:< iterableType) {
          val (card, innerTpe) = extractIterable(tpe.typeArguments(0))
          (card.succ, innerTpe)
        }
        else (NoDot, tpe)
    }

    override def reifyBasicTree(tree: Tree): Tree = tree match {
      case Ident(PlaceholderName(CorrespondsTo(sym, tpe))) if tpe <:< symbolType =>
        Apply(Select(u, nme.Ident), List(sym))
      case Select(tree, PlaceholderName(CorrespondsTo(sym, tpe))) if tpe <:< symbolType =>
        Apply(Select(u, nme.Select), List(reifyTree(tree), sym))
      case Literal(Constant(true)) =>
        Select(Select(u, nme.build), nme.True)
      case Literal(Constant(false)) =>
        Select(Select(u, nme.build), nme.False)
      case Placeholder(CorrespondsTo(tree, tpe)) if tpe <:< treeType =>
        tree
      case AppliedTypeTree(Ident(tpnme.QUASIQUOTE_TUPLE), args) =>
        reifyTupleType(args)
      case Apply(Ident(nme.QUASIQUOTE_TUPLE), args) =>
        reifyTuple(args)
      case Apply(f, List(Placeholder(CorrespondsTo(argss, tpe)))) if tpe <:< iterableIterableTreeType =>
        val f1 = reifyTree(f)
        val foldLeftF1 = Apply(TypeApply(Select(argss, nme.foldLeft), List(Select(u, tpnme.Tree))), List(f1))
        def syntheticParam(name: TermName) = ValDef(Modifiers(PARAM | SYNTHETIC), name, TypeTree(), EmptyTree)
        val uDotApply = Function(
          List(syntheticParam(nme.x_1), syntheticParam(nme.x_2)),
          Apply(Select(u, nme.Apply), List(Ident(nme.x_1), Ident(nme.x_2))))
        Apply(foldLeftF1, List(uDotApply))
      case Block(stats, p @ Placeholder(CorrespondsTo(tree, tpe))) =>
        mirrorBuildCall(nme.Block, reifyList(stats :+ p))
      case SyntacticClassDef(mods, name, tparams, constrmods, argss, parents, selfval, body) =>
        mirrorBuildCall(nme.SyntacticClassDef, reifyModifiers(mods), reifyName(name),
                        reifyList(tparams), reifyModifiers(constrmods), reifyList(argss),
                        reifyList(parents), reifyTree(selfval), reifyList(body))
      case CaseDef(Apply(Ident(nme.QUASIQUOTE_CASE), List(Placeholder(CorrespondsTo(tree, tpe)))), EmptyTree, EmptyTree) =>
        if (!(tpe <:< caseDefType)) c.abort(tree.pos, s"CaseDef expected but $tpe found")
        tree
      case Placeholder(name) if holes(name).cardinality != NoDot =>
        val Hole(tree, card) = holes(name)
        c.abort(tree.pos, s"Can't splice with $card cardinality here")
      case _ =>
        super.reifyBasicTree(tree)
    }

    def reifyTuple(args: List[Tree]) = args match {
      case Nil => reify(Literal(Constant(())))
      case List(hole @ Placeholder(CorrespondsTo(tree, tpe))) if !(tpe <:< iterableType) => reify(hole)
      case List(Placeholder(_)) => mirrorBuildCall(nme.TupleN, reifyList(args))
      case List(other) => reify(other)
      case _ => mirrorBuildCall(nme.TupleN, reifyList(args))
    }

    def reifyTupleType(args: List[Tree]) = args match {
      case Nil => reify(Select(Ident(nme.scala_), tpnme.Unit))
      case List(hole @ Placeholder(CorrespondsTo(tree, tpe))) if !(tpe <:< iterableType) => reify(hole)
      case List(Placeholder(_)) => mirrorBuildCall(nme.TupleTypeN, reifyList(args))
      case List(other) => reify(other)
      case _ => mirrorBuildCall(nme.TupleTypeN, reifyList(args))
    }

    override def reifyName(name: Name): Tree = name match {
      case CorrespondsTo(tree, tpe) =>
        if (tpe <:< nameType) tree
        else c.abort(tree.pos, s"Name expected but ${tpe} found")
      case _ =>
        super.reifyName(name)
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

    /** Reifies list filling all the valid holes.
     *
     *  Reification of non-trivial list is done in two steps:
     *
     *  1. split the list into groups where every placeholder is always
     *     put in a group of it's own and all subsquent non-holes are
     *     grouped together; element is considered to be a placeholder if it's
     *     in the domain of the fill function;
     *
     *  2. fold the groups into a sequence of lists added together with ++ using
     *     fill reification for holes and fallback reification for non-holes.
     *
     *  Example:
     *
     *    reifyListGeneric(lst) {
     *      // first we define patterns that extract high-cardinality holes (currently .. and ...)
     *      case ListHolePattern(...) => listTree
     *    } {
     *      // in the end we define how single elements are reified, typically with default reify call
     *      reify(_)
     *    }
     */
    def reifyListGeneric[T](xs: List[T])(fill: PartialFunction[T, Tree])(fallback: T => Tree): Tree =
      xs match {
        case Nil => mkList(Nil)
        case _ =>
          def reifyGroup(group: List[T]): Tree = group match {
            case List(elem) if fill.isDefinedAt(elem) => fill(elem)
            case elems => mkList(elems.map(fallback))
          }
          val head :: tail = group(xs) { (a, b) => !fill.isDefinedAt(a) && !fill.isDefinedAt(b) }
          // tail.foldLeft[Tree](reifyGroup(head)) { (tree, lst) => q"$tree ++ ${reifyGroup(lst)}" }
          tail.foldLeft[Tree](reifyGroup(head)) { (tree, lst) => Apply(Select(tree, nme.PLUSPLUS), List(reifyGroup(lst))) }
      }

    /** Reifies arbitrary list filling ..$x and ...$y holes when they are put
     *  in the correct position. Fallbacks to regular reification for non-high cardinality
     *  elements.
     */
    override def reifyList(xs: List[Any]): Tree = reifyListGeneric(xs) {
      case Placeholder(CorrespondsTo(tree, tpe)) if tpe <:< iterableTreeType => tree
      case CaseDef(Apply(Ident(nme.QUASIQUOTE_CASE), List(Placeholder(CorrespondsTo(tree, tpe)))), EmptyTree, EmptyTree) if tpe <:< iterableCaseDefType => tree
      case List(Placeholder(CorrespondsTo(tree, tpe))) if tpe <:< iterableIterableTreeType => tree
    } {
      reify(_)
    }

    /** Custom list reifier for annotations. It's required because they have different shape
     *  and additional $u.build.mkAnnotatorCtor wrapping is needed to ensure that user won't
     *  splice a non-constructor call in this position.
     */
    def reifyAnnotsList(annots: List[Tree]): Tree = reifyListGeneric(annots) {
      case AnnotPlaceholder(CorrespondsTo(tree, tpe), args) if tpe <:< iterableTreeType =>
        val x: TermName = c.freshName()
        val xToAnnotationRepr = Function(
          List(ValDef(Modifiers(PARAM), x, TypeTree(), EmptyTree)),
          mirrorBuildCall(nme.mkAnnotatorCtor, Ident(x), reify(args)))
        Apply(Select(tree, nme.map), List(xToAnnotationRepr))
    } {
      case AnnotPlaceholder(CorrespondsTo(tree, tpe), args) if tpe <:< treeType =>
        mirrorBuildCall(nme.mkAnnotatorCtor, tree, reify(args))
      case other => reify(other)
    }

    override def reifyModifiers(m: Modifiers) = {
      val (modsholes, annots) = m.annotations.partition {
        case ModsPlaceholder(_) => true
        case _ => false
      }
      val (mods, flags) = modsholes.map {
        case ModsPlaceholder(CorrespondsTo((tree, tpe))) => (tree, tpe)
      }.partition { case (tree, tpe) =>
        if (tpe <:< modsType) true
        else if (tpe <:< flagsType) false
        else c.abort(tree.pos, "Instance of FlagSet or Modifiers type is expected here but ${tree.tpe} found")
      }
      mods match {
        case (tree, _) :: Nil =>
          if (flags.nonEmpty) c.abort(flags(0)._1.pos, "Can't splice Flags together with Modifiers")
          if (annots.nonEmpty) c.abort(tree.pos, "Can't splice Modifiers together with additional annotations")
          ensureNoInlineFlags(m, tree.pos, "splice")
          tree
        case _ :: (second, _) :: Nil =>
          c.abort(second.pos, "Can't splice multiple Modifiers")
        case _ =>
          val baseFlags = mirrorBuildCall(nme.flagsFromBits, reify(m.flags))
          // val reifiedFlags = flags.foldLeft[Tree](baseFlags) { case (flag, (tree, _)) => q"$flag | $tree" }
          val reifiedFlags = flags.foldLeft[Tree](baseFlags) { case (flag, (tree, _)) => Apply(Select(flag, nme.OR), List(tree)) }
          mirrorFactoryCall(nme.Modifiers, reifiedFlags, reify(m.privateWithin), reifyAnnotsList(annots))
      }
    }
  }

  class UnapplyReifier(universe: Tree, holes: Holes) extends Reifier(universe, holes) {
    object CorrespondsTo {
      def unapply(name: String): Option[(Tree, Cardinality)] =
        holes.get(name).map { case Hole(tree, card) => (tree, card) }
    }

    override def reifyBasicTree(tree: Tree): Tree = tree match {
      case global.emptyValDef =>
        mirrorBuildCall(nme.EmptyValDefLike)
      case global.pendingSuperCall =>
        mirrorBuildCall(nme.PendingSuperCallLike)
      case Placeholder(CorrespondsTo(tree, card)) =>
        if (card != NoDot) c.abort(tree.pos, s"Can't extract a part of the tree with $card cardinality here")
        tree
      case AppliedTypeTree(Ident(tpnme.QUASIQUOTE_TUPLE), args) => reifyTupleType(args)
      case Apply(Ident(nme.QUASIQUOTE_TUPLE), args) => reifyTuple(args)
      case Applied(fun, targs, argss) if fun != tree =>
        if (targs.length > 0)
          mirrorBuildCall(nme.Applied, reify(fun), reifyList(targs), reifyList(argss))
        else
          mirrorBuildCall(nme.Applied2, reify(fun), reifyList(argss))
      case SyntacticClassDef(mods, name, tparams, constrmods, argss, parents, selfval, body) =>
        mirrorBuildCall(nme.SyntacticClassDef, reifyModifiers(mods), reifyName(name),
                        reifyList(tparams), reifyModifiers(constrmods), reifyList(argss),
                        reifyList(parents), reifyTree(selfval), reifyList(body))
      case _ =>
        super.reifyBasicTree(tree)
    }

    def reifyTuple(args: List[Tree]) = args match {
      case Nil => reify(Literal(Constant(())))
      case List(hole @ Placeholder(CorrespondsTo(tree, NoDot))) => reify(hole)
      case List(Placeholder(CorrespondsTo(tree, card))) => mirrorBuildCall(nme.TupleN, reifyList(args))
      case List(other) => reify(other)
      case _ => mirrorBuildCall(nme.TupleN, reifyList(args))
    }

    def reifyTupleType(args: List[Tree]) = args match {
      case Nil => reify(Select(Ident(nme.scala_), tpnme.Unit))
      case List(hole @ Placeholder(CorrespondsTo(tree, NoDot))) => reify(hole)
      case List(Placeholder(CorrespondsTo(tree, card))) => mirrorBuildCall(nme.TupleTypeN, reifyList(args))
      case List(other) => reify(other)
      case _ => mirrorBuildCall(nme.TupleTypeN, reifyList(args))
    }

    override def scalaFactoryCall(name: String, args: Tree*): Tree =
      call("scala." + name, args: _*)

    override def reifyName(name: Name): Tree =
      if (!holes.contains(name.toString)) super.reifyName(name)
      else holes(name.toString).tree

    def reifyListGeneric(xs: List[Any])(fill: PartialFunction[Any, Tree])(fallback: Any => Tree = reify) =
      xs match {
        case init :+ last if fill.isDefinedAt(last) =>
          init.foldRight[Tree](fill(last)) { (el, rest) =>
            val cons = Select(Select(Select(Ident(nme.scala_), nme.collection), nme.immutable), nme.CONS)
            Apply(cons, List(fallback(el), rest))
          }
        case _ =>
          mkList(xs.map(fallback))
      }

    override def reifyList(xs: List[Any]): Tree = reifyListGeneric(xs) {
      case Placeholder(CorrespondsTo(tree, DotDot)) => tree
      case List(Placeholder(CorrespondsTo(tree, DotDotDot))) => tree
    } ()

    def reifyAnnotsList(annots: List[Tree]): Tree = reifyListGeneric(annots) {
      case AnnotPlaceholder(CorrespondsTo(tree, DotDot), Nil) => tree
    } {
      case AnnotPlaceholder(CorrespondsTo(tree, NoDot), args) =>
        args match {
          case Nil => tree
          // case _ => q"$u.Apply($u.Select($u.New($tree), $u.nme.CONSTRUCTOR), ${reify(args)})"
          case _ =>
            val selectCONSTRUCTOR = Apply(Select(u, nme.Select), List(Apply(Select(u, nme.New), List(tree)), Select(Select(u, nme.nmeNme), nme.nmeCONSTRUCTOR)))
            Apply(Select(u, nme.Apply), List(selectCONSTRUCTOR, reify(args)))
        }
      case other =>
        reify(other)
    }

    override def reifyModifiers(m: Modifiers) = {
      val mods = m.annotations.collect {
        case ModsPlaceholder(CorrespondsTo(tree, _)) => tree
      }
      mods match {
        case tree :: Nil =>
          if (m.annotations.length != 1) c.abort(tree.pos, "Can't extract Modifiers together with additional annotations")
          ensureNoInlineFlags(m, tree.pos, "extract")
          tree
        case _ :: second :: rest =>
          c.abort(second.pos, "Can't extract multiple Modifiers")
        case Nil =>
          mirrorFactoryCall(nme.Modifiers, mirrorBuildCall(nme.FlagsAsBits, reify(m.flags)),
                                           reify(m.privateWithin), reifyAnnotsList(m.annotations))
      }
    }
  }

  trait Types {
    val universe: Tree

    lazy val universeType = universe.tpe
    lazy val nameType = memberType(universeType, tpnme.Name)
    lazy val termNameType = memberType(universeType, tpnme.TypeName)
    lazy val typeNameType = memberType(universeType, tpnme.TermName)
    lazy val modsType = memberType(universeType, tpnme.Modifiers)
    lazy val flagsType = memberType(universeType, tpnme.FlagSet)
    lazy val symbolType = memberType(universeType, tpnme.Symbol)
    lazy val treeType = memberType(universeType, tpnme.Tree)
    lazy val typeDefType = memberType(universeType, tpnme.TypeDef)
    lazy val caseDefType = memberType(universeType, tpnme.CaseDef)
    lazy val liftableType = LiftableClass.toType
    lazy val iterableType = appliedType(IterableClass.toType, List(AnyTpe))
    lazy val iterableTreeType = appliedType(iterableType, List(treeType))
    lazy val iterableCaseDefType = appliedType(iterableType, List(caseDefType))
    lazy val iterableIterableTreeType = appliedType(iterableType, List(iterableTreeType))
    lazy val listType = appliedType(ListClass.toType, List(AnyTpe))
    lazy val listTreeType = appliedType(listType, List(treeType))
    lazy val listListTreeType = appliedType(listType, List(listTreeType))
    lazy val optionTreeType = appliedType(OptionClass.toType, List(treeType))
    lazy val optionNameType = appliedType(OptionClass.toType, List(nameType))
  }

  def memberType(thistype: Type, name: TypeName): Type = {
    val sym = thistype.typeSymbol.typeSignature.member(name)
    sym.asType.toType.typeConstructor.asSeenFrom(thistype, sym.owner)
  }
}