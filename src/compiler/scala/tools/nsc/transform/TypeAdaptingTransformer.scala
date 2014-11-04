package scala.tools.nsc
package transform

import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.Global

/**
 * A trait usable by transforms that need to adapt trees of one type to another type
 */
trait TypeAdaptingTransformer {
  self: TreeDSL =>

  val analyzer: typechecker.Analyzer { val global: self.global.type }

  trait TypeAdapter {
    val typer: analyzer.Typer
    import global._
    import definitions._
    import CODE._

    def isMethodTypeWithEmptyParams(tpe: Type) = tpe match {
      case MethodType(Nil, _) => true
      case _                  => false
    }

    private def isSafelyRemovableUnbox(fn: Tree, arg: Tree): Boolean = {
     currentRun.runDefinitions.isUnbox(fn.symbol) && {
      val cls = arg.tpe.typeSymbol
      (cls == definitions.NullClass) || isBoxedValueClass(cls)
     }
    }

    private def isPrimitiveValueType(tpe: Type) = isPrimitiveValueClass(tpe.typeSymbol)

    private def isErasedValueType(tpe: Type) = tpe.isInstanceOf[ErasedValueType]

    private def isDifferentErasedValueType(tpe: Type, other: Type) =
      isErasedValueType(tpe) && (tpe ne other)

    def isPrimitiveValueMember(sym: Symbol) = isPrimitiveValueClass(sym.owner)

    @inline def box(tree: Tree, target: => String): Tree = {
      val result = box1(tree)
      if (tree.tpe =:= UnitTpe) ()
      else log(s"boxing ${tree.summaryString}: ${tree.tpe} into $target: ${result.tpe}")
      result
    }

    /** Box `tree` of unboxed type */
    private def box1(tree: Tree): Tree = tree match {
      case LabelDef(_, _, _) =>
        val ldef = deriveLabelDef(tree)(box1)
        ldef setType ldef.rhs.tpe
      case _ =>
        val tree1 = tree.tpe match {
          case ErasedValueType(clazz, _) =>
            New(clazz, cast(tree, underlyingOfValueClass(clazz)))
          case _ =>
            tree.tpe.typeSymbol match {
          case UnitClass  =>
            if (treeInfo isExprSafeToInline tree) REF(BoxedUnit_UNIT)
            else BLOCK(tree, REF(BoxedUnit_UNIT))
          case NothingClass => tree // a non-terminating expression doesn't need boxing
          case x          =>
            assert(x != ArrayClass)
            tree match {
              /* Can't always remove a Box(Unbox(x)) combination because the process of boxing x
               * may lead to throwing an exception.
               *
               * This is important for specialization: calls to the super constructor should not box/unbox specialized
               * fields (see TupleX). (ID)
               */
              case Apply(boxFun, List(arg)) if isSafelyRemovableUnbox(tree, arg) =>
                log(s"boxing an unbox: ${tree.symbol} -> ${arg.tpe}")
                arg
              case _ =>
                (REF(currentRun.runDefinitions.boxMethod(x)) APPLY tree) setPos (tree.pos) setType ObjectTpe
            }
            }
        }
        typer.typedPos(tree.pos)(tree1)
    }

    def unbox(tree: Tree, pt: Type): Tree = {
      val result = unbox1(tree, pt)
      log(s"unboxing ${tree.shortClass}: ${tree.tpe} as a ${result.tpe}")
      result
    }

    /** Unbox `tree` of boxed type to expected type `pt`.
     *
     *  @param tree the given tree
     *  @param pt   the expected type.
     *  @return     the unboxed tree
     */
    private def unbox1(tree: Tree, pt: Type): Tree = tree match {
/*
      case Boxed(unboxed) =>
        println("unbox shorten: "+tree) // this never seems to kick in during build and test; therefore disabled.
        adaptToType(unboxed, pt)
 */
      case LabelDef(_, _, _) =>
        val ldef = deriveLabelDef(tree)(unbox(_, pt))
        ldef setType ldef.rhs.tpe
      case _ =>
        val tree1 = pt match {
          case ErasedValueType(clazz, underlying) =>
            val tree0 =
              if (tree.tpe.typeSymbol == NullClass &&
                  isPrimitiveValueClass(underlying.typeSymbol)) {
                // convert `null` directly to underlying type, as going
                // via the unboxed type would yield a NPE (see SI-5866)
                unbox1(tree, underlying)
              } else
                Apply(Select(adaptToType(tree, clazz.tpe), clazz.derivedValueClassUnbox), List())
            cast(tree0, pt)
          case _ =>
            pt.typeSymbol match {
              case UnitClass  =>
                if (treeInfo isExprSafeToInline tree) UNIT
                else BLOCK(tree, UNIT)
              case x          =>
                assert(x != ArrayClass)
                // don't `setType pt` the Apply tree, as the Apply's fun won't be typechecked if the Apply tree already has a type
                Apply(currentRun.runDefinitions.unboxMethod(pt.typeSymbol), tree)
            }
        }
        typer.typedPos(tree.pos)(tree1)
    }

    /** Generate a synthetic cast operation from tree.tpe to pt.
     *  @pre pt eq pt.normalize
     */
    def cast(tree: Tree, pt: Type): Tree = {
      if ((tree.tpe ne null) && !(tree.tpe =:= ObjectTpe)) {
        def word = (
          if (tree.tpe <:< pt) "upcast"
          else if (pt <:< tree.tpe) "downcast"
          else if (pt weak_<:< tree.tpe) "coerce"
          else if (tree.tpe weak_<:< pt) "widen"
          else "cast"
        )
        log(s"erasure ${word}s from ${tree.tpe} to $pt")
      }
      if (pt =:= UnitTpe) {
        // See SI-4731 for one example of how this occurs.
        log("Attempted to cast to Unit: " + tree)
        tree.duplicate setType pt
      } else if (tree.tpe != null && tree.tpe.typeSymbol == ArrayClass && pt.typeSymbol == ArrayClass) {
        // See SI-2386 for one example of when this might be necessary.
        val needsExtraCast = isPrimitiveValueType(tree.tpe.typeArgs.head) && !isPrimitiveValueType(pt.typeArgs.head)
        val tree1 = if (needsExtraCast) gen.mkRuntimeCall(nme.toObjectArray, List(tree)) else tree
        gen.mkAttributedCast(tree1, pt)
      } else gen.mkAttributedCast(tree, pt)
    }

    /** Adapt `tree` to expected type `pt`.
     *
     *  @param tree the given tree
     *  @param pt   the expected type
     *  @return     the adapted tree
     */
    def adaptToType(tree: Tree, pt: Type): Tree = {
      if (settings.debug && pt != WildcardType)
        log("adapting " + tree + ":" + tree.tpe + " : " +  tree.tpe.parents + " to " + pt)//debug
      if (tree.tpe <:< pt)
        tree
      else if (isDifferentErasedValueType(tree.tpe, pt))
        adaptToType(box(tree, pt.toString), pt)
      else if (isDifferentErasedValueType(pt, tree.tpe))
        adaptToType(unbox(tree, pt), pt)
      else if (isPrimitiveValueType(tree.tpe) && !isPrimitiveValueType(pt)) {
        adaptToType(box(tree, pt.toString), pt)
      } else if (isMethodTypeWithEmptyParams(tree.tpe)) {
        // [H] this assert fails when trying to typecheck tree !(SomeClass.this.bitmap) for single lazy val
        //assert(tree.symbol.isStable, "adapt "+tree+":"+tree.tpe+" to "+pt)
        adaptToType(Apply(tree, List()) setPos tree.pos setType tree.tpe.resultType, pt)
//      } else if (pt <:< tree.tpe)
//        cast(tree, pt)
      } else if (isPrimitiveValueType(pt) && !isPrimitiveValueType(tree.tpe))
        adaptToType(unbox(tree, pt), pt)
      else
        cast(tree, pt)
    }
  }
}
