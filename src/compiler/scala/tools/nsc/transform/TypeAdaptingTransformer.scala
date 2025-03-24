/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package transform

import scala.annotation.tailrec
import scala.tools.nsc.ast.TreeDSL

/**
 * A trait usable by transforms that need to adapt trees of one type to another type
 */
trait TypeAdaptingTransformer { self: TreeDSL =>
  abstract class TypeAdapter {
    import global._
    import definitions._

    def typedPos(pos: Position)(tree: Tree): Tree

    /**
     * scala/bug#4148: can't always replace box(unbox(x)) by x because
     *   - unboxing x may lead to throwing an exception, e.g. in "aah".asInstanceOf[Int]
     *   - box(unbox(null)) is not `null` but the box of zero
     */
    private def isSafelyRemovableUnbox(fn: Tree, arg: Tree): Boolean = {
      currentRun.runDefinitions.isUnbox(fn.symbol) && {
        // replace box(unbox(null)) by null when passed to the super constructor in a specialized
        // class, see comment in SpecializeTypes.forwardCtorCall.
        arg.hasAttachment[specializeTypes.SpecializedSuperConstructorCallArgument.type] ||
          isBoxedValueClass(arg.tpe.typeSymbol)
      }
    }

    final def isPrimitiveValueMember(sym: Symbol)    = isPrimitiveValueClass(sym.owner)
    final def isMethodTypeWithEmptyParams(tpe: Type) = tpe.isInstanceOf[MethodType] && tpe.params.isEmpty
    final def applyMethodWithEmptyParams(qual: Tree) = Apply(qual, List()) setPos qual.pos setType qual.tpe.resultType

    import CODE._

    /** Box `tree` of unboxed type */
    final def box(tree: Tree): Tree = tree match {
      case LabelDef(_, _, _) =>
        val ldef = deriveLabelDef(tree)(box)
        ldef setType ldef.rhs.tpe
      case Apply(fun @ Ident(_), _) if fun.symbol.isLabel =>
        // don't box around label jumps, scala/bug#13043
        // need to set the tree type to avoid looping in `adaptToType`
        tree.setType(tree.tpe match {
          case ErasedValueType(clazz, _) => clazz.tpe
          case _ => ObjectTpe
        })
      case _ =>
        val tree1 = tree.tpe match {
          case ErasedValueType(clazz, _) => New(clazz, cast(tree, underlyingOfValueClass(clazz)))
          case _ => tree.tpe.typeSymbol match {
            case UnitClass =>
              if (treeInfo isExprSafeToInline tree) REF(BoxedUnit_UNIT)
              else BLOCK(tree, REF(BoxedUnit_UNIT))
            case x =>
              assert(x != ArrayClass, "array")
              tree match {
                case Apply(_, List(arg)) if isSafelyRemovableUnbox(tree, arg) =>
                  arg
                case _ =>
                  (REF(currentRun.runDefinitions.boxMethod(x)) APPLY tree) setPos (tree.pos) setType ObjectTpe
              }
          }
        }
        typedPos(tree.pos)(tree1)
    }

    /** Unbox `tree` of boxed type to expected type `pt`.
     *
     *  @param tree the given tree
     *  @param pt   the expected type.
     *  @return     the unboxed tree
     */
    final def unbox(tree: Tree, pt: Type): Tree = tree match {
      case LabelDef(_, _, _) =>
        val ldef = deriveLabelDef(tree)(unbox(_, pt))
        ldef setType ldef.rhs.tpe
      case _ =>
        def preservingSideEffects(side: Tree, value: Tree): Tree =
          if (treeInfo isExprSafeToInline side) value
          else BLOCK(side, value)
        val tree1 = pt match {
          case ErasedValueType(_, BoxedUnitTpe) => cast(preservingSideEffects(tree, REF(BoxedUnit_UNIT)), pt)
          case ErasedValueType(clazz, underlying) => cast(unboxValueClass(tree, clazz, underlying), pt)
          case _ =>
            pt.typeSymbol match {
              case UnitClass  =>
                preservingSideEffects(tree, UNIT)
              case ArrayClass => assert(pt.typeSymbol != ArrayClass, "array") ; tree
              case _          =>
                val unboxer = currentRun.runDefinitions.unboxMethod(pt.typeSymbol)
                if (settings.isDeveloper) assert(boxedClass(pt.typeSymbol).tpe <:< tree.tpe, s"${tree.tpe} is not a boxed ${pt}")
                Apply(unboxer, tree)  // don't `setType pt` the Apply tree, as the Apply's fun won't be typechecked if the Apply tree already has a type
            }
        }
        typedPos(tree.pos)(tree1)
    }

    final def unboxValueClass(tree: Tree, clazz: Symbol, underlying: Type): Tree =
      if (tree.tpe.typeSymbol == NullClass && isPrimitiveValueClass(underlying.typeSymbol)) {
        // convert `null` directly to underlying type, as going via the unboxed type would yield a NPE (see scala/bug#5866)
        unbox(tree, underlying)
      } else
        Apply(Select(adaptToType(tree, clazz.tpe), clazz.derivedValueClassUnbox), List())

    /** Generate a synthetic cast operation from tree.tpe to pt.
      *
      *  @note Pre-condition: pt eq pt.normalize
     */
    final def cast(tree: Tree, pt: Type): Tree = {
      if (settings.isDebug && (tree.tpe ne null) && !(tree.tpe =:= ObjectTpe)) {
        def word =
          if (tree.tpe <:< pt) "upcast"
          else if (pt <:< tree.tpe) "downcast"
          else if (pt weak_<:< tree.tpe) "coerce"
          else if (tree.tpe weak_<:< pt) "widen"
          else "cast"
        log(s"erasure ${word}s from ${tree.tpe} to $pt")
      }
      if (pt =:= UnitTpe) {
        // See scala/bug#4731 for one example of how this occurs.
        // TODO: that initial fix was quite symptomatic (the real problem was that it allowed an illegal override,
        // which resulted in types being so out of whack that'd case something to unit where we shouldn't),
        // so I'm not sure this case actually still arises.
        log("Attempted to cast to Unit: " + tree)
        tree.duplicate setType pt
      } else if (tree.tpe != null && tree.tpe.typeSymbol == ArrayClass && pt.typeSymbol == ArrayClass) {
        // See scala/bug#2386 for one example of when this might be necessary.
        val needsExtraCast = isPrimitiveValueType(tree.tpe.typeArgs.head) && !isPrimitiveValueType(pt.typeArgs.head)
        val tree1 = if (needsExtraCast) gen.mkRuntimeCall(nme.toObjectArray, List(tree)) else tree
        gen.mkAttributedCast(tree1, pt)
      } else {
        tree match {
          case ld: LabelDef =>
            // Push the cast into the RHS of matchEnd LabelDefs.
            ld.symbol.modifyInfo {
              case MethodType(params, _) => MethodType(params, pt)
              case x                     => throw new MatchError(x)
            }
            deriveLabelDef(ld)(rhs => cast(rhs, pt)).setType(pt)
          case _ =>
            gen.mkAttributedCast(tree, pt)
        }
      }
    }

    /** Adapt `tree` to expected type `pt`.
     *
     *  @param tree the given tree
     *  @param pt   the expected type
     *  @return     the adapted tree
     */
    @tailrec final def adaptToType(tree: Tree, pt: Type): Tree = {
      val tpe = tree.tpe
      if ((tpe eq pt) || tpe <:< pt) tree
      else if (tpe.isInstanceOf[ErasedValueType]) adaptToType(box(tree), pt) // what if pt is an erased value type?
      else if (pt.isInstanceOf[ErasedValueType])  adaptToType(unbox(tree, pt), pt)
      // See corresponding case in `Eraser`'s `adaptMember`
      // [H] this does not hold here, however: `assert(tree.symbol.isStable)` (when typechecking !(SomeClass.this.bitmap) for single lazy val)
      else if (isMethodTypeWithEmptyParams(tpe))  adaptToType(applyMethodWithEmptyParams(tree), pt)
      else {
        val gotPrimitiveVC      = isPrimitiveValueType(tpe)
        val expectedPrimitiveVC = isPrimitiveValueType(pt)

        if (gotPrimitiveVC && !expectedPrimitiveVC)      adaptToType(box(tree), pt)
        else if (!gotPrimitiveVC && expectedPrimitiveVC) adaptToType(unbox(tree, pt), pt)
        else cast(tree, pt)
      }
    }
  }
}
