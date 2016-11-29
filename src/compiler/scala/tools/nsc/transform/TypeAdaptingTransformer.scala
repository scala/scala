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
     * SI-4148: can't always replace box(unbox(x)) by x because
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

    private def isPrimitiveValueType(tpe: Type)      = isPrimitiveValueClass(tpe.typeSymbol)
    final def isPrimitiveValueMember(sym: Symbol)    = isPrimitiveValueClass(sym.owner)
    final def isMethodTypeWithEmptyParams(tpe: Type) = tpe.isInstanceOf[MethodType] && tpe.params.isEmpty
    final def applyMethodWithEmptyParams(qual: Tree) = Apply(qual, List()) setPos qual.pos setType qual.tpe.resultType

    import CODE._

    /** Box `tree` of unboxed type */
    final def box(tree: Tree): Tree = tree match {
      case LabelDef(_, _, _) =>
        val ldef = deriveLabelDef(tree)(box)
        ldef setType ldef.rhs.tpe
      case _ =>
        val tree1 = tree.tpe match {
          case ErasedValueType(clazz, _) => New(clazz, cast(tree, underlyingOfValueClass(clazz)))
          case _ => tree.tpe.typeSymbol match {
            case UnitClass =>
              if (treeInfo isExprSafeToInline tree) REF(BoxedUnit_UNIT)
              else BLOCK(tree, REF(BoxedUnit_UNIT))
            case NothingClass => tree // a non-terminating expression doesn't need boxing
            case x =>
              assert(x != ArrayClass)
              tree match {
                case Apply(boxFun, List(arg)) if isSafelyRemovableUnbox(tree, arg) =>
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
        val tree1 = pt match {
          case ErasedValueType(clazz, underlying) => cast(unboxValueClass(tree, clazz, underlying), pt)
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
        typedPos(tree.pos)(tree1)
    }

    final def unboxValueClass(tree: Tree, clazz: Symbol, underlying: Type): Tree =
      if (tree.tpe.typeSymbol == NullClass && isPrimitiveValueClass(underlying.typeSymbol)) {
        // convert `null` directly to underlying type, as going via the unboxed type would yield a NPE (see SI-5866)
        unbox(tree, underlying)
      } else
        Apply(Select(adaptToType(tree, clazz.tpe), clazz.derivedValueClassUnbox), List())

    /** Generate a synthetic cast operation from tree.tpe to pt.
      *
      *  @pre pt eq pt.normalize
     */
    final def cast(tree: Tree, pt: Type): Tree = {
      if (settings.debug && (tree.tpe ne null) && !(tree.tpe =:= ObjectTpe)) {
        def word =
          if (tree.tpe <:< pt) "upcast"
          else if (pt <:< tree.tpe) "downcast"
          else if (pt weak_<:< tree.tpe) "coerce"
          else if (tree.tpe weak_<:< pt) "widen"
          else "cast"
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
