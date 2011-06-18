/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import scala.tools.nsc.symtab.classfile.ClassfileConstants._
import scala.collection.{ mutable, immutable }
import symtab._
import Flags._

abstract class Erasure extends AddInterfaces
                          with typechecker.Analyzer
                          with TypingTransformers
                          with ast.TreeDSL
{
  import global._
  import definitions._
  import CODE._

  val phaseName: String = "erasure"

  def newTransformer(unit: CompilationUnit): Transformer =
    new ErasureTransformer(unit)

  override def keepsTypeParams = false

// -------- erasure on types --------------------------------------------------------

  /** An extractor object for generic arrays */
  object GenericArray {

    /** Is `tp` an unbounded generic type (i.e. which could be instantiated
     *  with primitive as well as class types)?.
     */
    private def genericCore(tp: Type): Type = tp.normalize match {
      case TypeRef(_, sym, _) if sym.isAbstractType && !sym.owner.isJavaDefined =>
        tp
      case ExistentialType(tparams, restp) =>
        genericCore(restp)
      case _ =>
        NoType
    }

    /** If `tp` is of the form Array[...Array[T]...] where `T` is an abstract type
     *  then Some(N, T) where N is the number of Array constructors enclosing `T`,
     *  otherwise None. Existentials on any level are ignored.
     */
    def unapply(tp: Type): Option[(Int, Type)] = tp.normalize match {
      case TypeRef(_, ArrayClass, List(arg)) =>
        genericCore(arg) match {
          case NoType =>
            unapply(arg) match {
              case Some((level, core)) => Some((level + 1, core))
              case None => None
            }
          case core =>
            Some(1, core)
        }
      case ExistentialType(tparams, restp) =>
        unapply(restp)
      case _ =>
        None
    }
  }

  private def unboundedGenericArrayLevel(tp: Type): Int = tp match {
    case GenericArray(level, core) if !(core <:< AnyRefClass.tpe) => level
    case _ => 0
  }

  // @M #2585 when generating a java generic signature that includes a selection of an inner class p.I,  (p = `pre`, I = `cls`)
  // must rewrite to p'.I, where p' refers to the class that directly defines the nested class I
  // see also #2585 marker in javaSig: there, type arguments must be included (use pre.baseType(cls.owner))
  // requires cls.isClass
  @inline private def rebindInnerClass(pre: Type, cls: Symbol): Type =
    if (cls.owner.isClass) cls.owner.tpe else pre // why not cls.isNestedClass?

  /**   The erasure |T| of a type T. This is:
   *
   *   - For a constant type, itself.
   *   - For a type-bounds structure, the erasure of its upper bound.
   *   - For every other singleton type, the erasure of its supertype.
   *   - For a typeref scala.Array+[T] where T is an abstract type, AnyRef.
   *   - For a typeref scala.Array+[T] where T is not an abstract type, scala.Array+[|T|].
   *   - For a typeref scala.Any or scala.AnyVal, java.lang.Object.
   *   - For a typeref scala.Unit, scala.runtime.BoxedUnit.
   *   - For a typeref P.C[Ts] where C refers to a class, |P|.C.
   *     (Where P is first rebound to the class that directly defines C.)
   *   - For a typeref P.C[Ts] where C refers to an alias type, the erasure of C's alias.
   *   - For a typeref P.C[Ts] where C refers to an abstract type, the
   *     erasure of C's upper bound.
   *   - For a non-empty type intersection (possibly with refinement),
   *     the erasure of its first parent.
   *   - For an empty type intersection, java.lang.Object.
   *   - For a method type (Fs)scala.Unit, (|Fs|)scala#Unit.
   *   - For any other method type (Fs)Y, (|Fs|)|T|.
   *   - For a polymorphic type, the erasure of its result type.
   *   - For the class info type of java.lang.Object, the same type without any parents.
   *   - For a class info type of a value class, the same type without any parents.
   *   - For any other class info type with parents Ps, the same type with
   *     parents |Ps|, but with duplicate references of Object removed.
   *   - for all other types, the type itself (with any sub-components erased)
   */
  object erasure extends TypeMap {
    // Compute the dominant part of the intersection type with given `parents` according to new spec.
    def intersectionDominator(parents: List[Type]): Type =
      if (parents.isEmpty) ObjectClass.tpe
      else {
        val psyms = parents map (_.typeSymbol)
        if (psyms contains ArrayClass) {
          // treat arrays specially
          arrayType(
            intersectionDominator(
              parents filter (_.typeSymbol == ArrayClass) map (_.typeArgs.head)))
        } else {
          // implement new spec for erasure of refined types.
          def isUnshadowed(psym: Symbol) =
            !(psyms exists (qsym => (psym ne qsym) && (qsym isNonBottomSubClass psym)))
          val cs = parents.iterator.filter { p => // isUnshadowed is a bit expensive, so try classes first
            val psym = p.typeSymbol
            psym.initialize
            psym.isClass && !psym.isTrait && isUnshadowed(psym)
          }
          (if (cs.hasNext) cs else parents.iterator.filter(p => isUnshadowed(p.typeSymbol))).next()
        }
      }

    def apply(tp: Type): Type = {
      tp match {
        case ConstantType(_) =>
          tp
        case st: SubType =>
          apply(st.supertype)
        case TypeRef(pre, sym, args) =>
          if (sym == ArrayClass)
            if (unboundedGenericArrayLevel(tp) == 1) ObjectClass.tpe
            else if (args.head.typeSymbol == NothingClass || args.head.typeSymbol == NullClass) arrayType(ObjectClass.tpe)
            else typeRef(apply(pre), sym, args map this)
          else if (sym == AnyClass || sym == AnyValClass || sym == SingletonClass || sym == NotNullClass) erasedTypeRef(ObjectClass)
          else if (sym == UnitClass) erasedTypeRef(BoxedUnitClass)
          else if (sym.isRefinementClass) apply(intersectionDominator(tp.parents))
          else if (sym.isClass) typeRef(apply(rebindInnerClass(pre, sym)), sym, List())  // #2585
          else apply(sym.info) // alias type or abstract type
        case PolyType(tparams, restpe) =>
          apply(restpe)
        case ExistentialType(tparams, restpe) =>
          apply(restpe)
        case mt @ MethodType(params, restpe) =>
          MethodType(
            cloneSymbols(params) map (p => p.setInfo(apply(p.tpe))),
            if (restpe.typeSymbol == UnitClass)
              erasedTypeRef(UnitClass)
            else if (settings.YdepMethTpes.value)
              // this replaces each typeref that refers to an argument by the type `p.tpe` of the actual argument p (p in params)
              apply(mt.resultType(params map (_.tpe)))
            else
              apply(restpe))
        case RefinedType(parents, decls) =>
          apply(intersectionDominator(parents))
        case AnnotatedType(_, atp, _) =>
          apply(atp)
        case ClassInfoType(parents, decls, clazz) =>
          ClassInfoType(
            if (clazz == ObjectClass || isValueClass(clazz)) Nil
            else if (clazz == ArrayClass) List(erasedTypeRef(ObjectClass))
            else removeDoubleObject(parents map this),
            decls, clazz)
        case _ =>
          mapOver(tp)
      }
    }
  }

  private object NeedsSigCollector extends TypeCollector(false) {
    def traverse(tp: Type) {
      if (!result) {
        tp match {
          case st: SubType =>
            traverse(st.supertype)
          case TypeRef(pre, sym, args) =>
            if (sym == ArrayClass) args foreach traverse
            else if (sym.isTypeParameterOrSkolem || sym.isExistentiallyBound || !args.isEmpty) result = true
            else if (sym.isClass) traverse(rebindInnerClass(pre, sym)) // #2585
            else if (!sym.owner.isPackageClass) traverse(pre)
          case PolyType(_, _) | ExistentialType(_, _) =>
            result = true
          case RefinedType(parents, decls) =>
            if (!parents.isEmpty) traverse(parents.head)
          case ClassInfoType(parents, _, _) =>
            parents foreach traverse
          case AnnotatedType(_, atp, _) =>
            traverse(atp)
          case _ =>
            mapOver(tp)
        }
      }
    }
  }

  private def needsJavaSig(tp: Type) = !settings.Ynogenericsig.value && NeedsSigCollector.collect(tp)

  // only refer to type params that will actually make it into the sig, this excludes:
  // * higher-order type parameters
  // * type parameters appearing in method parameters
  // * type members not visible in an enclosing template
  private def isTypeParameterInSig(sym: Symbol, initialSymbol: Symbol) = (
    !sym.isHigherOrderTypeParameter &&
    sym.isTypeParameterOrSkolem && (
      (initialSymbol.enclClassChain.exists(sym isNestedIn _)) ||
      traceSig("isMethod", (initialSymbol, initialSymbol.typeParams)) {
        (initialSymbol.isMethod && initialSymbol.typeParams.contains(sym))
      }
    )
  )

  // Ensure every '.' in the generated signature immediately follows
  // a close angle bracket '>'.  Any which do not are replaced with '$'.
  // This arises due to multiply nested classes in the face of the
  // rewriting explained at rebindInnerClass.   This should be done in a
  // more rigorous way up front rather than catching it after the fact,
  // but that will be more involved.
  private def dotCleanup(sig: String): String = {
    var last: Char = '\0'
    sig map {
      case '.' if last != '>' => last = '.' ; '$'
      case ch                 => last = ch ; ch
    }
  }
  // for debugging signatures: traces logic given system property
  private val traceProp = (sys.BooleanProp keyExists "scalac.sigs.trace").value // performance: get the value here
  private val traceSig  = util.Tracer(traceProp)

  /** This object is only used for sanity testing when -check:genjvm is set.
   *  In that case we make sure that the erasure of the `normalized` type
   *  is the same as the erased type that's generated. Normalization means
   *  unboxing some primitive types and further simplifications as they are done in jsig.
   */
  val prepareSigMap = new TypeMap {
    def squashBoxed(tp: Type): Type = tp.normalize match {
      case t @ RefinedType(parents, decls) =>
        val parents1 = parents mapConserve squashBoxed
        if (parents1 eq parents) tp
        else RefinedType(parents1, decls)
      case t @ ExistentialType(tparams, tpe) =>
        val tpe1 = squashBoxed(tpe)
        if (tpe1 eq tpe) t
        else ExistentialType(tparams, tpe1)
      case t =>
        if (boxedClass contains t.typeSymbol) ObjectClass.tpe
        else tp
    }
    def apply(tp: Type): Type = tp.normalize match {
      case tp1 @ TypeBounds(lo, hi) =>
        val lo1 = squashBoxed(apply(lo))
        val hi1 = squashBoxed(apply(hi))
        if ((lo1 eq lo) && (hi1 eq hi)) tp1
        else TypeBounds(lo1, hi1)
      case tp1 @ TypeRef(pre, sym, args) =>
        def argApply(tp: Type) = {
          val tp1 = apply(tp)
          if (tp1.typeSymbol == UnitClass) ObjectClass.tpe
          else squashBoxed(tp1)
        }
        if (sym == ArrayClass && args.nonEmpty)
          if (unboundedGenericArrayLevel(tp1) == 1) ObjectClass.tpe
          else mapOver(tp1)
        else if (sym == AnyClass || sym == AnyValClass || sym == SingletonClass)
          ObjectClass.tpe
        else if (sym == UnitClass)
          BoxedUnitClass.tpe
        else if (sym == NothingClass)
          RuntimeNothingClass.tpe
        else if (sym == NullClass)
          RuntimeNullClass.tpe
        else {
          val pre1 = apply(pre)
          val args1 = args mapConserve argApply
          if ((pre1 eq pre) && (args1 eq args)) tp1
          else TypeRef(pre1, sym, args1)
        }
      case tp1 @ MethodType(params, restpe) =>
        val params1 = mapOver(params)
        val restpe1 = if (restpe.normalize.typeSymbol == UnitClass) UnitClass.tpe else apply(restpe)
        if ((params1 eq params) && (restpe1 eq restpe)) tp1
        else MethodType(params1, restpe1)
      case tp1 @ RefinedType(parents, decls) =>
        val parents1 = parents mapConserve apply
        if (parents1 eq parents) tp1
        else RefinedType(parents1, decls)
      case t @ ExistentialType(tparams, tpe) =>
        val tpe1 = apply(tpe)
        if (tpe1 eq tpe) t
        else ExistentialType(tparams, tpe1)
      case tp1: ClassInfoType =>
        tp1
      case tp1 =>
        mapOver(tp1)
    }
  }

  /** The Java signature of type 'info', for symbol sym. The symbol is used to give the right return
   *  type for constructors.
   */
  def javaSig(sym0: Symbol, info: Type): Option[String] = atPhase(currentRun.erasurePhase) {
    def boxedSig(tp: Type) = jsig(tp, primitiveOK = false)

    def hiBounds(bounds: TypeBounds): List[Type] = bounds.hi.normalize match {
      case RefinedType(parents, _) => parents map normalize
      case tp                      => tp :: Nil
    }

    def jsig(tp0: Type, existentiallyBound: List[Symbol] = Nil, toplevel: Boolean = false, primitiveOK: Boolean = true): String = {
      val tp = tp0.dealias
      tp match {
        case st: SubType =>
          jsig(st.supertype, existentiallyBound, toplevel, primitiveOK)
        case ExistentialType(tparams, tpe) =>
          jsig(tpe, tparams, toplevel, primitiveOK)
        case TypeRef(pre, sym, args) =>
          def argSig(tp: Type) =
            if (existentiallyBound contains tp.typeSymbol) {
              val bounds = tp.typeSymbol.info.bounds
              if (!(AnyRefClass.tpe <:< bounds.hi)) "+" + boxedSig(bounds.hi)
              else if (!(bounds.lo <:< NullClass.tpe)) "-" + boxedSig(bounds.lo)
              else "*"
            } else {
              boxedSig(tp)
            }
          def classSig: String =
            "L"+atPhase(currentRun.icodePhase)(sym.fullName + global.genJVM.moduleSuffix(sym)).replace('.', '/')
          def classSigSuffix: String =
            "."+sym.name

          // If args isEmpty, Array is being used as a higher-kinded type
          if (sym == ArrayClass && args.nonEmpty) {
            if (unboundedGenericArrayLevel(tp) == 1) jsig(ObjectClass.tpe)
            else ARRAY_TAG.toString+(args map (jsig(_))).mkString
          }
          else if (isTypeParameterInSig(sym, sym0)) {
            assert(!sym.isAliasType, "Unexpected alias type: " + sym)
            TVAR_TAG.toString+sym.name+";"
          }
          else if (sym == AnyClass || sym == AnyValClass || sym == SingletonClass)
            jsig(ObjectClass.tpe)
          else if (sym == UnitClass)
            jsig(BoxedUnitClass.tpe)
          else if (sym == NothingClass)
            jsig(RuntimeNothingClass.tpe)
          else if (sym == NullClass)
            jsig(RuntimeNullClass.tpe)
          else if (isValueClass(sym)) {
            if (!primitiveOK) jsig(ObjectClass.tpe)
            else if (sym == UnitClass) jsig(BoxedUnitClass.tpe)
            else abbrvTag(sym).toString
          }
          else if (sym.isClass) {
            val preRebound = pre.baseType(sym.owner) // #2585
            traceSig("sym.isClass", (sym.ownerChain, preRebound, sym0.enclClassChain)) {
              dotCleanup(
                (
                  if (needsJavaSig(preRebound)) {
                    val s = jsig(preRebound, existentiallyBound)
                    if (s.charAt(0) == 'L') s.substring(0, s.length - 1) + classSigSuffix
                    else classSig
                  }
                  else classSig
                ) + (
                  if (args.isEmpty) "" else
                  "<"+(args map argSig).mkString+">"
                ) + (
                  ";"
                )
              )
            }
          }
          else jsig(erasure(tp), existentiallyBound, toplevel, primitiveOK)
        case PolyType(tparams, restpe) =>
          assert(tparams.nonEmpty)
          def boundSig(bounds: List[Type]) = {
            val (isTrait, isClass) = bounds partition (_.typeSymbol.isTrait)

            ":" + (
              if (isClass.isEmpty) "" else boxedSig(isClass.head)
            ) + (
              isTrait map (x => ":" + boxedSig(x)) mkString
            )
          }
          def paramSig(tsym: Symbol) = tsym.name + boundSig(hiBounds(tsym.info.bounds))

          val paramString = if (toplevel) tparams map paramSig mkString ("<", "", ">") else ""
          traceSig("PolyType", (tparams, restpe))(paramString + jsig(restpe))
        case MethodType(params, restpe) =>
          "("+(params map (_.tpe) map (jsig(_))).mkString+")"+
          (if (restpe.typeSymbol == UnitClass || sym0.isConstructor) VOID_TAG.toString else jsig(restpe))
        case RefinedType(parent :: _, decls) =>
          boxedSig(parent)
        case ClassInfoType(parents, _, _) =>
          (parents map (boxedSig(_))).mkString
        case AnnotatedType(_, atp, _) =>
          jsig(atp, existentiallyBound, toplevel, primitiveOK)
        case BoundedWildcardType(bounds) =>
          println("something's wrong: "+sym0+":"+sym0.tpe+" has a bounded wildcard type")
          jsig(bounds.hi, existentiallyBound, toplevel, primitiveOK)
        case _ =>
          val etp = erasure(tp)
          if (etp eq tp) throw new UnknownSig
          else jsig(etp)
      }
    }
    traceSig("javaSig", (sym0, info)) {
      if (needsJavaSig(info)) {
        try Some(jsig(info, toplevel = true))
        catch { case ex: UnknownSig => None }
      }
      else None
    }
  }

  class UnknownSig extends Exception

  /** Type reference after erasure */
  def erasedTypeRef(sym: Symbol): Type =
    typeRef(erasure(sym.owner.tpe), sym, List())

  /** Remove duplicate references to class Object in a list of parent classes */
  private def removeDoubleObject(tps: List[Type]): List[Type] = tps match {
    case List() => List()
    case tp :: tps1 =>
      if (tp.typeSymbol == ObjectClass) tp :: tps1.filter(_.typeSymbol != ObjectClass)
      else tp :: removeDoubleObject(tps1)
  }

  /**  The symbol's erased info. This is the type's erasure, except for the following symbols:
   *
   *   - For $asInstanceOf      : [T]T
   *   - For $isInstanceOf      : [T]scala#Boolean
   *   - For class Array        : [T]C where C is the erased classinfo of the Array class.
   *   - For Array[T].<init>    : {scala#Int)Array[T]
   *   - For a type parameter   : A type bounds type consisting of the erasures of its bounds.
   */
  def transformInfo(sym: Symbol, tp: Type): Type = {
    if (sym == Object_asInstanceOf)
      sym.info
    else if (sym == Object_isInstanceOf || sym == ArrayClass)
      PolyType(sym.info.typeParams, erasure(sym.info.resultType))
    else if (sym.isAbstractType)
      TypeBounds(WildcardType, WildcardType)
    else if (sym.isTerm && sym.owner == ArrayClass) {
      if (sym.isClassConstructor)
        tp match {
          case MethodType(params, TypeRef(pre, sym, args)) =>
            MethodType(cloneSymbols(params) map (p => p.setInfo(erasure(p.tpe))),
                       typeRef(erasure(pre), sym, args))
        }
      else if (sym.name == nme.apply)
        tp
      else if (sym.name == nme.update)
        (tp: @unchecked) match {
          case MethodType(List(index, tvar), restpe) =>
            MethodType(List(index.cloneSymbol.setInfo(erasure(index.tpe)), tvar),
                       erasedTypeRef(UnitClass))
        }
      else erasure(tp)
    } else if (
      sym.owner != NoSymbol &&
      sym.owner.owner == ArrayClass &&
      sym == Array_update.paramss.head(1)) {
      // special case for Array.update: the non-erased type remains, i.e. (Int,A)Unit
      // since the erasure type map gets applied to every symbol, we have to catch the
      // symbol here
      tp
    } else {
/*
      val erased =
        if (sym.isGetter && sym.tpe.isInstanceOf[MethodType])
          erasure mapOver sym.tpe // for getters, unlike for normal methods, always convert Unit to BoxedUnit.
        else
          erasure(tp)
*/
      transformMixinInfo(erasure(tp))
    }
  }

  val deconstMap = new TypeMap {
    def apply(tp: Type): Type = tp match {
      case PolyType(_, _) => mapOver(tp)
      case MethodType(_, _) => mapOver(tp) // nullarymethod was eliminated during uncurry
      case _ => tp.deconst
    }
  }

// -------- erasure on trees ------------------------------------------

  override def newTyper(context: Context) = new Eraser(context)

  /** An extractor object for boxed expressions
  object Boxed {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case LabelDef(name, params, Boxed(rhs)) =>
        Some(treeCopy.LabelDef(tree, name, params, rhs) setType rhs.tpe)
      case Select(_, _) if tree.symbol == BoxedUnit_UNIT =>
        Some(Literal(()) setPos tree.pos setType UnitClass.tpe)
      case Block(List(unboxed), ret @ Select(_, _)) if ret.symbol == BoxedUnit_UNIT =>
        Some(if (unboxed.tpe.typeSymbol == UnitClass) tree
             else Block(List(unboxed), Literal(()) setPos tree.pos setType UnitClass.tpe))
      case Apply(fn, List(unboxed)) if isBox(fn.symbol) =>
        Some(unboxed)
      case _ =>
        None
    }
  }
   */

  /** The modifier typer which retypes with erased types. */
  class Eraser(context: Context) extends Typer(context) {
    private def safeToRemoveUnbox(cls: Symbol): Boolean =
      (cls == definitions.NullClass) || isBoxedValueClass(cls)

    /** Box `tree` of unboxed type */
    private def box(tree: Tree): Tree = tree match {
      case LabelDef(name, params, rhs) =>
        val rhs1 = box(rhs)
        treeCopy.LabelDef(tree, name, params, rhs1) setType rhs1.tpe
      case _ =>
        typedPos(tree.pos)(tree.tpe.typeSymbol match {
          case UnitClass  =>
            if (treeInfo isPureExpr tree) REF(BoxedUnit_UNIT)
            else BLOCK(tree, REF(BoxedUnit_UNIT))
          case NothingClass => tree // a non-terminating expression doesn't need boxing
          case x          =>
            assert(x != ArrayClass)
            tree match {
              /** Can't always remove a Box(Unbox(x)) combination because the process of boxing x
               *  may lead to throwing an exception.
               *
               *  This is important for specialization: calls to the super constructor should not box/unbox specialized
               *  fields (see TupleX). (ID)
               */
              case Apply(boxFun, List(arg)) if isUnbox(tree.symbol) && safeToRemoveUnbox(arg.tpe.typeSymbol) =>
                log("boxing an unbox: " + tree + " and replying with " + arg)
                arg
              case _ =>
                (REF(boxMethod(x)) APPLY tree) setPos (tree.pos) setType ObjectClass.tpe
            }
        })
    }

    /** Unbox `tree` of boxed type to expected type `pt`.
     *
     *  @param tree the given tree
     *  @param pt   the expected type.
     *  @return     the unboxed tree
     */
    private def unbox(tree: Tree, pt: Type): Tree = tree match {
/*
      case Boxed(unboxed) =>
        println("unbox shorten: "+tree) // this never seems to kick in during build and test; therefore disabled.
        adaptToType(unboxed, pt)
 */
      case LabelDef(name, params, rhs) =>
        val rhs1 = unbox(rhs, pt)
        treeCopy.LabelDef(tree, name, params, rhs1) setType rhs1.tpe
      case _ =>
        typedPos(tree.pos)(pt.typeSymbol match {
          case UnitClass  =>
            if (treeInfo isPureExpr tree) UNIT
            else BLOCK(tree, UNIT)
          case x          =>
            assert(x != ArrayClass)
            Apply(unboxMethod(pt.typeSymbol), tree) setType pt
        })
    }

    /** Generate a synthetic cast operation from tree.tpe to pt.
     *  @pre pt eq pt.normalize
     */
    private def cast(tree: Tree, pt: Type): Tree =
      tree AS_ATTR pt

    private def isUnboxedValueMember(sym: Symbol) =
      sym != NoSymbol && isValueClass(sym.owner)

    /** Adapt `tree` to expected type `pt`.
     *
     *  @param tree the given tree
     *  @param pt   the expected type
     *  @return     the adapted tree
     */
    private def adaptToType(tree: Tree, pt: Type): Tree = {
      if (settings.debug.value && pt != WildcardType)
        log("adapting " + tree + ":" + tree.tpe + " : " +  tree.tpe.parents + " to " + pt)//debug
      if (tree.tpe <:< pt)
        tree
      else if (isValueClass(tree.tpe.typeSymbol) && !isValueClass(pt.typeSymbol))
        adaptToType(box(tree), pt)
      else if (tree.tpe.isInstanceOf[MethodType] && tree.tpe.params.isEmpty) {
        if (!tree.symbol.isStable) assert(false, "adapt "+tree+":"+tree.tpe+" to "+pt)
        adaptToType(Apply(tree, List()) setPos tree.pos setType tree.tpe.resultType, pt)
      } else if (pt <:< tree.tpe)
        cast(tree, pt)
      else if (isValueClass(pt.typeSymbol) && !isValueClass(tree.tpe.typeSymbol))
        adaptToType(unbox(tree, pt), pt)
      else
        cast(tree, pt)
    }

    // @PP 1/25/2011: This is less inaccurate than it was (I removed
    // BoxedAnyArray, asInstanceOf$erased, and other long ago eliminated symbols)
    // but I do not think it yet describes the code beneath it.

    /**  Replace member references as follows:
     *
     *   - `x == y` for == in class Any becomes `x equals y` with equals in class Object.
     *   - `x != y` for != in class Any becomes `!(x equals y)` with equals in class Object.
     *   - x.asInstanceOf[T] becomes x.$asInstanceOf[T]
     *   - x.isInstanceOf[T] becomes x.$isInstanceOf[T]
     *   - x.m where m is some other member of Any becomes x.m where m is a member of class Object.
     *   - x.m where x has unboxed value type T and m is not a directly translated member of T becomes T.box(x).m
     *   - x.m where x is a reference type and m is a directly translated member of value type T becomes x.TValue().m
     *   - All forms of x.m where x is a boxed type and m is a member of an unboxed class become
     *     x.m where m is the corresponding member of the boxed class.
     */
    private def adaptMember(tree: Tree): Tree = {
      //Console.println("adaptMember: " + tree);
      tree match {
        case Apply(TypeApply(sel @ Select(qual, name), List(targ)), List()) if tree.symbol == Any_asInstanceOf =>
          val qual1 = typedQualifier(qual, NOmode, ObjectClass.tpe) // need to have an expected type, see #3037
          val qualClass = qual1.tpe.typeSymbol
          val targClass = targ.tpe.typeSymbol
/*
          if (isNumericValueClass(qualClass) && isNumericValueClass(targClass))
            // convert numeric type casts
            atPos(tree.pos)(Apply(Select(qual1, "to" + targClass.name), List()))
          else
*/
          if (isValueClass(targClass)) unbox(qual1, targ.tpe)
          else tree
        case Select(qual, name) if (name != nme.CONSTRUCTOR) =>
          if (tree.symbol == NoSymbol)
            tree
          else if (tree.symbol == Any_asInstanceOf)
            adaptMember(atPos(tree.pos)(Select(qual, Object_asInstanceOf)))
          else if (tree.symbol == Any_isInstanceOf)
            adaptMember(atPos(tree.pos)(Select(qual, Object_isInstanceOf)))
          else if (tree.symbol.owner == AnyClass)
            adaptMember(atPos(tree.pos)(Select(qual, getMember(ObjectClass, name))))
          else {
            var qual1 = typedQualifier(qual)
            if ((isValueClass(qual1.tpe.typeSymbol) && !isUnboxedValueMember(tree.symbol)))
              qual1 = box(qual1)
            else if (!isValueClass(qual1.tpe.typeSymbol) && isUnboxedValueMember(tree.symbol))
              qual1 = unbox(qual1, tree.symbol.owner.tpe)

            if (isValueClass(tree.symbol.owner) && !isValueClass(qual1.tpe.typeSymbol))
              tree.symbol = NoSymbol
            else if (qual1.tpe.isInstanceOf[MethodType] && qual1.tpe.params.isEmpty) {
              assert(qual1.symbol.isStable, qual1.symbol);
              qual1 = Apply(qual1, List()) setPos qual1.pos setType qual1.tpe.resultType
            } else if (!(qual1.isInstanceOf[Super] || (qual1.tpe.typeSymbol isSubClass tree.symbol.owner))) {
              assert(tree.symbol.owner != ArrayClass)
              qual1 = cast(qual1, tree.symbol.owner.tpe)
            }
            treeCopy.Select(tree, qual1, name)
          }
        case SelectFromArray(qual, name, erasure) =>
          var qual1 = typedQualifier(qual)
          if (!(qual1.tpe <:< erasure)) qual1 = cast(qual1, erasure)
          Select(qual1, name) copyAttrs tree
        case _ =>
          tree
      }
    }

    /** A replacement for the standard typer's adapt method.
     */
    override protected def adapt(tree: Tree, mode: Int, pt: Type, original: Tree = EmptyTree): Tree =
      adaptToType(tree, pt)

    /** A replacement for the standard typer's `typed1` method.
     */
    override protected def typed1(tree: Tree, mode: Int, pt: Type): Tree = {
      val tree1 = try {
        super.typed1(adaptMember(tree), mode, pt)
      } catch {
        case er: TypeError =>
          Console.println("exception when typing " + tree)
          Console.println(er.msg + " in file " + context.owner.sourceFile)
          er.printStackTrace
          abort()
        case ex: Exception =>
          //if (settings.debug.value)
          Console.println("exception when typing " + tree);
          throw ex
      }
      def adaptCase(cdef: CaseDef): CaseDef = {
        val body1 = adaptToType(cdef.body, tree1.tpe)
        treeCopy.CaseDef(cdef, cdef.pat, cdef.guard, body1) setType body1.tpe
      }
      def adaptBranch(branch: Tree): Tree =
        if (branch == EmptyTree) branch else adaptToType(branch, tree1.tpe);

      tree1 match {
        case If(cond, thenp, elsep) =>
          treeCopy.If(tree1, cond, adaptBranch(thenp), adaptBranch(elsep))
        case Match(selector, cases) =>
          treeCopy.Match(tree1, selector, cases map adaptCase)
        case Try(block, catches, finalizer) =>
          treeCopy.Try(tree1, adaptBranch(block), catches map adaptCase, finalizer)
        case Ident(_) | Select(_, _) =>
          if (tree1.symbol.isOverloaded) {
            val first = tree1.symbol.alternatives.head
            val sym1 = tree1.symbol.filter {
              alt => alt == first || !(first.tpe looselyMatches alt.tpe)
            }
            if (tree.symbol ne sym1) {
              tree1.symbol = sym1
              tree1.tpe = sym1.tpe
            }
          }
          tree1
        case _ =>
          tree1
      }
    }
  }

  /** The erasure transformer */
  class ErasureTransformer(unit: CompilationUnit) extends Transformer {
    /** Emit an error if there is a double definition. This can happen if:
     *
     *  - A template defines two members with the same name and erased type.
     *  - A template defines and inherits two members `m` with different types,
     *    but their erased types are the same.
     *  - A template inherits two members `m` with different types,
     *    but their erased types are the same.
     */
    private def checkNoDoubleDefs(root: Symbol) {
      def doubleDefError(sym1: Symbol, sym2: Symbol) {
        // the .toString must also be computed at the earlier phase
        def atRefc[T](op: => T) = atPhase[T](currentRun.refchecksPhase.next)(op)
        val tpe1 = atRefc(root.thisType.memberType(sym1))
        val tpe2 = atRefc(root.thisType.memberType(sym2))
        if (!tpe1.isErroneous && !tpe2.isErroneous)
          unit.error(
          if (sym1.owner == root) sym1.pos else root.pos,
          (if (sym1.owner == sym2.owner) "double definition:\n"
           else if (sym1.owner == root) "name clash between defined and inherited member:\n"
           else "name clash between inherited members:\n") +
          sym1 + ":" + atRefc(tpe1.toString) +
            (if (sym1.owner == root) "" else sym1.locationString) + " and\n" +
          sym2 + ":" + atRefc(tpe2.toString) +
            (if (sym2.owner == root) " at line " + (sym2.pos).line else sym2.locationString) +
          "\nhave same type" +
          (if (atRefc(tpe1 =:= tpe2)) "" else " after erasure: " + atPhase(phase.next)(sym1.tpe)))
        sym1.setInfo(ErrorType)
      }

      val decls = root.info.decls
      var e = decls.elems
      while (e ne null) {
        if (e.sym.isTerm) {
          var e1 = decls.lookupNextEntry(e)
          while (e1 ne null) {
            if (atPhase(phase.next)(e1.sym.info =:= e.sym.info)) doubleDefError(e.sym, e1.sym)
            e1 = decls.lookupNextEntry(e1)
          }
        }
        e = e.next
      }

      val opc = new overridingPairs.Cursor(root) {
        override def exclude(sym: Symbol): Boolean =
          (!sym.isTerm || sym.isPrivate || super.exclude(sym)
           // specialized members have no type history before 'specialize', causing double def errors for curried defs
           || !sym.hasTypeAt(currentRun.refchecksPhase.id))

        override def matches(sym1: Symbol, sym2: Symbol): Boolean =
          atPhase(phase.next)(sym1.tpe =:= sym2.tpe)
      }
      while (opc.hasNext) {
        if (!atPhase(currentRun.refchecksPhase.next)(
              root.thisType.memberType(opc.overriding) matches
              root.thisType.memberType(opc.overridden))) {
          if (settings.debug.value)
            log("" + opc.overriding.locationString + " " +
                     opc.overriding.infosString +
                     opc.overridden.locationString + " " +
                     opc.overridden.infosString)
          doubleDefError(opc.overriding, opc.overridden)
        }
        opc.next
      }
    }

/*
      for (bc <- root.info.baseClasses.tail; other <- bc.info.decls.toList) {
        if (other.isTerm && !other.isConstructor && !(other hasFlag (PRIVATE | BRIDGE))) {
          for (member <- root.info.nonPrivateMember(other.name).alternatives) {
            if (member != other &&
                !(member hasFlag BRIDGE) &&
                atPhase(phase.next)(member.tpe =:= other.tpe) &&
                !atPhase(refchecksPhase.next)(
                  root.thisType.memberType(member) matches root.thisType.memberType(other))) {
              if (settings.debug.value) log("" + member.locationString + " " + member.infosString + other.locationString + " " + other.infosString);
              doubleDefError(member, other)
            }
          }
        }
      }
*/

    /**  Add bridge definitions to a template. This means:
     *
     *   If there is a concrete member `m` which overrides a member in a base
     *   class of the template, and the erased types of the two members differ,
     *   and the two members are not inherited or defined by some parent class
     *   of the template, then a bridge from the overridden member `m1` to the
     *   member `m0` is added. The bridge has the erased type of `m1` and
     *   forwards to `m0`.
     *
     *   No bridge is added if there is already a bridge to `m0` with the erased
     *   type of `m1` in the template.
     */
    private def bridgeDefs(owner: Symbol): (List[Tree], immutable.Set[Symbol]) = {
      var toBeRemoved: immutable.Set[Symbol] = immutable.Set()
      //println("computing bridges for " + owner)//DEBUG
      assert(phase == currentRun.erasurePhase)
      val site = owner.thisType
      val bridgesScope = new Scope
      val bridgeTarget = new mutable.HashMap[Symbol, Symbol]
      var bridges: List[Tree] = List()
      val opc = atPhase(currentRun.explicitouterPhase) {
        new overridingPairs.Cursor(owner) {
          override def parents: List[Type] = List(owner.info.parents.head)
          override def exclude(sym: Symbol): Boolean =
            !sym.isMethod || sym.isPrivate || super.exclude(sym)
        }
      }
      while (opc.hasNext) {
        val member = opc.overriding
        val other = opc.overridden
        //Console.println("bridge? " + member + ":" + member.tpe + member.locationString + " to " + other + ":" + other.tpe + other.locationString)//DEBUG
        if (atPhase(currentRun.explicitouterPhase)(!member.isDeferred)) {
          val otpe = erasure(other.tpe)
          val bridgeNeeded = atPhase(phase.next) (
            !(other.tpe =:= member.tpe) &&
            !(deconstMap(other.tpe) =:= deconstMap(member.tpe)) &&
            { var e = bridgesScope.lookupEntry(member.name)
              while ((e ne null) && !((e.sym.tpe =:= otpe) && (bridgeTarget(e.sym) == member)))
                e = bridgesScope.lookupNextEntry(e)
              (e eq null)
            }
          );
          if (bridgeNeeded) {
            val bridge = other.cloneSymbolImpl(owner)
              .setPos(owner.pos)
              .setFlag(member.flags | BRIDGE)
              .resetFlag(ACCESSOR | DEFERRED | LAZY | lateDEFERRED)
            // the parameter symbols need to have the new owner
            bridge.setInfo(otpe.cloneInfo(bridge))
            bridgeTarget(bridge) = member
            atPhase(phase.next) { owner.info.decls.enter(bridge) }
            if (other.owner == owner) {
              //println("bridge to same: "+other+other.locationString)//DEBUG
              atPhase(phase.next) { owner.info.decls.unlink(other) }
              toBeRemoved += other
            }
            bridgesScope enter bridge
            bridges =
              atPhase(phase.next) {
                atPos(bridge.pos) {
                  val bridgeDef =
                    DefDef(bridge,
                      member.tpe match {
                        case MethodType(List(), ConstantType(c)) => Literal(c)
                        case _ =>
                          (((Select(This(owner), member): Tree) /: bridge.paramss)
                             ((fun, vparams) => Apply(fun, vparams map Ident)))
                      });
                  if (settings.debug.value)
                    log("generating bridge from " + other + "(" + Flags.flagsToString(bridge.flags)  + ")" + ":" + otpe + other.locationString + " to " + member + ":" + erasure(member.tpe) + member.locationString + " =\n " + bridgeDef);
                  bridgeDef
                }
              } :: bridges
          }
        }
        opc.next
      }
      (bridges, toBeRemoved)
    }
/*
      for (bc <- site.baseClasses.tail; other <- bc.info.decls.toList) {
        if (other.isMethod && !other.isConstructor) {
          for (member <- site.nonPrivateMember(other.name).alternatives) {
            if (member != other &&
                !(member hasFlag DEFERRED) &&
                (site.memberType(member) matches site.memberType(other)) &&
                !(site.parents exists (p =>
                  (p.symbol isSubClass member.owner) && (p.symbol isSubClass other.owner)))) {
...
             }
          }
*/

    def addBridges(stats: List[Tree], base: Symbol): List[Tree] =
      if (base.isTrait) stats
      else {
        val (bridges, toBeRemoved) = bridgeDefs(base)
        if (bridges.isEmpty) stats
        else (stats filterNot (stat => toBeRemoved contains stat.symbol)) ::: bridges
      }

    /**  Transform tree at phase erasure before retyping it.
     *   This entails the following:
     *
     *   - Remove all type parameters in class and method definitions.
     *   - Remove all abstract and alias type definitions.
     *   - Remove all type applications other than those involving a type test or cast.
     *   - Remove all empty trees in statements and definitions in a PackageDef.
     *   - Check that there are no double definitions in a template.
     *   - Add bridge definitions to a template.
     *   - Replace all types in type nodes and the EmptyTree object by their erasure.
     *     Type nodes of type Unit representing result types of methods are left alone.
     *   - Given a selection q.s, where the owner of `s` is not accessible but the
     *     type symbol of q's type qT is accessible, insert a cast (q.asInstanceOf[qT]).s
     *     This prevents illegal access errors (see #4283).
     *   - Reset all other type attributes to null, thus enforcing a retyping.
     */
    private val preTransformer = new TypingTransformer(unit) {
      def preErase(tree: Tree): Tree = tree match {
        case ClassDef(mods, name, tparams, impl) =>
          if (settings.debug.value)
            log("defs of " + tree.symbol + " = " + tree.symbol.info.decls)
          treeCopy.ClassDef(tree, mods, name, List(), impl)
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          treeCopy.DefDef(tree, mods, name, List(), vparamss, tpt, rhs)
        case TypeDef(_, _, _, _) =>
          EmptyTree
        case Apply(instanceOf @ TypeApply(fun @ Select(qual, name), args @ List(arg)), List()) // !!! todo: simplify by having GenericArray also extract trees
              if ((fun.symbol == Any_isInstanceOf || fun.symbol == Object_isInstanceOf) &&
                  unboundedGenericArrayLevel(arg.tpe) > 0) =>
          val level = unboundedGenericArrayLevel(arg.tpe)
          def isArrayTest(arg: Tree) =
            gen.mkRuntimeCall("isArray", List(arg, Literal(Constant(level))))
          global.typer.typedPos(tree.pos) {
            if (level == 1) isArrayTest(qual)
            else
              gen.evalOnce(qual, currentOwner, unit) { qual1 =>
                gen.mkAnd(
                  Apply(TypeApply(Select(qual1(), fun.symbol),
                                  List(TypeTree(erasure(arg.tpe)))),
                        List()),
                  isArrayTest(qual1()))
              }
          }
        case TypeApply(fun, args) if (fun.symbol.owner != AnyClass &&
                                      fun.symbol != Object_asInstanceOf &&
                                      fun.symbol != Object_isInstanceOf) =>
          // leave all other type tests/type casts, remove all other type applications
          preErase(fun)
        case Apply(fn @ Select(qual, name), args) if (fn.symbol.owner == ArrayClass) =>
          if (unboundedGenericArrayLevel(qual.tpe.widen) == 1)
            // convert calls to apply/update/length on generic arrays to
            // calls of ScalaRunTime.array_xxx method calls
            global.typer.typedPos(tree.pos) { gen.mkRuntimeCall("array_"+name, qual :: args) }
          else
            // store exact array erasure in map to be retrieved later when we might
            // need to do the cast in adaptMember
            treeCopy.Apply(
              tree,
              SelectFromArray(qual, name, erasure(qual.tpe)).copyAttrs(fn),
              args)

        case Apply(fn @ Select(qual, _), Nil) if fn.symbol == Any_## || fn.symbol == Object_## =>
          // This is unattractive, but without it we crash here on ().## because after
          // erasure the ScalaRunTime.hash overload goes from Unit => Int to BoxedUnit => Int.
          // This must be because some earlier transformation is being skipped on ##, but so
          // far I don't know what.  For null we now define null.## == 0.
          val arg = qual.tpe.typeSymbolDirect match {
            case UnitClass  => BLOCK(qual, REF(BoxedUnit_UNIT))   // ({ expr; UNIT }).##
            case NullClass  => LIT(0)                             // (null: Object).##
            case _          => qual
          }
          Apply(gen.mkAttributedRef(scalaRuntimeHash), List(arg))

        case Apply(fn, args) =>
          if (fn.symbol == Any_asInstanceOf)
            (fn: @unchecked) match {
              case TypeApply(Select(qual, _), List(targ)) =>
                if (qual.tpe <:< targ.tpe) {
                  atPos(tree.pos) { Typed(qual, TypeTree(targ.tpe)) }
                } else if (isNumericValueClass(qual.tpe.typeSymbol) &&
                           isNumericValueClass(targ.tpe.typeSymbol)) {
                  // convert numeric type casts
                  val cname = newTermName("to" + targ.tpe.typeSymbol.name)
                  val csym = qual.tpe.member(cname)
                  assert(csym != NoSymbol)
                  atPos(tree.pos) { Apply(Select(qual, csym), List()) }
                } else
                  tree
            }
            // todo: also handle the case where the singleton type is buried in a compound
          else if (fn.symbol == Any_isInstanceOf) {
            fn match {
              case TypeApply(sel @ Select(qual, name), List(targ)) =>
                if (qual.tpe != null && isValueClass(qual.tpe.typeSymbol) && targ.tpe != null && targ.tpe <:< AnyRefClass.tpe)
                  unit.error(sel.pos, "isInstanceOf cannot test if value types are references.")

                def mkIsInstanceOf(q: () => Tree)(tp: Type): Tree =
                  Apply(
                    TypeApply(
                      Select(q(), Object_isInstanceOf) setPos sel.pos,
                      List(TypeTree(tp) setPos targ.pos)) setPos fn.pos,
                    List()) setPos tree.pos
                targ.tpe match {
                  case SingleType(_, _) | ThisType(_) | SuperType(_, _) =>
                    val cmpOp = if (targ.tpe <:< AnyValClass.tpe) Any_equals else Object_eq
                    atPos(tree.pos) {
                      Apply(Select(qual, cmpOp), List(gen.mkAttributedQualifier(targ.tpe)))
                    }
                  case RefinedType(parents, decls) if (parents.length >= 2) =>
                    // Optimization: don't generate isInstanceOf tests if the static type
                    // conforms, because it always succeeds.  (Or at least it had better.)
                    // At this writing the pattern matcher generates some instance tests
                    // involving intersections where at least one parent is statically known true.
                    // That needs fixing, but filtering the parents here adds an additional
                    // level of robustness (in addition to the short term fix.)
                    val parentTests = parents filterNot (qual.tpe <:< _)

                    if (parentTests.isEmpty) Literal(Constant(true))
                    else gen.evalOnce(qual, currentOwner, unit) { q =>
                      atPos(tree.pos) {
                        parentTests map mkIsInstanceOf(q) reduceRight gen.mkAnd
                      }
                    }
                  case _ =>
                    tree
                }
              case _ => tree
            }
          }
          else {
            def doDynamic(fn: Tree, qual: Tree): Tree = {
              if (fn.symbol.owner.isRefinementClass && fn.symbol.allOverriddenSymbols.isEmpty)
                ApplyDynamic(qual, args) setSymbol fn.symbol setPos tree.pos
              else tree
            }
            fn match {
              case Select(qual, _) => doDynamic(fn, qual)
              case TypeApply(fni@Select(qual, _), _) => doDynamic(fni, qual)// type parameters are irrelevant in case of dynamic call
              case _ =>
                tree
            }
          }

        case Select(qual, name) =>
          val owner = tree.symbol.owner
          // println("preXform: "+ (tree, tree.symbol, tree.symbol.owner, tree.symbol.owner.isRefinementClass))
          if (owner.isRefinementClass) {
            val overridden = tree.symbol.allOverriddenSymbols
            assert(!overridden.isEmpty, tree.symbol)
            tree.symbol = overridden.head
          }
          def isAccessible(sym: Symbol) = localTyper.context.isAccessible(sym, sym.owner.thisType)
          if (!isAccessible(owner) && qual.tpe != null) {
            // Todo: Figure out how qual.tpe could be null in the check above (it does appear in build where SwingWorker.this
            // has a null type).
            val qualSym = qual.tpe.widen.typeSymbol
            if (isAccessible(qualSym) && !qualSym.isPackageClass && !qualSym.isPackageObjectClass) {
              // insert cast to prevent illegal access error (see #4283)
              // util.trace("insert erasure cast ") (*/
              treeCopy.Select(tree, qual AS_ATTR qual.tpe.widen, name) //)
            } else tree
          } else tree

        case Template(parents, self, body) =>
          assert(!currentOwner.isImplClass)
          //Console.println("checking no dble defs " + tree)//DEBUG
          checkNoDoubleDefs(tree.symbol.owner)
          treeCopy.Template(tree, parents, emptyValDef, addBridges(body, currentOwner))

        case Match(selector, cases) =>
          Match(Typed(selector, TypeTree(selector.tpe)), cases)

        case Literal(ct) if ct.tag == ClassTag
                         && ct.typeValue.typeSymbol != definitions.UnitClass =>
          treeCopy.Literal(tree, Constant(erasure(ct.typeValue)))

        case _ =>
          tree
      }

      override def transform(tree: Tree): Tree = {
        // Reply to "!!! needed?" which adorned the next line: without it, build fails with:
        //   Exception in thread "main" scala.tools.nsc.symtab.Types$TypeError:
        //   value array_this is not a member of object scala.runtime.ScalaRunTime
        //
        // What the heck is array_this? See preTransformer in this file:
        //   gen.mkRuntimeCall("array_"+name, qual :: args)
        if (tree.symbol == ArrayClass && !tree.isType) tree
        else {
          val tree1 = preErase(tree)
          tree1 match {
            case EmptyTree | TypeTree() =>
              tree1 setType erasure(tree1.tpe)
            case DefDef(_, _, _, _, tpt, _) =>
              val result = super.transform(tree1) setType null
              tpt.tpe = erasure(tree1.symbol.tpe).resultType
              result
            case _ =>
              super.transform(tree1) setType null
          }
        }
      }
    }

    /** The main transform function: Pretransfom the tree, and then
     *  re-type it at phase erasure.next.
     */
    override def transform(tree: Tree): Tree = {
      val tree1 = preTransformer.transform(tree)
      atPhase(phase.next) {
        val tree2 = mixinTransformer.transform(tree1)
        if (settings.debug.value)
          log("tree after addinterfaces: \n" + tree2)

        newTyper(rootContext(unit, tree, true)).typed(tree2)
      }
    }
  }
}
