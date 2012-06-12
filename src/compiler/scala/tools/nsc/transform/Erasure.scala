/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import scala.reflect.internal.ClassfileConstants._
import scala.collection.{ mutable, immutable }
import symtab._
import Flags._

abstract class Erasure extends AddInterfaces
                          with reflect.internal.transform.Erasure
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

  // convert a numeric with a toXXX method
  def numericConversion(tree: Tree, numericSym: Symbol): Tree = {
    val mname      = newTermName("to" + numericSym.name)
    val conversion = tree.tpe member mname

    assert(conversion != NoSymbol, tree + " => " + numericSym)
    atPos(tree.pos)(Apply(Select(tree, conversion), Nil))
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
          case RefinedType(parents, _) =>
            parents foreach traverse
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

  override protected def verifyJavaErasure = settings.Xverify.value || settings.debug.value
  def needsJavaSig(tp: Type) = !settings.Ynogenericsig.value && NeedsSigCollector.collect(tp)

  // only refer to type params that will actually make it into the sig, this excludes:
  // * higher-order type parameters
  // * type parameters appearing in method parameters
  // * type members not visible in an enclosing template
  private def isTypeParameterInSig(sym: Symbol, initialSymbol: Symbol) = (
    !sym.isHigherOrderTypeParameter &&
    sym.isTypeParameterOrSkolem && (
      (initialSymbol.enclClassChain.exists(sym isNestedIn _)) ||
      (initialSymbol.isMethod && initialSymbol.typeParams.contains(sym))
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

  private def hiBounds(bounds: TypeBounds): List[Type] = bounds.hi.normalize match {
    case RefinedType(parents, _) => parents map (_.normalize)
    case tp                      => tp :: Nil
  }

  /** The Java signature of type 'info', for symbol sym. The symbol is used to give the right return
   *  type for constructors.
   */
  def javaSig(sym0: Symbol, info: Type): Option[String] = beforeErasure {
    val isTraitSignature = sym0.enclClass.isTrait

    def superSig(parents: List[Type]) = {
      val ps = (
        if (isTraitSignature) {
          // java is unthrilled about seeing interfaces inherit from classes
          val ok = parents filter (p => p.typeSymbol.isTrait || p.typeSymbol.isInterface)
          // traits should always list Object.
          if (ok.isEmpty || ok.head.typeSymbol != ObjectClass) ObjectClass.tpe :: ok
          else ok
        }
        else parents
      )
      (ps map boxedSig).mkString
    }
    def boxedSig(tp: Type) = jsig(tp, primitiveOK = false)
    def boundsSig(bounds: List[Type]) = {
      val (isTrait, isClass) = bounds partition (_.typeSymbol.isTrait)
      val classPart = isClass match {
        case Nil    => ":" // + boxedSig(ObjectClass.tpe)
        case x :: _ => ":" + boxedSig(x)
      }
      classPart :: (isTrait map boxedSig) mkString ":"
    }
    def paramSig(tsym: Symbol) = tsym.name + boundsSig(hiBounds(tsym.info.bounds))
    def polyParamSig(tparams: List[Symbol]) = (
      if (tparams.isEmpty) ""
      else tparams map paramSig mkString ("<", "", ">")
    )

    // Anything which could conceivably be a module (i.e. isn't known to be
    // a type parameter or similar) must go through here or the signature is
    // likely to end up with Foo<T>.Empty where it needs Foo<T>.Empty$.
    def fullNameInSig(sym: Symbol) = "L" + beforeIcode(sym.javaBinaryName)

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

          // If args isEmpty, Array is being used as a type constructor
          if (sym == ArrayClass && args.nonEmpty) {
            if (unboundedGenericArrayLevel(tp) == 1) jsig(ObjectClass.tpe)
            else ARRAY_TAG.toString+(args map (jsig(_))).mkString
          }
          else if (isTypeParameterInSig(sym, sym0)) {
            assert(!sym.isAliasType, "Unexpected alias type: " + sym)
            "" + TVAR_TAG + sym.name + ";"
          }
          else if (sym == AnyClass || sym == AnyValClass || sym == SingletonClass)
            jsig(ObjectClass.tpe)
          else if (sym == UnitClass)
            jsig(BoxedUnitClass.tpe)
          else if (sym == NothingClass)
            jsig(RuntimeNothingClass.tpe)
          else if (sym == NullClass)
            jsig(RuntimeNullClass.tpe)
          else if (isPrimitiveValueClass(sym)) {
            if (!primitiveOK) jsig(ObjectClass.tpe)
            else if (sym == UnitClass) jsig(BoxedUnitClass.tpe)
            else abbrvTag(sym).toString
          }
          else if (sym.isClass) {
            val preRebound = pre.baseType(sym.owner) // #2585
            dotCleanup(
              (
                if (needsJavaSig(preRebound)) {
                  val s = jsig(preRebound, existentiallyBound)
                  if (s.charAt(0) == 'L') s.substring(0, s.length - 1) + "." + sym.javaSimpleName
                  else fullNameInSig(sym)
                }
                else fullNameInSig(sym)
              ) + (
                if (args.isEmpty) "" else
                "<"+(args map argSig).mkString+">"
              ) + (
                ";"
              )
            )
          }
          else jsig(erasure(sym0)(tp), existentiallyBound, toplevel, primitiveOK)
        case PolyType(tparams, restpe) =>
          assert(tparams.nonEmpty)
          val poly = if (toplevel) polyParamSig(tparams) else ""
          poly + jsig(restpe)

        case MethodType(params, restpe) =>
          "("+(params map (_.tpe) map (jsig(_))).mkString+")"+
          (if (restpe.typeSymbol == UnitClass || sym0.isConstructor) VOID_TAG.toString else jsig(restpe))

        case RefinedType(parent :: _, decls) =>
          boxedSig(parent)
        case ClassInfoType(parents, _, _) =>
          superSig(parents)
        case AnnotatedType(_, atp, _) =>
          jsig(atp, existentiallyBound, toplevel, primitiveOK)
        case BoundedWildcardType(bounds) =>
          println("something's wrong: "+sym0+":"+sym0.tpe+" has a bounded wildcard type")
          jsig(bounds.hi, existentiallyBound, toplevel, primitiveOK)
        case _ =>
          val etp = erasure(sym0)(tp)
          if (etp eq tp) throw new UnknownSig
          else jsig(etp)
      }
    }
    if (needsJavaSig(info)) {
      try Some(jsig(info, toplevel = true))
      catch { case ex: UnknownSig => None }
    }
    else None
  }

  class UnknownSig extends Exception

  /**  The symbol's erased info. This is the type's erasure, except for the following symbols:
   *
   *   - For $asInstanceOf      : [T]T
   *   - For $isInstanceOf      : [T]scala#Boolean
   *   - For class Array        : [T]C where C is the erased classinfo of the Array class.
   *   - For Array[T].<init>    : {scala#Int)Array[T]
   *   - For a type parameter   : A type bounds type consisting of the erasures of its bounds.
   */
  override def transformInfo(sym: Symbol, tp: Type): Type =
    transformMixinInfo(super.transformInfo(sym, tp))

  val deconstMap = new TypeMap {
    // For some reason classOf[Foo] creates ConstantType(Constant(tpe)) with an actual Type for tpe,
    // which is later translated to a Class. Unfortunately that means we have bugs like the erasure
    // of Class[Foo] and classOf[Bar] not being seen as equivalent, leading to duplicate method
    // generation and failing bytecode. See ticket #4753.
    def apply(tp: Type): Type = tp match {
      case PolyType(_, _)                  => mapOver(tp)
      case MethodType(_, _)                => mapOver(tp)     // nullarymethod was eliminated during uncurry
      case ConstantType(Constant(_: Type)) => ClassClass.tpe  // all classOfs erase to Class
      case _                               => tp.deconst
    }
  }
  // Methods on Any/Object which we rewrite here while we still know what
  // is a primitive and what arrived boxed.
  private lazy val interceptedMethods = Set[Symbol](Any_##, Object_##, Any_getClass) ++ (
    // Each value class has its own getClass for ultra-precise class object typing.
    ScalaValueClasses map (_.tpe member nme.getClass_)
  )

// -------- erasure on trees ------------------------------------------

  override def newTyper(context: Context) = new Eraser(context)

  private def safeToRemoveUnbox(cls: Symbol): Boolean =
    (cls == definitions.NullClass) || isBoxedValueClass(cls)

  /** An extractor object for unboxed expressions (maybe subsumed by posterasure?) */
  object Unboxed {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Apply(fn, List(arg)) if isUnbox(fn.symbol) && safeToRemoveUnbox(arg.tpe.typeSymbol) =>
        Some(arg)
      case Apply(
        TypeApply(
          cast @ Select(
            Apply(
              sel @ Select(arg, acc),
              List()),
            asinstanceof),
          List(tpt)),
        List())
      if cast.symbol == Object_asInstanceOf &&
        tpt.tpe.typeSymbol.isDerivedValueClass &&
        sel.symbol == tpt.tpe.typeSymbol.firstParamAccessor =>
        Some(arg)
      case _ =>
        None
    }
  }

  /** An extractor object for boxed expressions (maybe subsumed by posterasure?) */
  object Boxed {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Apply(Select(New(tpt), nme.CONSTRUCTOR), List(arg)) if (tpt.tpe.typeSymbol.isDerivedValueClass) =>
        Some(arg)
      case LabelDef(name, params, Boxed(rhs)) =>
        Some(treeCopy.LabelDef(tree, name, params, rhs) setType rhs.tpe)
      case _ =>
        None
    }
  }

  class ComputeBridges(owner: Symbol) {
    assert(phase == currentRun.erasurePhase, phase)

    var toBeRemoved  = immutable.Set[Symbol]()
    val site         = owner.thisType
    val bridgesScope = newScope
    val bridgeTarget = mutable.HashMap[Symbol, Symbol]()
    var bridges      = List[Tree]()

    val opc = beforeExplicitOuter {
      new overridingPairs.Cursor(owner) {
        override def parents              = List(owner.info.firstParent)
        override def exclude(sym: Symbol) = !sym.isMethod || sym.isPrivate || super.exclude(sym)
      }
    }

    def compute(): (List[Tree], immutable.Set[Symbol]) = {
      while (opc.hasNext) {
        val member = opc.overriding
        val other  = opc.overridden
        //println("bridge? " + member + ":" + member.tpe + member.locationString + " to " + other + ":" + other.tpe + other.locationString)//DEBUG
        if (beforeExplicitOuter(!member.isDeferred))
          checkPair(member, other)

        opc.next
      }
      (bridges, toBeRemoved)
    }

    def checkPair(member: Symbol, other: Symbol) {
      val otpe = erasure(owner)(other.tpe)
      val bridgeNeeded = afterErasure (
        !(other.tpe =:= member.tpe) &&
        !(deconstMap(other.tpe) =:= deconstMap(member.tpe)) &&
        { var e = bridgesScope.lookupEntry(member.name)
          while ((e ne null) && !((e.sym.tpe =:= otpe) && (bridgeTarget(e.sym) == member)))
            e = bridgesScope.lookupNextEntry(e)
          (e eq null)
        }
      )
      if (!bridgeNeeded)
        return

      val newFlags = (member.flags | BRIDGE) & ~(ACCESSOR | DEFERRED | LAZY | lateDEFERRED)
      val bridge   = other.cloneSymbolImpl(owner, newFlags) setPos owner.pos

      debuglog("generating bridge from %s (%s): %s to %s: %s".format(
        other, flagsToString(newFlags),
        otpe + other.locationString, member,
        erasure(owner)(member.tpe) + member.locationString)
      )

      // the parameter symbols need to have the new owner
      bridge setInfo (otpe cloneInfo bridge)
      bridgeTarget(bridge) = member
      afterErasure(owner.info.decls enter bridge)
      if (other.owner == owner) {
        afterErasure(owner.info.decls.unlink(other))
        toBeRemoved += other
      }
      bridgesScope enter bridge
      bridges ::= makeBridgeDefDef(bridge, member, other)
    }

    def makeBridgeDefDef(bridge: Symbol, member: Symbol, other: Symbol) = afterErasure {
      // type checking ensures we can safely call `other`, but unless `member.tpe <:< other.tpe`,
      // calling `member` is not guaranteed to succeed in general, there's
      // nothing we can do about this, except for an unapply: when this subtype test fails,
      // return None without calling `member`
      //
      // TODO: should we do this for user-defined unapplies as well?
      // does the first argument list have exactly one argument -- for user-defined unapplies we can't be sure
      def maybeWrap(bridgingCall: Tree): Tree = {
        val guardExtractor = ( // can't statically know which member is going to be selected, so don't let this depend on member.isSynthetic
             (member.name == nme.unapply || member.name == nme.unapplySeq)
          && !afterErasure((member.tpe <:< other.tpe))) // no static guarantees (TODO: is the subtype test ever true?)

        import CODE._
        val _false    = FALSE_typed
        val pt        = member.tpe.resultType
        lazy val zero =
          if      (_false.tpe <:< pt)    _false
          else if (NoneModule.tpe <:< pt) REF(NoneModule)
          else EmptyTree

        if (guardExtractor && (zero ne EmptyTree)) {
          val typeTest = gen.mkIsInstanceOf(REF(bridge.firstParam), member.tpe.params.head.tpe)
          IF (typeTest) THEN bridgingCall ELSE zero
        } else bridgingCall
      }
      val rhs = member.tpe match {
        case MethodType(Nil, ConstantType(c)) => Literal(c)
        case _                                =>
          val sel: Tree    = Select(This(owner), member)
          val bridgingCall = (sel /: bridge.paramss)((fun, vparams) => Apply(fun, vparams map Ident))

          maybeWrap(bridgingCall)
      }
      atPos(bridge.pos)(DefDef(bridge, rhs))
    }
  }

  /** The modifier typer which retypes with erased types. */
  class Eraser(_context: Context) extends Typer(_context) {

    private def isPrimitiveValueType(tpe: Type) = isPrimitiveValueClass(tpe.typeSymbol)

    private def isErasedValueType(tpe: Type) = tpe.isInstanceOf[ErasedValueType]

    private def isDifferentErasedValueType(tpe: Type, other: Type) =
      isErasedValueType(tpe) && (tpe ne other)

    private def isPrimitiveValueMember(sym: Symbol) =
      sym != NoSymbol && isPrimitiveValueClass(sym.owner)

    private def box(tree: Tree, target: => String): Tree = {
      val result = box1(tree)
      log("boxing "+tree+":"+tree.tpe+" to "+target+" = "+result+":"+result.tpe)
      result
    }

    /** Box `tree` of unboxed type */
    private def box1(tree: Tree): Tree = tree match {
      case LabelDef(_, _, _) =>
        val ldef = deriveLabelDef(tree)(box1)
        ldef setType ldef.rhs.tpe
      case _ =>
        val tree1 = tree.tpe match {
          case ErasedValueType(clazz) =>
            tree match {
              case Unboxed(arg) if arg.tpe.typeSymbol == clazz =>
                log("shortcircuiting unbox -> box "+arg); arg
              case _ =>
                New(clazz, cast(tree, underlyingOfValueClass(clazz)))
            }
          case _ =>
            tree.tpe.typeSymbol match {
          case UnitClass  =>
            if (treeInfo isExprSafeToInline tree) REF(BoxedUnit_UNIT)
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
                log("boxing an unbox: " + tree + "/" + tree.symbol + " and replying with " + arg + " of type " + arg.tpe)
                arg
              case _ =>
                (REF(boxMethod(x)) APPLY tree) setPos (tree.pos) setType ObjectClass.tpe
            }
            }
        }
        typedPos(tree.pos)(tree1)
    }

    private def unbox(tree: Tree, pt: Type): Tree = {
      val result = unbox1(tree, pt)
      log("unboxing "+tree+":"+tree.tpe+" to "+pt+" = "+result+":"+result.tpe)
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
          case ErasedValueType(clazz) =>
            tree match {
              case Boxed(arg) if arg.tpe.isInstanceOf[ErasedValueType] =>
                log("shortcircuiting box -> unbox "+arg)
                arg
              case _ =>
                log("not boxed: "+tree)
                val tree0 = adaptToType(tree, clazz.tpe)
                cast(Apply(Select(tree0, clazz.firstParamAccessor), List()), pt)
            }
          case _ =>
            pt.typeSymbol match {
          case UnitClass  =>
            if (treeInfo isExprSafeToInline tree) UNIT
            else BLOCK(tree, UNIT)
          case x          =>
            assert(x != ArrayClass)
            // don't `setType pt` the Apply tree, as the Apply's fun won't be typechecked if the Apply tree already has a type
            Apply(unboxMethod(pt.typeSymbol), tree)
            }
        }
        typedPos(tree.pos)(tree1)
    }

    /** Generate a synthetic cast operation from tree.tpe to pt.
     *  @pre pt eq pt.normalize
     */
    private def cast(tree: Tree, pt: Type): Tree = {
      if (pt.typeSymbol == UnitClass) {
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
    private def adaptToType(tree: Tree, pt: Type): Tree = {
      if (settings.debug.value && pt != WildcardType)
        log("adapting " + tree + ":" + tree.tpe + " : " +  tree.tpe.parents + " to " + pt)//debug
      if (tree.tpe <:< pt)
        tree
      else if (isDifferentErasedValueType(tree.tpe, pt))
        adaptToType(box(tree, pt.toString), pt)
      else if (isDifferentErasedValueType(pt, tree.tpe))
        adaptToType(unbox(tree, pt), pt)
      else if (isPrimitiveValueType(tree.tpe) && !isPrimitiveValueType(pt)) {
        adaptToType(box(tree, pt.toString), pt)
      } else if (tree.tpe.isInstanceOf[MethodType] && tree.tpe.params.isEmpty) {
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

    /**  Replace member references as follows:
     *
     *   - `x == y` for == in class Any becomes `x equals y` with equals in class Object.
     *   - `x != y` for != in class Any becomes `!(x equals y)` with equals in class Object.
     *   - x.asInstanceOf[T] becomes x.$asInstanceOf[T]
     *   - x.isInstanceOf[T] becomes x.$isInstanceOf[T]
     *   - x.isInstanceOf[ErasedValueType(clazz)] becomes x.isInstanceOf[clazz.tpe]
     *   - x.m where m is some other member of Any becomes x.m where m is a member of class Object.
     *   - x.m where x has unboxed value type T and m is not a directly translated member of T becomes T.box(x).m
     *   - x.m where x is a reference type and m is a directly translated member of value type T becomes x.TValue().m
     *   - All forms of x.m where x is a boxed type and m is a member of an unboxed class become
     *     x.m where m is the corresponding member of the boxed class.
     */
    private def adaptMember(tree: Tree): Tree = {
      //Console.println("adaptMember: " + tree);
      tree match {
        case Apply(TypeApply(sel @ Select(qual, name), List(targ)), List())
        if tree.symbol == Any_asInstanceOf =>
          val qual1 = typedQualifier(qual, NOmode, ObjectClass.tpe) // need to have an expected type, see #3037
          val qualClass = qual1.tpe.typeSymbol
/*
          val targClass = targ.tpe.typeSymbol

          if (isNumericValueClass(qualClass) && isNumericValueClass(targClass))
            // convert numeric type casts
            atPos(tree.pos)(Apply(Select(qual1, "to" + targClass.name), List()))
          else
*/
          if (isPrimitiveValueType(targ.tpe) || isErasedValueType(targ.tpe)) unbox(qual1, targ.tpe)
          else tree
        case Apply(TypeApply(sel @ Select(qual, name), List(targ)), List())
        if tree.symbol == Any_isInstanceOf =>
          targ.tpe match {
            case ErasedValueType(clazz) => targ.setType(clazz.tpe)
            case _ =>
          }
            tree
        case Select(qual, name) =>
          if (tree.symbol == NoSymbol) {
            tree
          } else if (name == nme.CONSTRUCTOR) {
            if (tree.symbol.owner == AnyValClass) tree.symbol = ObjectClass.primaryConstructor
            tree
          } else if (tree.symbol == Any_asInstanceOf)
            adaptMember(atPos(tree.pos)(Select(qual, Object_asInstanceOf)))
          else if (tree.symbol == Any_isInstanceOf)
            adaptMember(atPos(tree.pos)(Select(qual, Object_isInstanceOf)))
          else if (tree.symbol.owner == AnyClass)
            adaptMember(atPos(tree.pos)(Select(qual, getMember(ObjectClass, name))))
          else {
            var qual1 = typedQualifier(qual)
            if ((isPrimitiveValueType(qual1.tpe) && !isPrimitiveValueMember(tree.symbol)) ||
                 isErasedValueType(qual1.tpe))
              qual1 = box(qual1, "owner "+tree.symbol.owner)
            else if (!isPrimitiveValueType(qual1.tpe) && isPrimitiveValueMember(tree.symbol))
              qual1 = unbox(qual1, tree.symbol.owner.tpe)

            if (isPrimitiveValueMember(tree.symbol) && !isPrimitiveValueType(qual1.tpe))
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
    override def typed1(tree: Tree, mode: Int, pt: Type): Tree = {
      val tree1 = try {
        tree match {
          case InjectDerivedValue(arg) =>
            val clazz = tree.symbol
            val result = typed1(arg, mode, underlyingOfValueClass(clazz)) setType ErasedValueType(clazz)
            log("transforming inject "+arg+":"+underlyingOfValueClass(clazz)+"/"+ErasedValueType(clazz)+" = "+result)
            return result

          case _ =>
        super.typed1(adaptMember(tree), mode, pt)
        }
      } catch {
        case er: TypeError =>
          Console.println("exception when typing " + tree)
          Console.println(er.msg + " in file " + context.owner.sourceFile)
          er.printStackTrace
          abort("unrecoverable error")
        case ex: Exception =>
          //if (settings.debug.value)
          try Console.println("exception when typing " + tree)
          finally throw ex
          throw ex
      }
      def adaptCase(cdef: CaseDef): CaseDef = {
        val newCdef = deriveCaseDef(cdef)(adaptToType(_, tree1.tpe))
        newCdef setType newCdef.body.tpe
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
      def afterErasure[T](op: => T): T = atPhase(phase.next.next)(op)
      def doubleDefError(sym1: Symbol, sym2: Symbol) {
        // the .toString must also be computed at the earlier phase
        val tpe1 = afterRefchecks(root.thisType.memberType(sym1))
        val tpe2 = afterRefchecks(root.thisType.memberType(sym2))
        if (!tpe1.isErroneous && !tpe2.isErroneous)
          unit.error(
          if (sym1.owner == root) sym1.pos else root.pos,
          (if (sym1.owner == sym2.owner) "double definition:\n"
           else if (sym1.owner == root) "name clash between defined and inherited member:\n"
           else "name clash between inherited members:\n") +
          sym1 + ":" + afterRefchecks(tpe1.toString) +
            (if (sym1.owner == root) "" else sym1.locationString) + " and\n" +
          sym2 + ":" + afterRefchecks(tpe2.toString) +
            (if (sym2.owner == root) " at line " + (sym2.pos).line else sym2.locationString) +
          "\nhave same type" +
          (if (afterRefchecks(tpe1 =:= tpe2)) "" else " after erasure: " + afterErasure(sym1.tpe)))
        sym1.setInfo(ErrorType)
      }

      val decls = root.info.decls
      var e = decls.elems
      while (e ne null) {
        if (e.sym.isTerm) {
          var e1 = decls.lookupNextEntry(e)
          while (e1 ne null) {
            if (afterErasure(e1.sym.info =:= e.sym.info)) doubleDefError(e.sym, e1.sym)
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
          afterErasure(sym1.tpe =:= sym2.tpe)
      }
      while (opc.hasNext) {
        if (!afterRefchecks(
              root.thisType.memberType(opc.overriding) matches
              root.thisType.memberType(opc.overridden))) {
          debuglog("" + opc.overriding.locationString + " " +
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
                afterErasure(member.tpe =:= other.tpe) &&
                !afterRefchecks(
                  root.thisType.memberType(member) matches root.thisType.memberType(other))) {
              debuglog("" + member.locationString + " " + member.infosString + other.locationString + " " + other.infosString);
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
      assert(phase == currentRun.erasurePhase, phase)
      debuglog("computing bridges for " + owner)
      new ComputeBridges(owner) compute()
    }

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
     *   - Remove all instance creations new C(arg) where C is an inlined class.
     *   - Reset all other type attributes to null, thus enforcing a retyping.
     */
    private val preTransformer = new TypingTransformer(unit) {
      def preErase(tree: Tree): Tree = tree match {
        case ClassDef(_,_,_,_) =>
          debuglog("defs of " + tree.symbol + " = " + tree.symbol.info.decls)
          copyClassDef(tree)(tparams = Nil)
        case DefDef(_,_,_,_,_,_) =>
          copyDefDef(tree)(tparams = Nil)
        case TypeDef(_, _, _, _) =>
          EmptyTree
        case Apply(instanceOf @ TypeApply(fun @ Select(qual, name), args @ List(arg)), List()) // !!! todo: simplify by having GenericArray also extract trees
              if ((fun.symbol == Any_isInstanceOf || fun.symbol == Object_isInstanceOf) &&
                  unboundedGenericArrayLevel(arg.tpe) > 0) =>
          val level = unboundedGenericArrayLevel(arg.tpe)
          def isArrayTest(arg: Tree) =
            gen.mkRuntimeCall(nme.isArray, List(arg, Literal(Constant(level))))

          global.typer.typedPos(tree.pos) {
            if (level == 1) isArrayTest(qual)
            else gen.evalOnce(qual, currentOwner, unit) { qual1 =>
              gen.mkAnd(
                gen.mkMethodCall(
                  qual1(),
                  fun.symbol,
                  List(specialErasure(fun.symbol)(arg.tpe)),
                  Nil
                ),
                isArrayTest(qual1())
              )
            }
          }
        case TypeApply(fun, args) if (fun.symbol.owner != AnyClass &&
                                      fun.symbol != Object_asInstanceOf &&
                                      fun.symbol != Object_isInstanceOf) =>
          // leave all other type tests/type casts, remove all other type applications
          preErase(fun)
        case Apply(fn @ Select(qual, name), args) if fn.symbol.owner == ArrayClass =>
          // Have to also catch calls to abstract types which are bounded by Array.
          if (unboundedGenericArrayLevel(qual.tpe.widen) == 1 || qual.tpe.typeSymbol.isAbstractType) {
            // convert calls to apply/update/length on generic arrays to
            // calls of ScalaRunTime.array_xxx method calls
            global.typer.typedPos(tree.pos)({
              val arrayMethodName = name match {
                case nme.apply  => nme.array_apply
                case nme.length => nme.array_length
                case nme.update => nme.array_update
                case nme.clone_ => nme.array_clone
                case _          => unit.error(tree.pos, "Unexpected array member, no translation exists.") ; nme.NO_NAME
              }
              gen.mkRuntimeCall(arrayMethodName, qual :: args)
            })
          }
          else {
            // store exact array erasure in map to be retrieved later when we might
            // need to do the cast in adaptMember
            treeCopy.Apply(
              tree,
              SelectFromArray(qual, name, erasure(tree.symbol)(qual.tpe)).copyAttrs(fn),
              args)
          }
        case Apply(fn @ Select(qual, _), Nil) if interceptedMethods(fn.symbol) =>
          if (fn.symbol == Any_## || fn.symbol == Object_##) {
            // This is unattractive, but without it we crash here on ().## because after
            // erasure the ScalaRunTime.hash overload goes from Unit => Int to BoxedUnit => Int.
            // This must be because some earlier transformation is being skipped on ##, but so
            // far I don't know what.  For null we now define null.## == 0.
            qual.tpe.typeSymbol match {
              case UnitClass | NullClass                    => LIT(0)
              case IntClass                                 => qual
              case s @ (ShortClass | ByteClass | CharClass) => numericConversion(qual, s)
              case BooleanClass                             => If(qual, LIT(true.##), LIT(false.##))
              case _                                        =>
                global.typer.typed(gen.mkRuntimeCall(nme.hash_, List(qual)))
            }
          }
          // Rewrite 5.getClass to ScalaRunTime.anyValClass(5)
          else if (isPrimitiveValueClass(qual.tpe.typeSymbol))
            global.typer.typed(gen.mkRuntimeCall(nme.anyValClass, List(qual, typer.resolveClassTag(tree.pos, qual.tpe.widen))))
          else
            tree

        case Apply(Select(New(tpt), nme.CONSTRUCTOR), List(arg)) if (tpt.tpe.typeSymbol.isDerivedValueClass) =>
          InjectDerivedValue(arg) setSymbol tpt.tpe.typeSymbol
        case Apply(fn, args) =>
          def qualifier = fn match {
            case Select(qual, _) => qual
            case TypeApply(Select(qual, _), _) => qual
          }
          if (fn.symbol == Any_asInstanceOf)
            (fn: @unchecked) match {
              case TypeApply(Select(qual, _), List(targ)) =>
                if (qual.tpe <:< targ.tpe)
                  atPos(tree.pos) { Typed(qual, TypeTree(targ.tpe)) }
                else if (isNumericValueClass(qual.tpe.typeSymbol) && isNumericValueClass(targ.tpe.typeSymbol))
                  atPos(tree.pos)(numericConversion(qual, targ.tpe.typeSymbol))
                else
                  tree
            }
            // todo: also handle the case where the singleton type is buried in a compound
          else if (fn.symbol == Any_isInstanceOf) {
            fn match {
              case TypeApply(sel @ Select(qual, name), List(targ)) =>
                if (qual.tpe != null && isPrimitiveValueClass(qual.tpe.typeSymbol) && targ.tpe != null && targ.tpe <:< AnyRefClass.tpe)
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
          } else if (fn.symbol.owner.isRefinementClass && !fn.symbol.isOverridingSymbol) {
            ApplyDynamic(qualifier, args) setSymbol fn.symbol setPos tree.pos
          } else if (fn.symbol.isMethodWithExtension) {
            Apply(gen.mkAttributedRef(extensionMethods.extensionMethod(fn.symbol)), qualifier :: args)
          } else {
                tree
            }

        case Select(qual, name) =>
          val owner = tree.symbol.owner
          // println("preXform: "+ (tree, tree.symbol, tree.symbol.owner, tree.symbol.owner.isRefinementClass))
          if (owner.isRefinementClass) {
            val overridden = tree.symbol.nextOverriddenSymbol
            assert(overridden != NoSymbol, tree.symbol)
            tree.symbol = overridden
          }

          def isAccessible(sym: Symbol) = localTyper.context.isAccessible(sym, sym.owner.thisType)
          if (!isAccessible(owner) && qual.tpe != null) {
            qual match {
              case Super(_, _) =>
                // Insert a cast here at your peril -- see SI-5162. Bail out if the target method is defined in
                // Java, otherwise, we'd get an IllegalAccessError at runtime. If the target method is defined in
                // Scala, however, we should have access.
                if (owner.isJavaDefined) unit.error(tree.pos, s"Unable to access ${tree.symbol.fullLocationString} with a super reference.")
                tree
              case _ =>
                // Todo: Figure out how qual.tpe could be null in the check above (it does appear in build where SwingWorker.this
                // has a null type).
                val qualSym = qual.tpe.widen.typeSymbol
                if (isAccessible(qualSym) && !qualSym.isPackageClass && !qualSym.isPackageObjectClass) {
                  // insert cast to prevent illegal access error (see #4283)
                  // util.trace("insert erasure cast ") (*/
                  treeCopy.Select(tree, gen.mkAttributedCast(qual, qual.tpe.widen), name) //)
                } else tree
            }
          } else tree
        case Template(parents, self, body) =>
          assert(!currentOwner.isImplClass)
          //Console.println("checking no dble defs " + tree)//DEBUG
          checkNoDoubleDefs(tree.symbol.owner)
          treeCopy.Template(tree, parents, emptyValDef, addBridges(body, currentOwner))

        case Match(selector, cases) =>
          Match(Typed(selector, TypeTree(selector.tpe)), cases)

        case Literal(ct) if ct.tag == ClazzTag
                         && ct.typeValue.typeSymbol != definitions.UnitClass =>
          val erased = ct.typeValue match {
            case TypeRef(pre, clazz, args) if clazz.isDerivedValueClass => scalaErasure.eraseNormalClassRef(pre, clazz)
            case tpe => specialScalaErasure(tpe)
          }
          treeCopy.Literal(tree, Constant(erased))

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
              tree1 setType specialScalaErasure(tree1.tpe)
            case ArrayValue(elemtpt, trees) =>
              treeCopy.ArrayValue(
                tree1, elemtpt setType specialScalaErasure.applyInArray(elemtpt.tpe), trees map transform) setType null
            case DefDef(_, _, _, _, tpt, _) =>
              val result = super.transform(tree1) setType null
              tpt.tpe = specialErasure(tree1.symbol)(tree1.symbol.tpe).resultType
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
      // log("tree after pretransform: "+tree1)
      afterErasure {
        val tree2 = mixinTransformer.transform(tree1)
        // debuglog("tree after addinterfaces: \n" + tree2)

        newTyper(rootContext(unit, tree, true)).typed(tree2)
      }
    }
  }
}
