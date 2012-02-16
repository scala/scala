/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import scala.tools.reflect.SigParser
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

  // A type function from T => Class[U], used to determine the return
  // type of getClass calls.  The returned type is:
  //
  //  1. If T is a value type, Class[T].
  //  2. If T is a phantom type (Any or AnyVal), Class[_].
  //  3. If T is a local class, Class[_ <: |T|].
  //  4. Otherwise, Class[_ <: T].
  //
  // Note: AnyVal cannot be Class[_ <: AnyVal] because if the static type of the
  // receiver is AnyVal, it implies the receiver is boxed, so the correct
  // class object is that of java.lang.Integer, not Int.
  //
  // TODO: If T is final, return type could be Class[T].  Should it?
  def getClassReturnType(tpe: Type): Type = {
    if (phase.erasedTypes) ClassClass.tpe else {
      val tp  = tpe.widen.normalize
      val sym = tp.typeSymbol

      if (isValueClass(sym)) ClassType(tp)
      else boundedClassType(
        if (isPhantomClass(sym)) ObjectClass.tpe
        else if (sym.isLocalClass) intersectionDominator(tp.parents)
        else tp
      )
    }
  }

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

  // for debugging signatures: traces logic given system property
  // performance: get the value here
  val traceSignatures = (sys.BooleanProp keyExists "scalac.sigs.trace").value
  private object traceSig extends util.Tracer(() => traceSignatures) {
    override def stringify(x: Any) = x match {
      case tp: Type   => super.stringify(dropAllRefinements(tp))
      case _          => super.stringify(x)
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

  /** Run the signature parser to catch bogus signatures.
   */
  def isValidSignature(sym: Symbol, sig: String) = (
    /** Since we're using a sun internal class for signature validation,
     *  we have to allow for it not existing or otherwise malfunctioning:
     *  in which case we treat every signature as valid.  Medium term we
     *  should certainly write independent signature validation.
     */
    SigParser.isParserAvailable && (
      if (sym.isMethod) SigParser verifyMethod sig
      else if (sym.isTerm) SigParser verifyType sig
      else SigParser verifyClass sig
    )
  )

  private def hiBounds(bounds: TypeBounds): List[Type] = bounds.hi.normalize match {
    case RefinedType(parents, _) => parents map (_.normalize)
    case tp                      => tp :: Nil
  }

  /** The Java signature of type 'info', for symbol sym. The symbol is used to give the right return
   *  type for constructors.
   */
  def javaSig(sym0: Symbol, info: Type): Option[String] = atPhase(currentRun.erasurePhase) {
    val isTraitSignature = sym0.enclClass.isTrait

    def superSig(parents: List[Type]) = traceSig("superSig", parents) {
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
      ps map boxedSig mkString
    }
    def boxedSig(tp: Type) = jsig(tp, primitiveOK = false)
    def boundsSig(bounds: List[Type]) = traceSig("boundsSig", bounds) {
      val (isTrait, isClass) = bounds partition (_.typeSymbol.isTrait)
      val classPart = isClass match {
        case Nil    => ":" // + boxedSig(ObjectClass.tpe)
        case x :: _ => ":" + boxedSig(x)
      }
      classPart :: (isTrait map boxedSig) mkString ":"
    }
    def paramSig(tsym: Symbol) = tsym.name + boundsSig(hiBounds(tsym.info.bounds))
    def polyParamSig(tparams: List[Symbol]) = traceSig("polyParamSig", tparams) (
      if (tparams.isEmpty) ""
      else tparams map paramSig mkString ("<", "", ">")
    )

    // Anything which could conceivably be a module (i.e. isn't known to be
    // a type parameter or similar) must go through here or the signature is
    // likely to end up with Foo<T>.Empty where it needs Foo<T>.Empty$.
    def fullNameInSig(sym: Symbol) = "L" + atPhase(currentRun.icodePhase)(sym.javaBinaryName)

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
          else if (isValueClass(sym)) {
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
          else jsig(erasure(sym0, tp), existentiallyBound, toplevel, primitiveOK)
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
          val etp = erasure(sym0, tp)
          if (etp eq tp) throw new UnknownSig
          else jsig(etp)
      }
    }
    val result = traceSig("javaSig", (sym0, info)) {
      if (needsJavaSig(info)) {
        try Some(jsig(info, toplevel = true))
        catch { case ex: UnknownSig => None }
      }
      else None
    }
    // Debugging: immediately verify signatures when tracing.
    if (traceSignatures) {
      result foreach { sig =>
        if (!isValidSignature(sym0, sig))
          println("**** invalid signature for " + sym0 + ": " + sig)
      }
    }

    result
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

  /** An extractor object for boxed expressions
  object Boxed {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case LabelDef(name, params, Boxed(rhs)) =>
        Some(treeCopy.LabelDef(tree, name, params, rhs) setType rhs.tpe)
      case Select(_, _) if tree.symbol == BoxedUnit_UNIT =>
        Some(Literal(Constant()) setPos tree.pos setType UnitClass.tpe)
      case Block(List(unboxed), ret @ Select(_, _)) if ret.symbol == BoxedUnit_UNIT =>
        Some(if (unboxed.tpe.typeSymbol == UnitClass) tree
             else Block(List(unboxed), Literal(Constant()) setPos tree.pos setType UnitClass.tpe))
      case Apply(fn, List(unboxed)) if isBox(fn.symbol) =>
        Some(unboxed)
      case _ =>
        None
    }
  }
   */

  /** The modifier typer which retypes with erased types. */
  class Eraser(_context: Context) extends Typer(_context) {
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
            if (treeInfo isExprSafeToInline tree) UNIT
            else BLOCK(tree, UNIT)
          case x          =>
            assert(x != ArrayClass)
            // don't `setType pt` the Apply tree, as the Apply's fun won't be typechecked if the Apply tree already has a type
            Apply(unboxMethod(pt.typeSymbol), tree)
        })
    }

    /** Generate a synthetic cast operation from tree.tpe to pt.
     *  @pre pt eq pt.normalize
     */
    private def cast(tree: Tree, pt: Type): Tree = {
      if (pt.typeSymbol == UnitClass) {
        // See SI-4731 for one example of how this occurs.
        log("Attempted to cast to Unit: " + tree)
        tree.duplicate setType pt
      }
      else gen.mkAttributedCast(tree, pt)
    }

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
        assert(tree.symbol.isStable, "adapt "+tree+":"+tree.tpe+" to "+pt)
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
          try Console.println("exception when typing " + tree)
          finally throw ex
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
                atPhase(phase.next)(member.tpe =:= other.tpe) &&
                !atPhase(refchecksPhase.next)(
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
      var toBeRemoved: immutable.Set[Symbol] = immutable.Set()
      //println("computing bridges for " + owner)//DEBUG
      assert(phase == currentRun.erasurePhase)
      val site = owner.thisType
      val bridgesScope = newScope
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
        //println("bridge? " + member + ":" + member.tpe + member.locationString + " to " + other + ":" + other.tpe + other.locationString)//DEBUG
        if (atPhase(currentRun.explicitouterPhase)(!member.isDeferred)) {
          val otpe = erasure(owner, other.tpe)
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
            val newFlags = (member.flags | BRIDGE) & ~(ACCESSOR | DEFERRED | LAZY | lateDEFERRED)
            val bridge   = other.cloneSymbolImpl(owner, newFlags) setPos owner.pos
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
                          val bridgingCall = (((Select(This(owner), member): Tree) /: bridge.paramss)
                             ((fun, vparams) => Apply(fun, vparams map Ident)))
                          // type checking ensures we can safely call `other`, but unless `member.tpe <:< other.tpe`, calling `member` is not guaranteed to succeed
                          // in general, there's nothing we can do about this, except for an unapply: when this subtype test fails, return None without calling `member`
                          if (  member.isSynthetic // TODO: should we do this for user-defined unapplies as well?
                             && ((member.name == nme.unapply) || (member.name == nme.unapplySeq))
                             // && (bridge.paramss.nonEmpty && bridge.paramss.head.nonEmpty && bridge.paramss.head.tail.isEmpty) // does the first argument list has exactly one argument -- for user-defined unapplies we can't be sure
                             && !(atPhase(phase.next)(member.tpe <:< other.tpe))) { // no static guarantees (TODO: is the subtype test ever true?)
                            import CODE._
                            val typeTest = gen.mkIsInstanceOf(REF(bridge.firstParam), member.tpe.params.head.tpe, any = true, wrapInApply = true) // any = true since we're before erasure (?), wrapInapply is true since we're after uncurry
                            // println("unapp type test: "+ typeTest)
                            IF (typeTest) THEN bridgingCall ELSE REF(NoneModule)
                          } else bridgingCall
                      });
                  debuglog("generating bridge from " + other + "(" + Flags.flagsToString(bridge.flags)  + ")" + ":" + otpe + other.locationString + " to " + member + ":" + erasure(owner, member.tpe) + member.locationString + " =\n " + bridgeDef);
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
          debuglog("defs of " + tree.symbol + " = " + tree.symbol.info.decls)
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
            gen.mkRuntimeCall(nme.isArray, List(arg, Literal(Constant(level))))

          global.typer.typedPos(tree.pos) {
            if (level == 1) isArrayTest(qual)
            else gen.evalOnce(qual, currentOwner, unit) { qual1 =>
              gen.mkAnd(
                gen.mkMethodCall(
                  qual1(),
                  fun.symbol,
                  List(erasure(fun.symbol, arg.tpe)),
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
              SelectFromArray(qual, name, erasure(tree.symbol, qual.tpe)).copyAttrs(fn),
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
          else if (isValueClass(qual.tpe.typeSymbol))
            global.typer.typed(gen.mkRuntimeCall(nme.anyValClass, List(qual)))
          else
            tree

        case Apply(fn, args) =>
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
              if (fn.symbol.owner.isRefinementClass && !fn.symbol.isOverridingSymbol)
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
            val overridden = tree.symbol.nextOverriddenSymbol
            assert(overridden != NoSymbol, tree.symbol)
            tree.symbol = overridden
          }
          def isAccessible(sym: Symbol) = localTyper.context.isAccessible(sym, sym.owner.thisType)
          if (!isAccessible(owner) && qual.tpe != null) {
            // Todo: Figure out how qual.tpe could be null in the check above (it does appear in build where SwingWorker.this
            // has a null type).
            val qualSym = qual.tpe.widen.typeSymbol
            if (isAccessible(qualSym) && !qualSym.isPackageClass && !qualSym.isPackageObjectClass) {
              // insert cast to prevent illegal access error (see #4283)
              // util.trace("insert erasure cast ") (*/
              treeCopy.Select(tree, gen.mkAttributedCast(qual, qual.tpe.widen), name) //)
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
          treeCopy.Literal(tree, Constant(erasure(NoSymbol, ct.typeValue)))

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
              tree1 setType erasure(NoSymbol, tree1.tpe)
            case DefDef(_, _, _, _, tpt, _) =>
              val result = super.transform(tree1) setType null
              tpt.tpe = erasure(tree1.symbol, tree1.symbol.tpe).resultType
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
        debuglog("tree after addinterfaces: \n" + tree2)

        newTyper(rootContext(unit, tree, true)).typed(tree2)
      }
    }
  }
}
