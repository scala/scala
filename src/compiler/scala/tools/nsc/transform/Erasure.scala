/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import scala.reflect.internal.ClassfileConstants._
import scala.collection.{ mutable, immutable }
import symtab._
import Flags._
import scala.reflect.internal.Mode._

abstract class Erasure extends AddInterfaces
                          with scala.reflect.internal.transform.Erasure
                          with typechecker.Analyzer
                          with TypingTransformers
                          with ast.TreeDSL
                          with TypeAdaptingTransformer
{
  import global._
  import definitions._
  import CODE._

  val analyzer: typechecker.Analyzer { val global: Erasure.this.global.type } =
    this.asInstanceOf[typechecker.Analyzer { val global: Erasure.this.global.type }]

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
            else if (!sym.isTopLevel) traverse(pre)
          case PolyType(_, _) | ExistentialType(_, _) =>
            result = true
          case RefinedType(parents, _) =>
            parents foreach traverse
          case ClassInfoType(parents, _, _) =>
            parents foreach traverse
          case AnnotatedType(_, atp) =>
            traverse(atp)
          case _ =>
            mapOver(tp)
        }
      }
    }
  }

  override protected def verifyJavaErasure = settings.Xverify || settings.debug
  def needsJavaSig(tp: Type) = !settings.Ynogenericsig && NeedsSigCollector.collect(tp)

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
    // OPT 50% of time in generic signatures (~1% of compile time) was in this method, hence the imperative rewrite.
    var last: Char = '\u0000'
    var i = 0
    val len = sig.length
    val copy: Array[Char] = sig.toCharArray
    var changed = false
    while (i < len) {
      val ch = copy(i)
      if (ch == '.' && last != '>') {
         copy(i) = '$'
         changed = true
      }
      last = ch
      i += 1
    }
    if (changed) new String(copy) else sig
  }

  /** This object is only used for sanity testing when -check:genjvm is set.
   *  In that case we make sure that the erasure of the `normalized` type
   *  is the same as the erased type that's generated. Normalization means
   *  unboxing some primitive types and further simplifications as they are done in jsig.
   */
  val prepareSigMap = new TypeMap {
    def squashBoxed(tp: Type): Type = tp.dealiasWiden match {
      case t @ RefinedType(parents, decls) =>
        val parents1 = parents mapConserve squashBoxed
        if (parents1 eq parents) tp
        else RefinedType(parents1, decls)
      case t @ ExistentialType(tparams, tpe) =>
        val tpe1 = squashBoxed(tpe)
        if (tpe1 eq tpe) t
        else ExistentialType(tparams, tpe1)
      case t =>
        if (boxedClass contains t.typeSymbol) ObjectTpe
        else tp
    }
    def apply(tp: Type): Type = tp.dealiasWiden match {
      case tp1 @ TypeBounds(lo, hi) =>
        val lo1 = squashBoxed(apply(lo))
        val hi1 = squashBoxed(apply(hi))
        if ((lo1 eq lo) && (hi1 eq hi)) tp1
        else TypeBounds(lo1, hi1)
      case tp1 @ TypeRef(pre, sym, args) =>
        def argApply(tp: Type) = {
          val tp1 = apply(tp)
          if (tp1.typeSymbol == UnitClass) ObjectTpe
          else squashBoxed(tp1)
        }
        if (sym == ArrayClass && args.nonEmpty)
          if (unboundedGenericArrayLevel(tp1) == 1) ObjectTpe
          else mapOver(tp1)
        else if (sym == AnyClass || sym == AnyValClass || sym == SingletonClass)
          ObjectTpe
        else if (sym == UnitClass)
          BoxedUnitTpe
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
        val restpe1 = if (restpe.typeSymbol == UnitClass) UnitTpe else apply(restpe)
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

  private def hiBounds(bounds: TypeBounds): List[Type] = bounds.hi.dealiasWiden match {
    case RefinedType(parents, _) => parents map (_.dealiasWiden)
    case tp                      => tp :: Nil
  }

  private def isErasedValueType(tpe: Type) = tpe.isInstanceOf[ErasedValueType]

  /* Drop redundant types (ones which are implemented by some other parent) from the immediate parents.
   * This is important on Android because there is otherwise an interface explosion.
   */
  def minimizeParents(parents: List[Type]): List[Type] = if (parents.isEmpty) parents else {
    def isInterfaceOrTrait(sym: Symbol) = sym.isInterface || sym.isTrait

    var rest   = parents.tail
    var leaves = collection.mutable.ListBuffer.empty[Type] += parents.head
    while(rest.nonEmpty) {
      val candidate = rest.head
      val nonLeaf = leaves exists { t => t.typeSymbol isSubClass candidate.typeSymbol }
      if(!nonLeaf) {
        leaves = leaves filterNot { t => isInterfaceOrTrait(t.typeSymbol) && (candidate.typeSymbol isSubClass t.typeSymbol) }
        leaves += candidate
      }
      rest = rest.tail
    }
    leaves.toList
  }


  /** The Java signature of type 'info', for symbol sym. The symbol is used to give the right return
   *  type for constructors.
   */
  def javaSig(sym0: Symbol, info: Type): Option[String] = enteringErasure {
    val isTraitSignature = sym0.enclClass.isTrait

    def superSig(parents: List[Type]) = {
      def isInterfaceOrTrait(sym: Symbol) = sym.isInterface || sym.isTrait

      // a signature should always start with a class
      def ensureClassAsFirstParent(tps: List[Type]) = tps match {
        case Nil => ObjectTpe :: Nil
        case head :: tail if isInterfaceOrTrait(head.typeSymbol) => ObjectTpe :: tps
        case _ => tps
      }

      val minParents = minimizeParents(parents)
      val validParents =
        if (isTraitSignature)
          // java is unthrilled about seeing interfaces inherit from classes
          minParents filter (p => isInterfaceOrTrait(p.typeSymbol))
        else minParents

      val ps = ensureClassAsFirstParent(validParents)

      (ps map boxedSig).mkString
    }
    def boxedSig(tp: Type) = jsig(tp, primitiveOK = false)
    def boundsSig(bounds: List[Type]) = {
      val (isTrait, isClass) = bounds partition (_.typeSymbol.isTrait)
      val classPart = isClass match {
        case Nil    => ":" // + boxedSig(ObjectTpe)
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
    def fullNameInSig(sym: Symbol) = "L" + enteringIcode(sym.javaBinaryName)

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
              if (!(AnyRefTpe <:< bounds.hi)) "+" + boxedSig(bounds.hi)
              else if (!(bounds.lo <:< NullTpe)) "-" + boxedSig(bounds.lo)
              else "*"
            } else tp match {
              case PolyType(_, res) =>
                "*" // SI-7932
              case _ =>
                boxedSig(tp)
            }
          def classSig = {
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

          // If args isEmpty, Array is being used as a type constructor
          if (sym == ArrayClass && args.nonEmpty) {
            if (unboundedGenericArrayLevel(tp) == 1) jsig(ObjectTpe)
            else ARRAY_TAG.toString+(args map (jsig(_))).mkString
          }
          else if (isTypeParameterInSig(sym, sym0)) {
            assert(!sym.isAliasType, "Unexpected alias type: " + sym)
            "" + TVAR_TAG + sym.name + ";"
          }
          else if (sym == AnyClass || sym == AnyValClass || sym == SingletonClass)
            jsig(ObjectTpe)
          else if (sym == UnitClass)
            jsig(BoxedUnitTpe)
          else if (sym == NothingClass)
            jsig(RuntimeNothingClass.tpe)
          else if (sym == NullClass)
            jsig(RuntimeNullClass.tpe)
          else if (isPrimitiveValueClass(sym)) {
            if (!primitiveOK) jsig(ObjectTpe)
            else if (sym == UnitClass) jsig(BoxedUnitTpe)
            else abbrvTag(sym).toString
          }
          else if (sym.isDerivedValueClass) {
            val unboxed     = sym.derivedValueClassUnbox.tpe_*.finalResultType
            val unboxedSeen = (tp memberType sym.derivedValueClassUnbox).finalResultType
            def unboxedMsg  = if (unboxed == unboxedSeen) "" else s", seen within ${sym.simpleName} as $unboxedSeen"
            logResult(s"Erasure of value class $sym (underlying type $unboxed$unboxedMsg) is") {
              if (isPrimitiveValueType(unboxedSeen) && !primitiveOK)
                classSig
              else
                jsig(unboxedSeen, existentiallyBound, toplevel, primitiveOK)
            }
          }
          else if (sym.isClass)
            classSig
          else
            jsig(erasure(sym0)(tp), existentiallyBound, toplevel, primitiveOK)
        case PolyType(tparams, restpe) =>
          assert(tparams.nonEmpty)
          val poly = if (toplevel) polyParamSig(tparams) else ""
          poly + jsig(restpe)

        case MethodType(params, restpe) =>
          val buf = new StringBuffer("(")
          params foreach (p => buf append jsig(p.tpe))
          buf append ")"
          buf append (if (restpe.typeSymbol == UnitClass || sym0.isConstructor) VOID_TAG.toString else jsig(restpe))
          buf.toString

        case RefinedType(parent :: _, decls) =>
          boxedSig(parent)
        case ClassInfoType(parents, _, _) =>
          superSig(parents)
        case AnnotatedType(_, atp) =>
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

  // ## requires a little translation
  private lazy val poundPoundMethods = Set[Symbol](Any_##, Object_##)
  // Methods on Any/Object which we rewrite here while we still know what
  // is a primitive and what arrived boxed.
  private lazy val interceptedMethods = poundPoundMethods ++ primitiveGetClassMethods

// -------- erasure on trees ------------------------------------------

  override def newTyper(context: Context) = new Eraser(context)

  class ComputeBridges(unit: CompilationUnit, root: Symbol) {
    assert(phase == currentRun.erasurePhase, phase)

    var toBeRemoved  = immutable.Set[Symbol]()
    val site         = root.thisType
    val bridgesScope = newScope
    val bridgeTarget = mutable.HashMap[Symbol, Symbol]()
    var bridges      = List[Tree]()

    val opc = enteringExplicitOuter {
      new overridingPairs.Cursor(root) {
        override def parents              = List(root.info.firstParent)
        override def exclude(sym: Symbol) = !sym.isMethod || super.exclude(sym)
      }
    }

    def compute(): (List[Tree], immutable.Set[Symbol]) = {
      while (opc.hasNext) {
        if (enteringExplicitOuter(!opc.low.isDeferred))
          checkPair(opc.currentPair)

        opc.next()
      }
      (bridges, toBeRemoved)
    }

    /** Check that a bridge only overrides members that are also overridden by the original member.
     *  This test is necessary only for members that have a value class in their type.
     *  Such members are special because their types after erasure and after post-erasure differ/.
     *  This means we generate them after erasure, but the post-erasure transform might introduce
     *  a name clash. The present method guards against these name clashes.
     *
     *  @param  member   The original member
     *  @param  other    The overridden symbol for which the bridge was generated
     *  @param  bridge   The bridge
     */
    def checkBridgeOverrides(member: Symbol, other: Symbol, bridge: Symbol): Seq[(Position, String)] = {
      def fulldef(sym: Symbol) =
        if (sym == NoSymbol) sym.toString
        else s"$sym: ${sym.tpe} in ${sym.owner}"
      val clashErrors = mutable.Buffer[(Position, String)]()
      def clashError(what: String) = {
        val pos = if (member.owner == root) member.pos else root.pos
        val msg = sm"""bridge generated for member ${fulldef(member)}
                      |which overrides ${fulldef(other)}
                      |clashes with definition of $what;
                      |both have erased type ${exitingPostErasure(bridge.tpe)}"""
        clashErrors += Tuple2(pos, msg)
      }
      for (bc <- root.baseClasses) {
        if (settings.debug)
          exitingPostErasure(println(
            sm"""check bridge overrides in $bc
                |${bc.info.nonPrivateDecl(bridge.name)}
                |${site.memberType(bridge)}
                |${site.memberType(bc.info.nonPrivateDecl(bridge.name) orElse IntClass)}
                |${(bridge.matchingSymbol(bc, site))}"""))

        def overriddenBy(sym: Symbol) =
          sym.matchingSymbol(bc, site).alternatives filter (sym => !sym.isBridge)
        for (overBridge <- exitingPostErasure(overriddenBy(bridge))) {
          if (overBridge == member) {
            clashError("the member itself")
          } else {
            val overMembers = overriddenBy(member)
            if (!overMembers.exists(overMember =>
              exitingPostErasure(overMember.tpe =:= overBridge.tpe))) {
              clashError(fulldef(overBridge))
            }
          }
        }
      }
      clashErrors
    }

    /** TODO - work through this logic with a fine-toothed comb, incorporating
     *  into SymbolPairs where appropriate.
     */
    def checkPair(pair: SymbolPair) {
      import pair._
      val member = low
      val other  = high
      val otpe   = highErased

      val bridgeNeeded = exitingErasure (
        !member.isMacro &&
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

      var newFlags = (member.flags | BRIDGE | ARTIFACT) & ~(ACCESSOR | DEFERRED | LAZY | lateDEFERRED)
      // If `member` is a ModuleSymbol, the bridge should not also be a ModuleSymbol. Otherwise we
      // end up with two module symbols with the same name in the same scope, which is surprising
      // when implementing later phases.
      if (member.isModule) newFlags = (newFlags | METHOD) & ~(MODULE | lateMETHOD | STABLE)
      val bridge = other.cloneSymbolImpl(root, newFlags) setPos root.pos

      debuglog("generating bridge from %s (%s): %s to %s: %s".format(
        other, flagsToString(newFlags),
        otpe + other.locationString, member,
        specialErasure(root)(member.tpe) + member.locationString)
      )

      // the parameter symbols need to have the new owner
      bridge setInfo (otpe cloneInfo bridge)
      bridgeTarget(bridge) = member

      def sigContainsValueClass = (member.tpe exists (_.typeSymbol.isDerivedValueClass))

      val shouldAdd = (
            !sigContainsValueClass
        ||  (checkBridgeOverrides(member, other, bridge) match {
              case Nil => true
              case es if member.owner.isAnonymousClass => resolveAnonymousBridgeClash(member, bridge); true
              case es => for ((pos, msg) <- es) reporter.error(pos, msg); false
            })
      )

      if (shouldAdd) {
        exitingErasure(root.info.decls enter bridge)
        if (other.owner == root) {
          exitingErasure(root.info.decls.unlink(other))
          toBeRemoved += other
        }

        bridgesScope enter bridge
        bridges ::= makeBridgeDefDef(bridge, member, other)
      }
    }

    def makeBridgeDefDef(bridge: Symbol, member: Symbol, other: Symbol) = exitingErasure {
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
          && !exitingErasure((member.tpe <:< other.tpe))) // no static guarantees (TODO: is the subtype test ever true?)

        import CODE._
        val _false    = FALSE
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
          val sel: Tree    = Select(This(root), member)
          val bridgingCall = (sel /: bridge.paramss)((fun, vparams) => Apply(fun, vparams map Ident))

          maybeWrap(bridgingCall)
      }
      DefDef(bridge, rhs)
    }
  }

  /** The modifier typer which retypes with erased types. */
  class Eraser(_context: Context) extends Typer(_context) with TypeAdapter {
    val typer = this.asInstanceOf[analyzer.Typer]

    override protected def stabilize(tree: Tree, pre: Type, mode: Mode, pt: Type): Tree = tree

    /**  Replace member references as follows:
     *
     *   - `x == y` for == in class Any becomes `x equals y` with equals in class Object.
     *   - `x != y` for != in class Any becomes `!(x equals y)` with equals in class Object.
     *   - x.asInstanceOf[T] becomes x.$asInstanceOf[T]
     *   - x.isInstanceOf[T] becomes x.$isInstanceOf[T]
     *   - x.isInstanceOf[ErasedValueType(tref)] becomes x.isInstanceOf[tref.sym.tpe]
     *   - x.m where m is some other member of Any becomes x.m where m is a member of class Object.
     *   - x.m where x has unboxed value type T and m is not a directly translated member of T becomes T.box(x).m
     *   - x.m where x is a reference type and m is a directly translated member of value type T becomes x.TValue().m
     *   - All forms of x.m where x is a boxed type and m is a member of an unboxed class become
     *     x.m where m is the corresponding member of the boxed class.
     */
    private def adaptMember(tree: Tree): Tree = {
      //Console.println("adaptMember: " + tree);
      tree match {
        case Apply(ta @ TypeApply(sel @ Select(qual, name), List(targ)), List())
        if tree.symbol == Any_asInstanceOf =>
          val qual1 = typedQualifier(qual, NOmode, ObjectTpe) // need to have an expected type, see #3037
          // !!! Make pending/run/t5866b.scala work. The fix might be here and/or in unbox1.
          if (isPrimitiveValueType(targ.tpe) || isErasedValueType(targ.tpe)) {
            val noNullCheckNeeded = targ.tpe match {
              case ErasedValueType(_, underlying) =>
                isPrimitiveValueClass(underlying.typeSymbol)
              case _ =>
                true
            }
            if (noNullCheckNeeded) unbox(qual1, targ.tpe)
            else {
              val untyped =
//                util.trace("new asinstanceof test") {
                  gen.evalOnce(qual1, context.owner, context.unit) { qual =>
                    If(Apply(Select(qual(), nme.eq), List(Literal(Constant(null)) setType NullTpe)),
                       Literal(Constant(null)) setType targ.tpe,
                       unbox(qual(), targ.tpe))
                  }
//                }
              typed(untyped)
            }
          } else treeCopy.Apply(tree, treeCopy.TypeApply(ta, treeCopy.Select(sel, qual1, name), List(targ)), List())

        case Apply(TypeApply(sel @ Select(qual, name), List(targ)), List())
        if tree.symbol == Any_isInstanceOf =>
          targ.tpe match {
            case ErasedValueType(clazz, _) => targ.setType(clazz.tpe)
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
            adaptMember(atPos(tree.pos)(Select(qual, getMember(ObjectClass, tree.symbol.name))))
          else {
            var qual1 = typedQualifier(qual)
            if ((isPrimitiveValueType(qual1.tpe) && !isPrimitiveValueMember(tree.symbol)) ||
                 isErasedValueType(qual1.tpe))
              qual1 = box(qual1, "owner "+tree.symbol.owner)
            else if (!isPrimitiveValueType(qual1.tpe) && isPrimitiveValueMember(tree.symbol))
              qual1 = unbox(qual1, tree.symbol.owner.tpe)

            def selectFrom(qual: Tree) = treeCopy.Select(tree, qual, name)

            if (isPrimitiveValueMember(tree.symbol) && !isPrimitiveValueType(qual1.tpe)) {
              tree.symbol = NoSymbol
              selectFrom(qual1)
            } else if (isMethodTypeWithEmptyParams(qual1.tpe)) {
              assert(qual1.symbol.isStable, qual1.symbol)
              val applied = Apply(qual1, List()) setPos qual1.pos setType qual1.tpe.resultType
              adaptMember(selectFrom(applied))
            } else if (!(qual1.isInstanceOf[Super] || (qual1.tpe.typeSymbol isSubClass tree.symbol.owner))) {
              assert(tree.symbol.owner != ArrayClass)
              selectFrom(cast(qual1, tree.symbol.owner.tpe))
            } else {
              selectFrom(qual1)
            }
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
    override protected def adapt(tree: Tree, mode: Mode, pt: Type, original: Tree = EmptyTree): Tree =
      adaptToType(tree, pt)

    /** A replacement for the standard typer's `typed1` method.
     */
    override def typed1(tree: Tree, mode: Mode, pt: Type): Tree = {
      val tree1 = try {
        tree match {
          case InjectDerivedValue(arg) =>
            (tree.attachments.get[TypeRefAttachment]: @unchecked) match {
              case Some(itype) =>
                val tref = itype.tpe
                val argPt = enteringErasure(erasedValueClassArg(tref))
                log(s"transforming inject $arg -> $tref/$argPt")
                val result = typed(arg, mode, argPt)
                log(s"transformed inject $arg -> $tref/$argPt = $result:${result.tpe}")
                return result setType ErasedValueType(tref.sym, result.tpe)

            }
          case _ =>
            super.typed1(adaptMember(tree), mode, pt)
        }
      } catch {
        case er: TypeError =>
          Console.println("exception when typing " + tree+"/"+tree.getClass)
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
        if (branch == EmptyTree) branch else adaptToType(branch, tree1.tpe)

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
              tree1 setSymbol sym1 setType sym1.tpe
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
    import overridingPairs.Cursor

    private def doubleDefError(pair: SymbolPair) {
      import pair._

      if (!pair.isErroneous) {
        val what = (
          if (low.owner == high.owner) "double definition"
          else if (low.owner == base) "name clash between defined and inherited member"
          else "name clash between inherited members"
        )
        val when = if (exitingRefchecks(lowType matches highType)) "" else " after erasure: " + exitingPostErasure(highType)

        reporter.error(pos,
          s"""|$what:
              |${exitingRefchecks(highString)} and
              |${exitingRefchecks(lowString)}
              |have same type$when""".trim.stripMargin
        )
      }
      low setInfo ErrorType
    }

    private def sameTypeAfterErasure(sym1: Symbol, sym2: Symbol) =
      exitingPostErasure(sym1.info =:= sym2.info) && !sym1.isMacro && !sym2.isMacro

    /** TODO - adapt SymbolPairs so it can be used here. */
    private def checkNoDeclaredDoubleDefs(base: Symbol) {
      val decls = base.info.decls

      // SI-8010 force infos, otherwise makeNotPrivate in ExplicitOuter info transformer can trigger
      //         a scope rehash while were iterating and we can see the same entry twice!
      //         Inspection of SymbolPairs (the basis of OverridingPairs), suggests that it is immune
      //         from this sort of bug as it copies the symbols into a temporary scope *before* any calls to `.info`,
      //         ie, no variant of it calls `info` or `tpe` in `SymbolPair#exclude`.
      //
      //         Why not just create a temporary scope here? We need to force the name changes in any case before
      //         we do these checks, so that we're comparing same-named methods based on the expanded names that actually
      //         end up in the bytecode.
      exitingPostErasure(decls.foreach(_.info))

      var e = decls.elems
      while (e ne null) {
        if (e.sym.isTerm) {
          var e1 = decls lookupNextEntry e
          while (e1 ne null) {
            assert(e.sym ne e1.sym, s"Internal error: encountered ${e.sym.debugLocationString} twice during scope traversal. This might be related to SI-8010.")
            if (sameTypeAfterErasure(e.sym, e1.sym))
              doubleDefError(new SymbolPair(base, e.sym, e1.sym))

            e1 = decls lookupNextEntry e1
          }
        }
        e = e.next
      }
    }

    /** Emit an error if there is a double definition. This can happen if:
     *
     *  - A template defines two members with the same name and erased type.
     *  - A template defines and inherits two members `m` with different types,
     *    but their erased types are the same.
     *  - A template inherits two members `m` with different types,
     *    but their erased types are the same.
     */
    private def checkNoDoubleDefs(root: Symbol) {
      checkNoDeclaredDoubleDefs(root)
      object opc extends Cursor(root) {
        // specialized members have no type history before 'specialize', causing double def errors for curried defs
        override def exclude(sym: Symbol): Boolean = (
             sym.isType
          || super.exclude(sym)
          || !sym.hasTypeAt(currentRun.refchecksPhase.id)
        )
        override def matches(lo: Symbol, high: Symbol) = !high.isPrivate
      }
      def isErasureDoubleDef(pair: SymbolPair) = {
        import pair._
        log(s"Considering for erasure clash:\n$pair")
        !exitingRefchecks(lowType matches highType) && sameTypeAfterErasure(low, high)
      }
      opc.iterator filter isErasureDoubleDef foreach doubleDefError
    }

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
      new ComputeBridges(unit, owner) compute()
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

      private def preEraseNormalApply(tree: Apply) = {
        val fn = tree.fun
        val args = tree.args

        def qualifier = fn match {
          case Select(qual, _) => qual
          case TypeApply(Select(qual, _), _) => qual
        }
        def preEraseAsInstanceOf = {
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
        }

        def preEraseIsInstanceOf = {
          fn match {
            case TypeApply(sel @ Select(qual, name), List(targ)) =>
              if (qual.tpe != null && isPrimitiveValueClass(qual.tpe.typeSymbol) && targ.tpe != null && targ.tpe <:< AnyRefTpe)
                reporter.error(sel.pos, "isInstanceOf cannot test if value types are references.")

              def mkIsInstanceOf(q: () => Tree)(tp: Type): Tree =
                Apply(
                  TypeApply(
                    Select(q(), Object_isInstanceOf) setPos sel.pos,
                    List(TypeTree(tp) setPos targ.pos)) setPos fn.pos,
                  List()) setPos tree.pos
              targ.tpe match {
                case SingleType(_, _) | ThisType(_) | SuperType(_, _) =>
                  val cmpOp = if (targ.tpe <:< AnyValTpe) Any_equals else Object_eq
                  atPos(tree.pos) {
                    Apply(Select(qual, cmpOp), List(gen.mkAttributedQualifier(targ.tpe)))
                  }
                case RefinedType(parents, decls) if (parents.length >= 2) =>
                  gen.evalOnce(qual, currentOwner, unit) { q =>
                    // Optimization: don't generate isInstanceOf tests if the static type
                    // conforms, because it always succeeds.  (Or at least it had better.)
                    // At this writing the pattern matcher generates some instance tests
                    // involving intersections where at least one parent is statically known true.
                    // That needs fixing, but filtering the parents here adds an additional
                    // level of robustness (in addition to the short term fix.)
                    val parentTests = parents filterNot (qual.tpe <:< _)

                    if (parentTests.isEmpty) Literal(Constant(true))
                    else atPos(tree.pos) {
                      parentTests map mkIsInstanceOf(q) reduceRight gen.mkAnd
                    }
                  }
                case _ =>
                  tree
              }
            case _ => tree
          }
        }

        if (fn.symbol == Any_asInstanceOf) {
          preEraseAsInstanceOf
        } else if (fn.symbol == Any_isInstanceOf) {
          preEraseIsInstanceOf
        } else if (fn.symbol.isOnlyRefinementMember) {
          // !!! Another spot where we produce overloaded types (see test pos/t6301)
          log(s"${fn.symbol.fullLocationString} originates in refinement class - call will be implemented via reflection.")
          ApplyDynamic(qualifier, args) setSymbol fn.symbol setPos tree.pos
        } else if (fn.symbol.isMethodWithExtension && !fn.symbol.tpe.isErroneous) {
          Apply(gen.mkAttributedRef(extensionMethods.extensionMethod(fn.symbol)), qualifier :: args)
        } else {
          tree
        }
      }

      private def preEraseApply(tree: Apply) = {
        tree.fun match {
          case TypeApply(fun @ Select(qual, name), args @ List(arg))
          if ((fun.symbol == Any_isInstanceOf || fun.symbol == Object_isInstanceOf) &&
              unboundedGenericArrayLevel(arg.tpe) > 0) => // !!! todo: simplify by having GenericArray also extract trees
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
          case fn @ Select(qual, name) =>
            val args = tree.args
            if (fn.symbol.owner == ArrayClass) {
              // Have to also catch calls to abstract types which are bounded by Array.
              if (unboundedGenericArrayLevel(qual.tpe.widen) == 1 || qual.tpe.typeSymbol.isAbstractType) {
                // convert calls to apply/update/length on generic arrays to
                // calls of ScalaRunTime.array_xxx method calls
                global.typer.typedPos(tree.pos) {
                  val arrayMethodName = name match {
                    case nme.apply  => nme.array_apply
                    case nme.length => nme.array_length
                    case nme.update => nme.array_update
                    case nme.clone_ => nme.array_clone
                    case _          => reporter.error(tree.pos, "Unexpected array member, no translation exists.") ; nme.NO_NAME
                  }
                  gen.mkRuntimeCall(arrayMethodName, qual :: args)
                }
              } else {
                // store exact array erasure in map to be retrieved later when we might
                // need to do the cast in adaptMember
                // Note: No specialErasure needed here because we simply cast, on
                // elimination of SelectFromArray, no boxing or unboxing is done there.
                treeCopy.Apply(
                  tree,
                  SelectFromArray(qual, name, erasure(tree.symbol)(qual.tpe)).copyAttrs(fn),
                  args)
              }
            }
            else if (args.isEmpty && interceptedMethods(fn.symbol)) {
              if (poundPoundMethods.contains(fn.symbol)) {
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
                    // Since we are past typer, we need to avoid creating trees carrying
                    // overloaded types.  This logic is custom (and technically incomplete,
                    // although serviceable) for def hash.  What is really needed is for
                    // the overloading logic presently hidden away in a few different
                    // places to be properly exposed so we can just call "resolveOverload"
                    // after typer.  Until then:
                    val alts    = ScalaRunTimeModule.info.member(nme.hash_).alternatives
                    def alt1    = alts find (_.info.paramTypes.head =:= qual.tpe)
                    def alt2    = ScalaRunTimeModule.info.member(nme.hash_) suchThat (_.info.paramTypes.head.typeSymbol == AnyClass)
                    val newTree = gen.mkRuntimeCall(nme.hash_, qual :: Nil) setSymbol (alt1 getOrElse alt2)

                    global.typer.typed(newTree)
                }
              } else if (isPrimitiveValueClass(qual.tpe.typeSymbol)) {
                // Rewrite 5.getClass to ScalaRunTime.anyValClass(5)
                global.typer.typed(gen.mkRuntimeCall(nme.anyValClass, List(qual, typer.resolveClassTag(tree.pos, qual.tpe.widen))))
              } else if (primitiveGetClassMethods.contains(fn.symbol)) {
                // if we got here then we're trying to send a primitive getClass method to either
                // a) an Any, in which cage Object_getClass works because Any erases to object. Or
                //
                // b) a non-primitive, e.g. because the qualifier's type is a refinement type where one parent
                //    of the refinement is a primitive and another is AnyRef. In that case
                //    we get a primitive form of _getClass trying to target a boxed value
                //    so we need replace that method name with Object_getClass to get correct behavior.
                //    See SI-5568.
                tree setSymbol Object_getClass
              } else {
                devWarning(s"The symbol '${fn.symbol}' was interecepted but didn't match any cases, that means the intercepted methods set doesn't match the code")
                tree
              }
            } else qual match {
              case New(tpt) if name == nme.CONSTRUCTOR && tpt.tpe.typeSymbol.isDerivedValueClass =>
                // println("inject derived: "+arg+" "+tpt.tpe)
                val List(arg) = args
                val attachment = new TypeRefAttachment(tree.tpe.asInstanceOf[TypeRef])
                InjectDerivedValue(arg) updateAttachment attachment
              case _ =>
                preEraseNormalApply(tree)
            }

          case _ =>
            preEraseNormalApply(tree)
        }
      }

      def preErase(tree: Tree): Tree = tree match {
        case tree: Apply =>
          preEraseApply(tree)

        case TypeApply(fun, args) if (fun.symbol.owner != AnyClass &&
                                      fun.symbol != Object_asInstanceOf &&
                                      fun.symbol != Object_isInstanceOf) =>
          // leave all other type tests/type casts, remove all other type applications
          preErase(fun)

        case Select(qual, name) =>
          val sym = tree.symbol
          val owner = sym.owner
          if (owner.isRefinementClass) {
            sym.allOverriddenSymbols filterNot (_.owner.isRefinementClass) match {
              case overridden :: _ =>
                log(s"${sym.fullLocationString} originates in refinement class - replacing with ${overridden.fullLocationString}.")
                tree.symbol = overridden
              case Nil =>
                // Ideally this should not be reached or reachable; anything which would
                // get here should have been caught in the surrounding Apply.
                devWarning(s"Failed to rewrite reflective apply - now don't know what to do with " + tree)
                return treeCopy.Select(tree, gen.mkAttributedCast(qual, qual.tpe.widen), name)
            }
          }

          def isJvmAccessible(sym: Symbol) = (sym.isClass && !sym.isJavaDefined) || localTyper.context.isAccessible(sym, sym.owner.thisType)
          if (!isJvmAccessible(owner) && qual.tpe != null) {
            qual match {
              case Super(_, _) =>
                // Insert a cast here at your peril -- see SI-5162.
                reporter.error(tree.pos, s"Unable to access ${tree.symbol.fullLocationString} with a super reference.")
                tree
              case _ =>
                // Todo: Figure out how qual.tpe could be null in the check above (it does appear in build where SwingWorker.this
                // has a null type).
                val qualSym = qual.tpe.widen.typeSymbol
                if (isJvmAccessible(qualSym) && !qualSym.isPackageClass && !qualSym.isPackageObjectClass) {
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
          treeCopy.Template(tree, parents, noSelfType, addBridges(body, currentOwner))

        case Match(selector, cases) =>
          Match(Typed(selector, TypeTree(selector.tpe)), cases)

        case Literal(ct) if ct.tag == ClazzTag
                         && ct.typeValue.typeSymbol != definitions.UnitClass =>
          val erased = ct.typeValue match {
            case tr @ TypeRef(_, clazz, _) if clazz.isDerivedValueClass => scalaErasure.eraseNormalClassRef(tr)
            case tpe => specialScalaErasure(tpe)
          }
          treeCopy.Literal(tree, Constant(erased))

        case ClassDef(_,_,_,_) =>
          debuglog("defs of " + tree.symbol + " = " + tree.symbol.info.decls)
          copyClassDef(tree)(tparams = Nil)
        case DefDef(_,_,_,_,_,_) =>
          copyDefDef(tree)(tparams = Nil)
        case TypeDef(_, _, _, _) =>
          EmptyTree

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
                tree1, elemtpt setType specialScalaErasure.applyInArray(elemtpt.tpe), trees map transform).clearType()
            case DefDef(_, _, _, _, tpt, _) =>
              try super.transform(tree1).clearType()
              finally tpt setType specialErasure(tree1.symbol)(tree1.symbol.tpe).resultType
            case _ =>
              super.transform(tree1).clearType()
          }
        }
      }
    }

    /** The main transform function: Pretransform the tree, and then
     *  re-type it at phase erasure.next.
     */
    override def transform(tree: Tree): Tree = {
      val tree1 = preTransformer.transform(tree)
      // log("tree after pretransform: "+tree1)
      exitingErasure {
        val tree2 = mixinTransformer.transform(tree1)
        // debuglog("tree after addinterfaces: \n" + tree2)

        newTyper(rootContextPostTyper(unit, tree)).typed(tree2)
      }
    }
  }

  final def resolveAnonymousBridgeClash(sym: Symbol, bridge: Symbol) {
    // TODO reinstate this after Delambdafy generates anonymous classes that meet this requirement.
    // require(sym.owner.isAnonymousClass, sym.owner)
    log(s"Expanding name of ${sym.debugLocationString} as it clashes with bridge. Renaming deemed safe because the owner is anonymous.")
    sym.expandName(sym.owner)
    bridge.resetFlag(BRIDGE)
  }

  private class TypeRefAttachment(val tpe: TypeRef)
}
