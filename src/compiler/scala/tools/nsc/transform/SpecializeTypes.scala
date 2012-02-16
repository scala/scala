/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Iulian Dragos
 */

package scala.tools.nsc
package transform

import scala.tools.nsc.symtab.Flags
import scala.collection.{ mutable, immutable }

/** Specialize code on types.
 *
 *  Make sure you've read the thesis:
 *
 *    Iulian Dragos: Compiling Scala for Performance (chapter 4)
 *
 *  There are some things worth noting, (possibly) not mentioned there:
 *  0) Make sure you understand the meaning of various `SpecializedInfo` descriptors
 *     defined below.
 *
 *  1) Specializing traits by introducing bridges in specialized methods
 *     of the specialized trait may introduce problems during mixin composition.
 *     Concretely, it may cause cyclic calls and result in a stack overflow.
 *     See ticket #4351.
 *     This was solved by introducing an `Abstract` specialized info descriptor.
 *     Instead of generating a bridge in the trait, an abstract method is generated.
 *
 *  2) Specialized private members sometimes have to be switched to protected.
 *     In some cases, even this is not enough. Example:
 *
 *     {{{
 *       class A[@specialized T](protected val d: T) {
 *         def foo(that: A[T]) = that.d
 *       }
 *     }}}
 *
 *     Specialization will generate a specialized class and a specialized method:
 *
 *     {{{
 *       class A$mcI$sp(protected val d: Int) extends A[Int] {
 *         def foo(that: A[Int]) = foo$mcI$sp(that)
 *         def foo(that: A[Int]) = that.d
 *       }
 *     }}}
 *
 *     Above, `A$mcI$sp` cannot access `d`, so the method cannot be typechecked.
 */
abstract class SpecializeTypes extends InfoTransform with TypingTransformers {
  import global._
  import Flags._
  /** the name of the phase: */
  val phaseName: String = "specialize"

  /** The following flags may be set by this phase: */
  override def phaseNewFlags: Long = notPRIVATE | lateFINAL

  /** This phase changes base classes. */
  override def changesBaseClasses = true
  override def keepsTypeParams = true

  type TypeEnv = immutable.Map[Symbol, Type]
  def emptyEnv: TypeEnv = Map[Symbol, Type]()

  private implicit val typeOrdering: Ordering[Type] = Ordering[String] on ("" + _.typeSymbol.name)

  import definitions.{
    RootClass, BooleanClass, UnitClass, ArrayClass,
    ScalaValueClasses, isValueClass, isScalaValueType,
    SpecializedClass, RepeatedParamClass, JavaRepeatedParamClass,
    AnyRefClass, ObjectClass, AnyRefModule,
    GroupOfSpecializable, uncheckedVarianceClass, ScalaInlineClass
  }

  /** TODO - this is a lot of maps.
   */

  /** For a given class and concrete type arguments, give its specialized class */
  val specializedClass: mutable.Map[(Symbol, TypeEnv), Symbol] = new mutable.LinkedHashMap

  /** Map a method symbol to a list of its specialized overloads in the same class. */
  private val overloads: mutable.Map[Symbol, List[Overload]] = mutable.HashMap[Symbol, List[Overload]]() withDefaultValue Nil

  /** Map a symbol to additional information on specialization. */
  private val info: mutable.Map[Symbol, SpecializedInfo] = perRunCaches.newMap[Symbol, SpecializedInfo]()

  /** Map class symbols to the type environments where they were created. */
  private val typeEnv = mutable.HashMap[Symbol, TypeEnv]() withDefaultValue emptyEnv

  // holds mappings from regular type parameter symbols to symbols of
  // specialized type parameters which are subtypes of AnyRef
  private val anyrefSpecCache = perRunCaches.newMap[Symbol, Symbol]()

  // holds mappings from members to the type variables in the class
  // that they were already specialized for, so that they don't get
  // specialized twice (this is for AnyRef specializations)
  private val wasSpecializedForTypeVars = perRunCaches.newMap[Symbol, Set[Symbol]]() withDefaultValue Set()

  /** Concrete methods that use a specialized type, or override such methods. */
  private val concreteSpecMethods = new mutable.HashSet[Symbol]()

  private def isSpecialized(sym: Symbol)          = sym hasAnnotation SpecializedClass
  private def hasSpecializedFlag(sym: Symbol)     = sym hasFlag SPECIALIZED
  private def specializedTypes(tps: List[Symbol]) = tps filter isSpecialized
  private def specializedOn(sym: Symbol): List[Symbol] = {
    sym getAnnotation SpecializedClass match {
      case Some(ann @ AnnotationInfo(_, args, _)) =>
        args map (_.tpe) flatMap { tp =>
          tp baseType GroupOfSpecializable match {
            case TypeRef(_, GroupOfSpecializable, arg :: Nil) =>
              arg.typeArgs map (_.typeSymbol)
            case _ =>
              List(tp.typeSymbol)
          }
        }
      case _ => Nil
    }
  }

  // If we replace `isBoundedGeneric` with (tp <:< AnyRefClass.tpe),
  // then pos/spec-List.scala fails - why? Does this kind of check fail
  // for similar reasons? Does `sym.isAbstractType` make a difference?
  private def isSpecializedAnyRefSubtype(tp: Type, sym: Symbol) = (
       (specializedOn(sym) contains AnyRefModule)
    && !isValueClass(tp.typeSymbol)
    && isBoundedGeneric(tp)
  )
  private def isBoundedGeneric(tp: Type) = tp match {
    case TypeRef(_, sym, _) if sym.isAbstractType => (tp <:< AnyRefClass.tpe)
    case TypeRef(_, sym, _)                       => !isValueClass(sym)
    case _                                        => false
  }

  object TypeEnv {
    /** Return a new type environment binding specialized type parameters of sym to
     *  the given args. Expects the lists to have the same length.
     */
    def fromSpecialization(sym: Symbol, args: List[Type]): TypeEnv = {
      ifDebug(assert(sym.info.typeParams.length == args.length, sym + " args: " + args))

      emptyEnv ++ collectMap2(sym.info.typeParams, args)((k, v) => isSpecialized(k))
    }

    /** Does typeenv `t1` include `t2`? All type variables in `t1`
     *  are defined in `t2` and:
     *  - are bound to the same type, or
     *  - are an AnyRef specialization and `t2` is bound to a subtype of AnyRef
     */
    def includes(t1: TypeEnv, t2: TypeEnv) = t1 forall {
      case (sym, tpe) =>
        t2 get sym exists { t2tp =>
          (tpe == t2tp) || !(isScalaValueType(tpe) || isScalaValueType(t2tp)) // u.t.b. (t2tp <:< AnyRefClass.tpe)
        }
    }

    /** Reduce the given environment to contain mappings only for type variables in tps. */
    def restrict(env: TypeEnv, tps: immutable.Set[Symbol]): TypeEnv =
      env filterKeys tps toMap

    /** Is the given environment a valid specialization for sym?
     *  It is valid if each binding is from a @specialized type parameter in sym (or its owner)
     *  to a type for which `sym` is specialized.
     */
    def isValid(env: TypeEnv, sym: Symbol): Boolean = {
      env forall { case (tvar, tpe) =>
        isSpecialized(tvar) && (concreteTypes(tvar) contains tpe) && {
          (sym.typeParams contains tvar) ||
          (sym.owner != RootClass && (sym.owner.typeParams contains tvar))
        }
      }
    }
  }

  /** Returns the generic class that was specialized to 'sClass', or
   *  'sClass' itself if sClass is not a specialized subclass.
   */
  def genericClass(sClass: Symbol): Symbol =
    if (hasSpecializedFlag(sClass)) sClass.superClass
    else sClass

  case class Overload(sym: Symbol, env: TypeEnv) {
    override def toString = "specialized overload " + sym + " in " + env
  }

  /** Just to mark uncheckable */
  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new SpecializationPhase(prev)
  class SpecializationPhase(prev: scala.tools.nsc.Phase) extends super.Phase(prev) {
    override def checkable = false
  }

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new SpecializationTransformer(unit)

  abstract class SpecializedInfo {
    def target: Symbol

    /** Are type bounds of @specialized type parameters of 'target' now in 'env'? */
    def typeBoundsIn(env: TypeEnv) = false

    /** A degenerated method has @specialized type parameters that appear only in
     *  type bounds of other @specialized type parameters (and not in its result type).
     */
    def degenerate = false

    def isAccessor = false
  }

  /** Symbol is a special overloaded method of 'original', in the environment env. */
  case class SpecialOverload(original: Symbol, env: TypeEnv) extends SpecializedInfo {
    def target = original
  }

  /** Symbol is a method that should be forwarded to 't' */
  case class Forward(t: Symbol) extends SpecializedInfo {
    def target = t
  }

  /** Symbol is a specialized abstract method, either specialized or original. The original `t` is abstract. */
  case class Abstract(t: Symbol) extends SpecializedInfo {
    def target = t
  }

  /** Symbol is a specialized accessor for the `target` field. */
  case class SpecializedAccessor(target: Symbol) extends SpecializedInfo {
    override def isAccessor = true
  }

  /** Symbol is a specialized method whose body should be the target's method body. */
  case class Implementation(target: Symbol) extends SpecializedInfo

  /** Symbol is a specialized override paired with `target`. */
  case class SpecialOverride(target: Symbol) extends SpecializedInfo

  /** A specialized inner class that specializes original inner class `target` on a type parameter of the enclosing class, in the typeenv `env`. */
  case class SpecializedInnerClass(target: Symbol, env: TypeEnv) extends SpecializedInfo

  /** Symbol is a normalized member obtained by specializing 'target'. */
  case class NormalizedMember(target: Symbol) extends SpecializedInfo {

    /** Type bounds of a @specialized type var are now in the environment. */
    override def typeBoundsIn(env: TypeEnv): Boolean = {
      target.info.typeParams exists { tvar =>
        isSpecialized(tvar) && (specializedTypeVars(tvar.info.bounds) exists env.isDefinedAt)
      }
    }

    override lazy val degenerate = {
      val stvTypeParams = specializedTypeVars(target.info.typeParams map (_.info))
      val stvResult     = specializedTypeVars(target.info.resultType)

      log("degenerate: " + target + " stv tparams: " + stvTypeParams + " stv info: " + stvResult)

      (stvTypeParams -- stvResult).nonEmpty
    }
  }

  /** Has `clazz` any type parameters that need be specialized? */
  def hasSpecializedParams(clazz: Symbol) =
    clazz.info.typeParams exists isSpecialized

  /** Return specialized type parameters. */
  def specializedParams(sym: Symbol): List[Symbol] =
    sym.info.typeParams filter isSpecialized

  def splitParams(tps: List[Symbol]) =
    tps partition isSpecialized

  /** Given an original class symbol and a list of types its type parameters are instantiated at
   *  returns a list of type parameters that should remain in the TypeRef when instantiating a
   *  specialized type.
   */
  def survivingArgs(sym: Symbol, args: List[Type]): List[Type] =
    for ((tvar, tpe) <- sym.info.typeParams.zip(args) if !isSpecialized(tvar) || !isScalaValueType(tpe))
      yield tpe

  val specializedType = new TypeMap {
    override def apply(tp: Type): Type = tp match {
      case TypeRef(pre, sym, args) if args.nonEmpty =>
        val pre1 = this(pre)
        // when searching for a specialized class, take care to map all
        // type parameters that are subtypes of AnyRef to AnyRef
        val args1 = map2(args, sym.typeParams) {
          case (tp, orig) if isSpecializedAnyRefSubtype(tp, orig) => AnyRefClass.tpe
          case (tp, _)                                            => tp
        }
        specializedClass.get((sym, TypeEnv.fromSpecialization(sym, args1))) match {
          case Some(sym1) => typeRef(pre1, sym1, survivingArgs(sym, args))
          case None       => typeRef(pre1, sym, args)
        }
      case _ => tp
    }
  }

  /** Return the specialized overload of sym in the given env, if any. */
  def overload(sym: Symbol, env: TypeEnv) =
    overloads(sym).find(ov => TypeEnv.includes(ov.env, env))

  /** Return the specialized name of 'sym' in the given environment. It
   *  guarantees the same result regardless of the map order by sorting
   *  type variables alphabetically.
   */
  private def specializedName(sym: Symbol, env: TypeEnv): TermName = {
    val tvars = (
      if (sym.isClass) env.keySet
      else specializedTypeVars(sym).intersect(env.keySet)
    )
    val (methparams, others) = tvars.toList sortBy ("" + _.name) partition (_.owner.isMethod)
    debuglog("specName(" + sym + ") env: " + env + " tvars: " + tvars)

    specializedName(sym.name, methparams map env, others map env)
  }

  /** Specialize name for the two list of types. The first one denotes
   *  specialization on method type parameters, the second on outer environment.
   */
  private def specializedName(name: Name, types1: List[Type], types2: List[Type]): TermName = {
    if (nme.INITIALIZER == name || (types1.isEmpty && types2.isEmpty))
      name
    else if (nme.isSetterName(name))
      nme.getterToSetter(specializedName(nme.setterToGetter(name), types1, types2))
    else if (nme.isLocalName(name))
      nme.getterToLocal(specializedName(nme.localToGetter(name), types1, types2))
    else {
      val (base, cs, ms) = nme.splitSpecializedName(name)
      val abbrevs = definitions.abbrvTag withDefaultValue definitions.abbrvTag(ObjectClass)
      newTermName(base.toString + "$"
                  + "m" + ms + types1.map(t => abbrevs(t.typeSymbol)).mkString("", "", "")
                  + "c" + cs + types2.map(t => abbrevs(t.typeSymbol)).mkString("", "", "$sp"))
    }
  }

  lazy val specializableTypes = (ScalaValueClasses :+ AnyRefClass) map (_.tpe) sorted

  /** If the symbol is the companion of a value class, the value class.
   *  Otherwise, AnyRef.
   */
  def specializesClass(sym: Symbol): Symbol = {
    val c = sym.companionClass
    if (isValueClass(c)) c else AnyRefClass
  }

  /** Return the types `sym` should be specialized at. This may be some of the primitive types
   *  or AnyRef. AnyRef means that a new type parameter T will be generated later, known to be a
   *  subtype of AnyRef (T <: AnyRef).
   *  These are in a meaningful order for stability purposes.
   */
  def concreteTypes(sym: Symbol): List[Type] = {
    val types = (
      if (!isSpecialized(sym)) Nil      // no @specialized Annotation
      else specializedOn(sym) match {
        case Nil  => specializableTypes                             // specialized on everything
        case args => args map (s => specializesClass(s).tpe) sorted // specialized on args
      }
    )
    if (isBoundedGeneric(sym.tpe) && (types contains AnyRefClass))
      reporter.warning(sym.pos, sym + " is always a subtype of " + AnyRefClass.tpe + ".")

    types
  }

  /** Return a list of all type environments for all specializations
   *  of @specialized types in `tps`.
   */
  private def specializations(tps: List[Symbol]): List[TypeEnv] = {
    // the keys in each TypeEnv
    val keys: List[Symbol] = tps filter isSpecialized
    // creating each permutation of concrete types
    def loop(ctypes: List[List[Type]]): List[List[Type]] = ctypes match {
      case Nil         => Nil
      case set :: Nil  => set map (x => List(x))
      case set :: sets => for (x <- set ; xs <- loop(sets)) yield x :: xs
    }
    // zip the keys with each permutation to create a TypeEnv
    loop(keys map concreteTypes) map (xss => Map(keys zip xss: _*))
  }

  /** Does the given 'sym' need to be specialized in the environment 'env'?
   *  Specialization is needed for
   *    - members with specialized type parameters found in the given environment
   *    - constructors of specialized classes
   *    - normalized members whose type bounds appear in the environment
   */
  private def needsSpecialization(env: TypeEnv, sym: Symbol): Boolean = {
    specializedTypeVars(sym).intersect(env.keySet).diff(wasSpecializedForTypeVars(sym)).nonEmpty ||
     (sym.isClassConstructor && (sym.enclClass.typeParams exists isSpecialized)) ||
     (isNormalizedMember(sym) && info(sym).typeBoundsIn(env))
  }

  def isNormalizedMember(m: Symbol) = hasSpecializedFlag(m) && (info get m exists {
    case NormalizedMember(_)  => true
    case _                    => false
  })
  def specializedTypeVars(tpes: List[Type]): immutable.Set[Symbol] = {
    val buf = Set.newBuilder[Symbol]
    tpes foreach (tp => buf ++= specializedTypeVars(tp))
    buf.result
  }
  def specializedTypeVars(sym: Symbol): immutable.Set[Symbol] =
    atPhase(currentRun.typerPhase)(specializedTypeVars(sym.info))

  /** Return the set of @specialized type variables mentioned by the given type.
   *  It only counts type variables that appear:
   *    - naked
   *    - as arguments to type constructors in @specialized positions
   *      (arrays are considered as Array[@specialized T])
   */
  def specializedTypeVars(tpe: Type): immutable.Set[Symbol] = tpe match {
    case TypeRef(pre, sym, args) =>
      if (sym.isAliasType)
        specializedTypeVars(tpe.normalize)
      else if (sym.isTypeParameter && isSpecialized(sym) || (sym.isTypeSkolem && isSpecialized(sym.deSkolemize)))
        Set(sym)
      else if (sym == ArrayClass)
        specializedTypeVars(args)
      else if (args.isEmpty)
        Set()
      else
        specializedTypeVars(sym.typeParams zip args collect { case (tp, arg) if isSpecialized(tp) => arg })

    case PolyType(tparams, resTpe)   => specializedTypeVars(resTpe :: tparams.map(_.info))
    // since this method may be run at phase typer (before uncurry, where NMTs are eliminated)
    case NullaryMethodType(resTpe)   => specializedTypeVars(resTpe)
    case MethodType(argSyms, resTpe) => specializedTypeVars(resTpe :: argSyms.map(_.tpe))
    case ExistentialType(_, res)     => specializedTypeVars(res)
    case AnnotatedType(_, tp, _)     => specializedTypeVars(tp)
    case TypeBounds(lo, hi)          => specializedTypeVars(List(lo, hi))
    case RefinedType(parents, _)     => parents flatMap specializedTypeVars toSet
    case _                           => Set()
  }

  /** Returns the type parameter in the specialized class `clazz` that corresponds to type parameter
   *  `sym` in the original class. It will create it if needed or use the one from the cache.
   */
  private def typeParamSubAnyRef(sym: Symbol, clazz: Symbol) = (
    anyrefSpecCache.getOrElseUpdate(sym,
      clazz.newTypeParameter(sym.name append nme.SPECIALIZED_SUFFIX_NAME toTypeName, sym.pos)
        setInfo TypeBounds(sym.info.bounds.lo, AnyRefClass.tpe)
    ).tpe
  )

  /** Cleans the anyrefSpecCache of all type parameter symbols of a class.
   */
  private def cleanAnyRefSpecCache(clazz: Symbol, decls: List[Symbol]) = (
    // remove class type parameters and those of normalized members.
    clazz :: decls foreach {
      _.tpe match {
        case PolyType(tparams, _) => anyrefSpecCache --= tparams
        case _                    => ()
      }
    }
  )

  /** Type parameters that survive when specializing in the specified environment. */
  def survivingParams(params: List[Symbol], env: TypeEnv) =
    params.filter(p => !isSpecialized(p) || !isScalaValueType(env(p)))

  /** Produces the symbols from type parameters `syms` of the original owner,
   *  in the given type environment `env`. The new owner is `nowner`.
   *
   *  Non-specialized type parameters are cloned into new ones.
   *  Type parameters specialized on AnyRef have preexisting symbols.
   *
   *  For instance, a @specialized(AnyRef) T, will become T$sp <: AnyRef.
   */
  def produceTypeParameters(syms: List[Symbol], nowner: Symbol, env: TypeEnv) = {
    val cloned = for (s <- syms) yield if (!env.contains(s)) s.cloneSymbol(nowner) else env(s).typeSymbol
    // log("producing type params: " + cloned.map(t => (t, t.tpe.bounds.hi)))
    foreach2(syms, cloned) { (orig, cln) =>
      cln.removeAnnotation(SpecializedClass)
      if (env.contains(orig))
        cln modifyInfo (info => TypeBounds(info.bounds.lo, AnyRefClass.tpe))
    }
    cloned map (_ substInfo (syms, cloned))
  }

  /** Maps AnyRef bindings from a raw environment (holding AnyRefs) into type parameters from
   *  the specialized symbol (class (specialization) or member (normalization)), leaves everything else as-is.
   */
  private def mapAnyRefsInSpecSym(env: TypeEnv, origsym: Symbol, specsym: Symbol): TypeEnv = env map {
    case (sym, tp) if tp == AnyRefClass.tpe && sym.owner == origsym => (sym, typeParamSubAnyRef(sym, specsym))
    case x => x
  }

  /** Maps AnyRef bindings from a raw environment (holding AnyRefs) into type parameters from
   *  the original class, leaves everything else as-is.
   */
  private def mapAnyRefsInOrigCls(env: TypeEnv, origcls: Symbol): TypeEnv = env map {
    case (sym, tp) if (tp == AnyRefClass.tpe) && sym.owner == origcls => (sym, sym.tpe)
    case x => x
  }

  /** Specialize 'clazz', in the environment `outerEnv`. The outer
   *  environment contains bindings for specialized types of enclosing
   *  classes.
   *
   *  A class C is specialized w.r.t to its own specialized type params
   *  `stps`, by specializing its members, and creating a new class for
   *  each combination of `stps`.
   */
  def specializeClass(clazz: Symbol, outerEnv: TypeEnv): List[Symbol] = {
    def specializedClass(env0: TypeEnv, normMembers: List[Symbol]): Symbol = {
      /** It gets hard to follow all the clazz and cls, and specializedClass
       *  was both already used for a map and mucho long.  So "sClass" is the
       *  specialized subclass of "clazz" throughout this file.
       */
      val sClass = clazz.owner.newClass(specializedName(clazz, env0).toTypeName, clazz.pos, (clazz.flags | SPECIALIZED) & ~CASE)

      def cloneInSpecializedClass(member: Symbol, flagFn: Long => Long) =
        member.cloneSymbol(sClass, flagFn(member.flags | SPECIALIZED))
      
      sClass.sourceFile = clazz.sourceFile
      currentRun.symSource(sClass) = clazz.sourceFile // needed later on by mixin

      val env = mapAnyRefsInSpecSym(env0, clazz, sClass)
      typeEnv(sClass) = env
      this.specializedClass((clazz, env0)) = sClass

      val decls1                        = newScope  // declarations of the newly specialized class 'sClass'
      var oldClassTParams: List[Symbol] = Nil       // original unspecialized type parameters
      var newClassTParams: List[Symbol] = Nil       // unspecialized type parameters of 'specializedClass' (cloned)

      // has to be a val in order to be computed early. It is later called
      // within 'atPhase(next)', which would lead to an infinite cycle otherwise
      val specializedInfoType: Type = {
        oldClassTParams = survivingParams(clazz.info.typeParams, env)
        newClassTParams = produceTypeParameters(oldClassTParams, sClass, env) map subst(env)
        // log("new tparams " + newClassTParams.zip(newClassTParams map {s => (s.tpe, s.tpe.bounds.hi)}) + ", in env: " + env)

        def applyContext(tpe: Type) =
          subst(env, tpe).instantiateTypeParams(oldClassTParams, newClassTParams map (_.tpe))

        /** Return a list of specialized parents to be re-mixed in a specialized subclass.
         *  Assuming env = [T -> Int] and
         *    class Integral[@specialized T] extends Numeric[T]
         *  and Numeric[U] is specialized on U, this produces List(Numeric$mcI).
         *
         *  so that class Integral$mci extends Integral[Int] with Numeric$mcI.
         */
        def specializedParents(parents: List[Type]): List[Type] = {
          var res: List[Type] = Nil
          // log(specializedClass + ": seeking specialized parents of class with parents: " + parents.map(_.typeSymbol))
          for (p <- parents) {
            val stp = atPhase(phase.next)(specializedType(p))
            if (stp != p)
              if (p.typeSymbol.isTrait) res ::= stp
              else if (currentRun.compiles(clazz))
                reporter.warning(clazz.pos, p.typeSymbol + " must be a trait. Specialized version of "
                  + clazz + " will inherit generic " + p)  // TODO change to error
          }
          res
        }

        var parents = List(applyContext(atPhase(currentRun.typerPhase)(clazz.tpe)))
        // log("!!! Parents: " + parents + ", sym: " + parents.map(_.typeSymbol))
        if (parents.head.typeSymbol.isTrait)
          parents = parents.head.parents.head :: parents
        val extraSpecializedMixins = specializedParents(clazz.info.parents map applyContext)
        if (extraSpecializedMixins.nonEmpty)
          debuglog("specializeClass on " + clazz + " founds extra specialized mixins: " + extraSpecializedMixins.mkString(", "))
        // If the class being specialized has a self-type, the self type may
        // require specialization.  First exclude classes whose self types have
        // the same type constructor as the class itself, since they will
        // already be covered.  Then apply the current context to the self-type
        // as with the parents and assign it to typeOfThis.
        if (clazz.typeOfThis.typeConstructor ne clazz.typeConstructor) {
          sClass.typeOfThis = applyContext(clazz.typeOfThis)
          log("Rewriting self-type for specialized class:\n" +
              "    " +  clazz.defStringSeenAs(clazz.typeOfThis) + "\n" +
              " => " + sClass.defStringSeenAs(sClass.typeOfThis)
          )
        }
        polyType(newClassTParams, ClassInfoType(parents ::: extraSpecializedMixins, decls1, sClass))
      }

      atPhase(phase.next)(sClass setInfo specializedInfoType)
      val fullEnv = outerEnv ++ env

      /** Enter 'sym' in the scope of the current specialized class. It's type is
       *  mapped through the active environment, binding type variables to concrete
       *  types. The existing typeEnv for `sym` is composed with the current active
       *  environment
       */
      def enterMember(sym: Symbol): Symbol = {
        typeEnv(sym) = fullEnv ++ typeEnv(sym) // append the full environment
        sym modifyInfo (_.substThis(clazz, sClass).instantiateTypeParams(oldClassTParams, newClassTParams map (_.tpe)))
        // we remove any default parameters. At this point, they have been all
        // resolved by the type checker. Later on, erasure re-typechecks everything and
        // chokes if it finds default parameters for specialized members, even though
        // they are never needed.
        mapParamss(sym)(_ resetFlag DEFAULTPARAM)
        decls1 enter subst(fullEnv)(sym)
      }

      /** Create and enter in scope an overridden symbol m1 for `m` that forwards
       *  to `om`. `om` is a fresh, special overload of m1 that is an implementation
       *  of `m`. For example, for a
       *
       *  class Foo[@specialized A] {
       *    def m(x: A) = <body> // m
       *  }
       *  , for class Foo$I extends Foo[Int], this method enters two new symbols in
       *  the scope of Foo$I:
       *
       *    def m(x: Int) = m$I(x) // m1
       *    def m$I(x: Int) = <body>/adapted to env {A -> Int} // om
       */
      def forwardToOverload(m: Symbol): Symbol = {
        val specMember = enterMember(cloneInSpecializedClass(m, f => (f | OVERRIDE) & ~(DEFERRED | CASEACCESSOR)))
        val om         = specializedOverload(sClass, m, env).setFlag(OVERRIDE)
        val original = info.get(m) match {
          case Some(NormalizedMember(tg)) => tg
          case _                          => m
        }
        info(specMember) = Forward(om)
        info(om)         = if (original.isDeferred) Forward(original) else Implementation(original)
        typeEnv(om)      = env ++ typeEnv(m) // add the environment for any method tparams

        overloads(specMember) ::= Overload(om, typeEnv(om))
        enterMember(om)
      }

      for (m <- normMembers ; if needsSpecialization(outerEnv ++ env, m) && satisfiable(fullEnv)) {
        if (!m.isDeferred)
          addConcreteSpecMethod(m)
        // specialized members have to be overridable.
        if (m.isPrivate)
          m.resetFlag(PRIVATE).setFlag(PROTECTED)

        if (m.isConstructor) {
          val specCtor = enterMember(cloneInSpecializedClass(m, x => x))
          info(specCtor) = Forward(m)
        }
        else if (isNormalizedMember(m)) {  // methods added by normalization
          val NormalizedMember(original) = info(m)
          if (nonConflicting(env ++ typeEnv(m))) {
            if (info(m).degenerate) {
              debuglog("degenerate normalized member " + m + " info(m): " + info(m))
              val specMember = enterMember(cloneInSpecializedClass(m, _ & ~DEFERRED))

              info(specMember)    = Implementation(original)
              typeEnv(specMember) = env ++ typeEnv(m)
            }
            else debuglog({
              val om = forwardToOverload(m)
              "normalizedMember " + m + " om: " + om + " typeEnv(om): " + typeEnv(om)
            })
          }
          else
            log("conflicting env for " + m + " env: " + env)
        }
        else if (m.isDeferred) { // abstract methods
          val specMember = enterMember(cloneInSpecializedClass(m, _ | DEFERRED))
          debuglog("deferred " + specMember.fullName + " remains abstract")

          info(specMember) = new Abstract(specMember)
          // was: new Forward(specMember) {
          //   override def target = m.owner.info.member(specializedName(m, env))
          // }
        } else if (m.isMethod && !m.hasAccessorFlag) { // other concrete methods
          // log("other concrete " + m)
          forwardToOverload(m)

        } else if (m.isValue && !m.isMethod) { // concrete value definition
          def mkAccessor(field: Symbol, name: Name) = {
            val newFlags = (SPECIALIZED | m.getter(clazz).flags) & ~(LOCAL | CASEACCESSOR | PARAMACCESSOR | LAZY)
            // we rely on the super class to initialize param accessors
            val sym = sClass.newMethod(name, field.pos, newFlags)
            info(sym) = SpecializedAccessor(field)
            sym
          }
          def overrideIn(clazz: Symbol, sym: Symbol) = {
            val newFlags = (sym.flags | OVERRIDE | SPECIALIZED) & ~(DEFERRED | CASEACCESSOR | PARAMACCESSOR | LAZY)
            val sym1     = sym.cloneSymbol(clazz, newFlags)
            sym1 modifyInfo (_ asSeenFrom (clazz.tpe, sym1.owner))
          }
          val specVal = specializedOverload(sClass, m, env)

          addConcreteSpecMethod(m)
          specVal.asInstanceOf[TermSymbol].setAlias(m)

          enterMember(specVal)
          // create accessors
          debuglog("m: " + m + " isLocal: " + nme.isLocalName(m.name) + " specVal: " + specVal.name + " isLocal: " + nme.isLocalName(specVal.name))
          if (nme.isLocalName(m.name)) {
            val specGetter = mkAccessor(specVal, nme.localToGetter(specVal.name)) setInfo MethodType(Nil, specVal.info)
            val origGetter = overrideIn(sClass, m.getter(clazz))
            info(origGetter) = Forward(specGetter)
            enterMember(specGetter)
            enterMember(origGetter)
            debuglog("created accessors: " + specGetter + " orig: " + origGetter)

            clazz.caseFieldAccessors.find(_.name.startsWith(m.name)) foreach { cfa =>
              val cfaGetter = overrideIn(sClass, cfa)
              info(cfaGetter) = SpecializedAccessor(specVal)
              enterMember(cfaGetter)
              debuglog("found case field accessor for " + m + " added override " + cfaGetter);
            }

            if (specVal.isVariable && m.setter(clazz) != NoSymbol) {
              val specSetter = mkAccessor(specVal, nme.getterToSetter(specGetter.name))
                .resetFlag(STABLE)
              specSetter.setInfo(MethodType(specSetter.newSyntheticValueParams(List(specVal.info)),
                                            UnitClass.tpe))
              val origSetter = overrideIn(sClass, m.setter(clazz))
              info(origSetter) = Forward(specSetter)
              enterMember(specSetter)
              enterMember(origSetter)
            }
          } else { // if there are no accessors, specialized methods will need to access this field in specialized subclasses
            m.resetFlag(PRIVATE)
            specVal.resetFlag(PRIVATE)
          }
        } else if (m.isClass) {
          val specClass: Symbol = cloneInSpecializedClass(m, x => x)
          typeEnv(specClass) = fullEnv
          specClass.name = specializedName(specClass, fullEnv).toTypeName
          enterMember(specClass)
          log("entered specialized class " + specClass.fullName)
          info(specClass) = SpecializedInnerClass(m, fullEnv)
        }
      }
      sClass
    }

    val decls1 = clazz.info.decls.toList flatMap { m: Symbol =>
      if (m.isAnonymousClass) List(m) else {
        normalizeMember(m.owner, m, outerEnv) flatMap { normalizedMember =>
          val ms = specializeMember(m.owner, normalizedMember, outerEnv, clazz.info.typeParams)
          // interface traits have concrete members now
          if (ms.nonEmpty && clazz.isTrait && clazz.isInterface)
            clazz.resetFlag(INTERFACE)

          if (normalizedMember.isMethod) {
            val newTpe = subst(outerEnv, normalizedMember.info)
            // only do it when necessary, otherwise the method type might be at a later phase already
            if (newTpe != normalizedMember.info) {
              normalizedMember updateInfo newTpe
            }
          }
          normalizedMember :: ms
        }
      }
    }

    val subclasses = specializations(clazz.info.typeParams) filter satisfiable
    subclasses foreach { env =>
      val spc      = specializedClass(env, decls1)
      val existing = clazz.owner.info.decl(spc.name)

      // a symbol for the specialized class already exists if there's a classfile for it.
      // keeping both crashes the compiler on test/files/pos/spec-Function1.scala
      if (existing != NoSymbol)
        clazz.owner.info.decls.unlink(existing)

      atPhase(phase.next)(clazz.owner.info.decls enter spc) //!!! assumes fully specialized classes
    }
    if (subclasses.nonEmpty) clazz.resetFlag(FINAL)
    cleanAnyRefSpecCache(clazz, decls1)
    decls1
  }

  /** Expand member `sym` to a set of normalized members. Normalized members
   *  are monomorphic or polymorphic only in non-specialized types.
   *
   *  Given method m[@specialized T, U](x: T, y: U) it returns
   *     m[T, U](x: T, y: U),
   *     m$I[ U](x: Int, y: U),
   *     m$D[ U](x: Double, y: U)
   *     // etc.
   */
  private def normalizeMember(owner: Symbol, sym: Symbol, outerEnv: TypeEnv): List[Symbol] = {
    debuglog("normalizeMember: " + sym.fullName)
    sym :: (
      if (!sym.isMethod || atPhase(currentRun.typerPhase)(sym.typeParams.isEmpty)) Nil
      else {
        var specializingOn = specializedParams(sym)
        val unusedStvars   = specializingOn filterNot specializedTypeVars(sym.info)

        if (unusedStvars.nonEmpty && currentRun.compiles(sym) && !sym.isSynthetic) {
          reporter.warning(sym.pos,
            "%s %s unused or used in non-specializable positions.".format(
              unusedStvars.mkString("", ", ", ""),
              if (unusedStvars.length == 1) "is" else "are")
          )
          unusedStvars foreach (_ removeAnnotation SpecializedClass)
          specializingOn = specializingOn filterNot (unusedStvars contains)
        }
        for (env0 <- specializations(specializingOn) if needsSpecialization(env0, sym)) yield {
          val tps          = survivingParams(sym.info.typeParams, env0)
          val specMember   = sym.cloneSymbol(owner, (sym.flags | SPECIALIZED) & ~DEFERRED)
          val env          = mapAnyRefsInSpecSym(env0, sym, specMember)
          val (keys, vals) = env.toList.unzip

          specMember.name = specializedName(sym, env)
          debuglog("normalizing: " + sym + " to " + specMember + " with params " + tps)

          typeEnv(specMember) = outerEnv ++ env
          val tps1 = produceTypeParameters(tps, specMember, env)
          tps1 foreach (_ modifyInfo (_.instantiateTypeParams(keys, vals)))

          // the cloneInfo is necessary so that method parameter symbols are cloned at the new owner
          val methodType = sym.info.resultType.instantiateTypeParams(keys ++ tps, vals ++ tps1.map(_.tpe)).cloneInfo(specMember)
          specMember setInfo polyType(tps1, methodType)

          debuglog("expanded member: " + sym  + ": " + sym.info +
            " -> " + specMember +
            ": " + specMember.info +
            " env: " + env
          )
          info(specMember) = NormalizedMember(sym)
          overloads(sym) ::= Overload(specMember, env)
          specMember
        }
      }
    )
  }

  /** Specialize member `m` w.r.t. to the outer environment and the type
   *  parameters of the innermost enclosing class.
   *
   *  Turns 'private' into 'protected' for members that need specialization.
   *
   *  Return a list of symbols that are specializations of 'sym', owned by 'owner'.
   */
  private def specializeMember(owner: Symbol, sym: Symbol, outerEnv: TypeEnv, tps: List[Symbol]): List[Symbol] = {
    def specializeOn(tparams: List[Symbol]): List[Symbol] =
      for (spec0 <- specializations(tparams)) yield {
        val spec = mapAnyRefsInOrigCls(spec0, owner)
        if (sym.isPrivate/* || sym.isProtected*/) {
          //sym.privateWithin = sym.enclosingPackage
          sym.resetFlag(PRIVATE).setFlag(PROTECTED)
          log("-->d SETTING PRIVATE WITHIN TO " + sym.enclosingPackage + " for " + sym)
        }

        val specMember = subst(outerEnv)(specializedOverload(owner, sym, spec))
        typeEnv(specMember) = typeEnv(sym) ++ outerEnv ++ spec
        wasSpecializedForTypeVars(specMember) ++= spec collect { case (s, tp) if s.tpe == tp => s }

        debuglog("sym " + specMember + " was specialized for type vars " + wasSpecializedForTypeVars(specMember))
        debuglog("added specialized overload: %s in env: %s".format(specMember, typeEnv(specMember)))

        overloads(sym) ::= Overload(specMember, spec)
        specMember
      }

    if (sym.isMethod) {
      debuglog("specializeMember %s with tps: %s stvars(sym): %s".format(sym, tps, specializedTypeVars(sym)))

      val tps1 = if (sym.isConstructor) tps filter (sym.info.paramTypes contains _) else tps
      val tps2 = tps1 intersect specializedTypeVars(sym).toList
      if (!sym.isDeferred)
        addConcreteSpecMethod(sym)

      val ms = specializeOn(tps2)
      ms foreach (m => info(m) = SpecialOverload(sym, typeEnv(m)))
      ms
    }
    else Nil
  }

  /** Return the specialized overload of `m`, in the given environment. */
  private def specializedOverload(owner: Symbol, sym: Symbol, env: TypeEnv): Symbol = {
    // this method properly duplicates the symbol's info
    val specMember  = sym.cloneSymbol(owner, (sym.flags | SPECIALIZED) & ~(DEFERRED | CASEACCESSOR | ACCESSOR | LAZY))
    specMember.name = specializedName(sym, env)
    specMember modifyInfo (info => subst(env, info.asSeenFrom(owner.thisType, sym.owner)))
  }

  /** For each method m that overrides an inherited method m', add a special
   *  overload method `om` that overrides the corresponding overload in the
   *  superclass. For the following example:
   *
   *  class IntFun extends Function1[Int, Int] {
   *    def apply(x: Int): Int = ..
   *  }
   *
   *  this method will return List('apply$mcII$sp')
   */
  private def specialOverrides(clazz: Symbol): List[Symbol] = {
    /** Return the overridden symbol in syms that needs a specialized overriding symbol,
     *  together with its specialization environment. The overridden symbol may not be
     *  the closest to 'overriding', in a given hierarchy.
     *
     *  An method m needs a special override if
     *    * m overrides a method whose type contains specialized type variables
     *    * there is a valid specialization environment that maps the overridden method type to m's type.
     */
    def needsSpecialOverride(overriding: Symbol): (Symbol, TypeEnv) = {
      def checkOverriddenTParams(overridden: Symbol) {
        foreach2(overridden.info.typeParams, overriding.info.typeParams) { (baseTvar, derivedTvar) =>
          val missing = concreteTypes(baseTvar).toSet -- concreteTypes(derivedTvar).toSet
          if (missing.nonEmpty) {
            reporter.error(derivedTvar.pos,
              "Type parameter has to be specialized at least for the same types as in the overridden method. Missing "
              + "types: " + missing.mkString("", ", ", "")
            )
          }
        }
      }
      if (!overriding.isParamAccessor) {
        for (overridden <- overriding.allOverriddenSymbols) {
          debuglog(
            "Overridden: " + overridden.fullName +
            ": " + overridden.info +
            "\n by " + overriding.fullName +
            ": " + overriding.info
          )
          val stvars = specializedTypeVars(overridden.info)
          if (stvars.nonEmpty) {
            debuglog("\t\tspecializedTVars: " + stvars)
            if (currentRun compiles overriding)
              checkOverriddenTParams(overridden)

            val env    = unify(overridden.info, overriding.info, emptyEnv, false)
            def atNext = atPhase(phase.next)(overridden.owner.info.decl(specializedName(overridden, env)))

            debuglog("\t\tenv: " + env + "isValid: " + TypeEnv.isValid(env, overridden) + "found: " + atNext)
            if (TypeEnv.restrict(env, stvars).nonEmpty && TypeEnv.isValid(env, overridden) && atNext != NoSymbol)
              return (overridden, env)
          }
        }
      }
      (NoSymbol, emptyEnv)
    }
    (clazz.info.decls flatMap { overriding =>
      needsSpecialOverride(overriding) match {
        case (NoSymbol, _)     => None
        case (overridden, env) =>
          val om = specializedOverload(clazz, overridden, env)
          log("Added specialized overload %s for %s in env: %s with type: %s".format(om, overriding.fullName, env, om.info))
          typeEnv(om) = env
          addConcreteSpecMethod(overriding)
          info(om) = (
            if (overriding.isDeferred) {    // abstract override
              debuglog("abstract override " + overriding.fullName + " with specialized " + om.fullName)
              Forward(overriding)
            }
            else {
              // if the override is a normalized member, 'om' gets the
              // implementation from its original target, and adds the
              // environment of the normalized member (that is, any
              // specialized /method/ type parameter bindings)
              val impl = info get overriding match {
                case Some(NormalizedMember(target)) =>
                  typeEnv(om) = env ++ typeEnv(overriding)
                  target
                case _ =>
                  overriding
              }
              info(overriding) = Forward(om setPos overriding.pos)
              SpecialOverride(impl)
            }
          )
          overloads(overriding) ::= Overload(om, env)
          ifDebug(atPhase(phase.next)(assert(
            overridden.owner.info.decl(om.name) != NoSymbol,
            "Could not find " + om.name + " in " + overridden.owner.info.decls))
          )
          Some(om)
      }
    }).toList
  }

  case object UnifyError extends scala.util.control.ControlThrowable

  /** Return the most general type environment that specializes tp1 to tp2.
   *  It only allows binding of type parameters annotated with @specialized.
   *  Fails if such an environment cannot be found.
   *
   *  If `strict` is true, a UnifyError is thrown if unification is impossible.
   */
  private def unify(tp1: Type, tp2: Type, env: TypeEnv, strict: Boolean): TypeEnv = (tp1, tp2) match {
    case (TypeRef(_, sym1, _), _) if isSpecialized(sym1) =>
      debuglog("Unify - basic case: " + tp1 + ", " + tp2)
      if (isValueClass(tp2.typeSymbol) || isSpecializedAnyRefSubtype(tp2, sym1))
        env + ((sym1, tp2))
      else
        if (strict) throw UnifyError else env
    case (TypeRef(_, sym1, args1), TypeRef(_, sym2, args2)) =>
      debuglog("Unify TypeRefs: " + tp1 + " and " + tp2 + " with args " + (args1, args2) + " - ")
      if (strict && args1.length != args2.length) throw UnifyError
      val e = unify(args1, args2, env, strict)
      debuglog("unified to: " + e)
      e
    case (TypeRef(_, sym1, _), _) if sym1.isTypeParameterOrSkolem =>
      env
    case (MethodType(params1, res1), MethodType(params2, res2)) =>
      if (strict && params1.length != params2.length) throw UnifyError
      debuglog("Unify MethodTypes: " + tp1 + " and " + tp2)
      unify(res1 :: (params1 map (_.tpe)), res2 :: (params2 map (_.tpe)), env, strict)
    case (PolyType(tparams1, res1), PolyType(tparams2, res2)) =>
      if (strict && tparams1.length != tparams2.length) throw UnifyError
      debuglog("Unify PolyTypes: " + tp1 + " and " + tp2)
      unify(res1, res2, env, strict)
    case (PolyType(_, res), other) =>
      unify(res, other, env, strict)
    case (ThisType(_), ThisType(_)) => env
    case (_, SingleType(_, _))                  => unify(tp1, tp2.underlying, env, strict)
    case (SingleType(_, _), _)                  => unify(tp1.underlying, tp2, env, strict)
    case (ThisType(_), _)                       => unify(tp1.widen, tp2, env, strict)
    case (_, ThisType(_))                       => unify(tp1, tp2.widen, env, strict)
    case (RefinedType(_, _), RefinedType(_, _)) => env
    case (AnnotatedType(_, tp1, _), tp2)        => unify(tp2, tp1, env, strict)
    case (ExistentialType(_, res1), _)          => unify(tp2, res1, env, strict)
    case _ =>
      debuglog("don't know how to unify %s [%s] with %s [%s]".format(tp1, tp1.getClass, tp2, tp2.getClass))
      env
  }

  private def unify(tp1: List[Type], tp2: List[Type], env: TypeEnv, strict: Boolean): TypeEnv = {
    if (tp1.isEmpty || tp2.isEmpty) env
    else (tp1 zip tp2).foldLeft(env) { (env, args) =>
      if (!strict) unify(args._1, args._2, env, strict)
      else {
        val nenv = unify(args._1, args._2, emptyEnv, strict)
        if (env.keySet intersect nenv.keySet isEmpty) env ++ nenv
        else {
          debuglog("could not unify: u(" + args._1 + ", " + args._2 + ") yields " + nenv + ", env: " + env)
          throw UnifyError
        }
      }
    }
  }

  /** Apply type bindings in the given environment `env` to all declarations.  */
  private def subst(env: TypeEnv, decls: List[Symbol]): List[Symbol] =
    decls map subst(env)

  /** Apply the type environment 'env' to the given type. All type
   *  bindings are supposed to be to primitive types. A type variable
   *  that is annotated with 'uncheckedVariance' is mapped to the corresponding
   *  primitive type losing the annotation.
   */
  private def subst(env: TypeEnv, tpe: Type): Type = {
    class FullTypeMap(from: List[Symbol], to: List[Type]) extends SubstTypeMap(from, to) with AnnotationFilter {
      def keepAnnotation(annot: AnnotationInfo) = !(annot matches uncheckedVarianceClass)

      override def mapOver(tp: Type): Type = tp match {
        case ClassInfoType(parents, decls, clazz) =>
          val parents1  = parents mapConserve this
          val decls1    = mapOver(decls)

          if ((parents1 eq parents) && (decls1 eq decls)) tp
          else ClassInfoType(parents1, decls1, clazz)
        case _ =>
          super.mapOver(tp)
      }
    }
    val (keys, values) = env.toList.unzip
    (new FullTypeMap(keys, values))(tpe)
  }

  private def subst(env: TypeEnv)(decl: Symbol): Symbol =
    decl modifyInfo (info =>
      if (decl.isConstructor) MethodType(subst(env, info).params, decl.owner.tpe)
      else subst(env, info)
    )

  /** Checks if the type parameter symbol is not specialized
   *  and is used as type parameters when extending a class with a specialized
   *  type parameter.
   *  At some point we may remove this restriction.
   *
   *  Example:
   *
   *    class Base[@specialized T]
   *    class Derived[T] extends Base[T] // a non-specialized T is
   *                                     // used as a type param for Base
   *                                     // -> returning true
   */
  private def notSpecializedIn(tsym: Symbol, supertpe: Type) = supertpe match {
    case TypeRef(_, supersym, supertargs) =>
      val tspec = specializedOn(tsym).toSet
      for (supt <- supersym.typeParams) {
        val supspec = specializedOn(supt).toSet
        if (tspec != supspec && tspec.subsetOf(supspec))
          reporter.error(tsym.pos, "Type parameter has to be specialized at least for the same types as in the superclass. Missing types: " + (supspec.diff(tspec)).mkString(", "))
      }
    case _ => //log("nope")
  }

  private def unspecializableClass(tp: Type) = (
       definitions.isRepeatedParamType(tp)  // ???
    || tp.typeSymbol.isJavaDefined
    || tp.typeSymbol.isPackageClass
  )

  /** Type transformation. It is applied to all symbols, compiled or loaded.
   *  If it is a 'no-specialization' run, it is applied only to loaded symbols.
   */
  override def transformInfo(sym: Symbol, tpe: Type): Type = {
    if (settings.nospecialization.value && currentRun.compiles(sym)) tpe
    else tpe.resultType match {
      case cinfo @ ClassInfoType(parents, decls, clazz) if !unspecializableClass(cinfo) =>
        val tparams  = tpe.typeParams
        if (tparams.isEmpty)
          atPhase(phase.next)(parents map (_.typeSymbol.info))

        val parents1 = parents map specializedType
        debuglog("transformInfo %s %s with parents1 %s ph: %s".format(
          if (tparams.nonEmpty) " (poly)" else "",
          clazz, parents1, phase)
        )
        val newScope = newScopeWith(specializeClass(clazz, typeEnv(clazz)) ++ specialOverrides(clazz): _*)
        // If tparams.isEmpty, this is just the ClassInfoType.
        polyType(tparams, ClassInfoType(parents1, newScope, clazz))
      case _ =>
        tpe
    }
  }

  /** Is any type variable in `env` conflicting with any if its type bounds, when
   *  type bindings in `env` are taken into account?
   *
   *  A conflicting type environment could still be satisfiable.
   */
  def conflicting(env: TypeEnv) = !nonConflicting(env)
  def nonConflicting(env: TypeEnv) = env forall { case (tvar, tpe) =>
    (subst(env, tvar.info.bounds.lo) <:< tpe) && (tpe <:< subst(env, tvar.info.bounds.hi))
  }

  /** The type environment is sound w.r.t. to all type bounds or only soft
   *  conflicts appear. An environment is sound if all bindings are within
   *  the bounds of the given type variable. A soft conflict is a binding
   *  that does not fall within the bounds, but whose bounds contain
   *  type variables that are @specialized, (that could become satisfiable).
   */
  def satisfiable(env: TypeEnv): Boolean = satisfiable(env, false)
  def satisfiable(env: TypeEnv, warnings: Boolean): Boolean = {
    def matches(tpe1: Type, tpe2: Type): Boolean = {
      val t1 = subst(env, tpe1)
      val t2 = subst(env, tpe2)
      ((t1 <:< t2)
        || specializedTypeVars(t1).nonEmpty
        || specializedTypeVars(t2).nonEmpty)
     }

    env forall { case (tvar, tpe) =>
      matches(tvar.info.bounds.lo, tpe) && matches(tpe, tvar.info.bounds.hi) || {
        if (warnings)
          reporter.warning(tvar.pos, "Bounds prevent specialization of " + tvar)

        log("specvars: " +
          tvar.info.bounds.lo + ": " +
          specializedTypeVars(tvar.info.bounds.lo) + " " +
          subst(env, tvar.info.bounds.hi) + ": " +
          specializedTypeVars(subst(env, tvar.info.bounds.hi))
        )
        false
      }
    }
  }

  class Duplicator extends {
    val global: SpecializeTypes.this.global.type = SpecializeTypes.this.global
  } with typechecker.Duplicators

  /** A tree symbol substituter that substitutes on type skolems.
   *  If a type parameter is a skolem, it looks for the original
   *  symbol in the 'from' and maps it to the corresponding new
   *  symbol. The new symbol should probably be a type skolem as
   *  well (not enforced).
   *
   *  All private members are made protected in order to be accessible from
   *  specialized classes.
   */
  class ImplementationAdapter(from: List[Symbol],
                              to: List[Symbol],
                              targetClass: Symbol,
                              addressFields: Boolean) extends TreeSymSubstituter(from, to) {
    override val symSubst = new SubstSymMap(from, to) {
      override def matches(sym1: Symbol, sym2: Symbol) =
        if (sym2.isTypeSkolem) sym2.deSkolemize eq sym1
        else sym1 eq sym2
    }

    private def isAccessible(sym: Symbol): Boolean =
      (currentClass == sym.owner.enclClass) && (currentClass != targetClass)

    private def shouldMakePublic(sym: Symbol): Boolean =
      sym.hasFlag(PRIVATE | PROTECTED) && (addressFields || !nme.isLocalName(sym.name))

    /** All private members that are referenced are made protected,
     *  in order to be accessible from specialized subclasses.
     */
    override def transform(tree: Tree): Tree = tree match {
      case Select(qual, name) =>
        val sym = tree.symbol
        if (sym.isPrivate) debuglog(
          "seeing private member %s, currentClass: %s, owner: %s, isAccessible: %b, isLocalName: %b".format(
            sym, currentClass, sym.owner.enclClass, isAccessible(sym), nme.isLocalName(sym.name))
        )
        if (shouldMakePublic(sym) && !isAccessible(sym)) {
          debuglog("changing private flag of " + sym)
          sym.makeNotPrivate(sym.owner)
        }
        super.transform(tree)

      case _ =>
        super.transform(tree)
    }
  }

  /** Return the generic class corresponding to this specialized class. */
  def originalClass(clazz: Symbol): Symbol =
    if (hasSpecializedFlag(clazz)) {
      val (originalName, _, _) = nme.splitSpecializedName(clazz.name)
      clazz.owner.info.decl(originalName).suchThat(_.isClass)
    } else NoSymbol

  def illegalSpecializedInheritance(clazz: Symbol): Boolean = (
       hasSpecializedFlag(clazz) 
    && originalClass(clazz).parentSymbols.exists(p => hasSpecializedParams(p) && !p.isTrait)
  )

  def specializeCalls(unit: CompilationUnit) = new TypingTransformer(unit) {
    /** Map a specializable method to it's rhs, when not deferred. */
    val body: mutable.Map[Symbol, Tree] = new mutable.HashMap

    /** Map a specializable method to its value parameter symbols. */
    val parameters: mutable.Map[Symbol, List[List[Symbol]]] = new mutable.HashMap

    /** Collect method bodies that are concrete specialized methods.
     */
    class CollectMethodBodies extends Traverser {
      override def traverse(tree: Tree) = tree match {
        case  DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          if (concreteSpecMethods(tree.symbol) || tree.symbol.isConstructor) {
            debuglog("!!! adding body of a defdef %s, symbol %s: %s".format(tree, tree.symbol, rhs))
            body(tree.symbol) = rhs
            //          body(tree.symbol) = tree // whole method
            parameters(tree.symbol) = mmap(vparamss)(_.symbol)
            concreteSpecMethods -= tree.symbol
          } // no need to descend further down inside method bodies

        case ValDef(mods, name, tpt, rhs) if concreteSpecMethods(tree.symbol) =>
          body(tree.symbol) = rhs
          // log("!!! adding body of a valdef " + tree.symbol + ": " + rhs)
          //super.traverse(tree)
        case _ =>
          super.traverse(tree)
      }
    }

    def doesConform(origSymbol: Symbol, treeType: Type, memberType: Type, env: TypeEnv) =
      (treeType =:= memberType) || { // anyref specialization
        memberType match {
          case PolyType(_, resTpe) =>
            debuglog("Conformance for anyref - polytype with result type: " + resTpe + " and " + treeType + "\nOrig. sym.: " + origSymbol)
            try {
              val e = unify(origSymbol.tpe, memberType, emptyEnv, true)
              debuglog("obtained env: " + e)
              e.keySet == env.keySet
            } catch {
              case _ =>
                debuglog("Could not unify.")
                false
            }
          case _ => false
        }
      }

    override def transform(tree: Tree): Tree = {
      val symbol = tree.symbol

      /** The specialized symbol of 'tree.symbol' for tree.tpe, if there is one */
      def specSym(qual: Tree): Option[Symbol] = {
        val env = unify(symbol.tpe, tree.tpe, emptyEnv, false)
        log("[specSym] checking for rerouting: %s with \n\tsym.tpe: %s, \n\ttree.tpe: %s \n\tenv: %s \n\tname: %s"
                .format(tree, symbol.tpe, tree.tpe, env, specializedName(symbol, env)))
        if (!env.isEmpty) {  // a method?
          val specCandidates = qual.tpe.member(specializedName(symbol, env))
          val specMember = specCandidates suchThat { s =>
            doesConform(symbol, tree.tpe, qual.tpe.memberType(s), env)
          }
          
          log("[specSym] found: " + specCandidates.tpe + ", instantiated as: " + tree.tpe)
          log("[specSym] found specMember: " + specMember)
          if (specMember ne NoSymbol)
            if (TypeEnv.includes(typeEnv(specMember), env)) Some(specMember)
            else {
              log("wrong environments for specialized member: \n\ttypeEnv(%s) = %s\n\tenv = %s".format(specMember, typeEnv(specMember), env))
              None
            }
          else None
        } else None
      }

      curTree = tree
      tree match {
        case Apply(Select(New(tpt), nme.CONSTRUCTOR), args) =>
          if (findSpec(tpt.tpe).typeSymbol ne tpt.tpe.typeSymbol) {
            // the ctor can be specialized
            log("** instantiated specialized type: " + findSpec(tpt.tpe))
            try {
              atPos(tree.pos)(
                localTyper.typed(
                  Apply(
                    Select(New(TypeTree(findSpec(tpt.tpe))), nme.CONSTRUCTOR),
                    transformTrees(args))))
            } catch {
              case te: TypeError =>
                reporter.error(tree.pos, te.msg)
                super.transform(tree)
            }
          } else super.transform(tree)

        case TypeApply(Select(qual, name), targs)
                if (!specializedTypeVars(symbol.info).isEmpty && name != nme.CONSTRUCTOR) =>
          debuglog("checking typeapp for rerouting: " + tree + " with sym.tpe: " + symbol.tpe + " tree.tpe: " + tree.tpe)
          val qual1 = transform(qual)
          // log(">>> TypeApply: " + tree + ", qual1: " + qual1)
          specSym(qual1) match {
            case Some(specMember) =>
              debuglog("found " + specMember.fullName)
              ifDebug(assert(symbol.info.typeParams.length == targs.length, symbol.info.typeParams + " / " + targs))

              val env = typeEnv(specMember)
              val residualTargs = symbol.info.typeParams zip targs collect {
                case (tvar, targ) if !env.contains(tvar) || !isValueClass(env(tvar).typeSymbol) => targ
              }

              ifDebug(assert(residualTargs.length == specMember.info.typeParams.length,
                "residual: %s, tparams: %s, env: %s".format(residualTargs, symbol.info.typeParams, env))
              )

              val tree1 = gen.mkTypeApply(Select(qual1, specMember), residualTargs)
              log("rewrote " + tree + " to " + tree1)
              localTyper.typedOperator(atPos(tree.pos)(tree1)) // being polymorphic, it must be a method

            case None => super.transform(tree)
          }

        case Select(Super(_, _), name) if illegalSpecializedInheritance(currentClass) =>
          val pos = tree.pos
          log(pos.source.file.name+":"+pos.line+": not specializing call to super inside illegal specialized inheritance class.")
          log(pos.lineContent)
          tree

        case Select(qual, name) =>
          debuglog("[%s] looking at Select: %s sym: %s: %s [tree.tpe: %s]".format(
            tree.pos.safeLine, tree, symbol, symbol.info, tree.tpe))

          //log("!!! select " + tree + " -> " + symbol.info + " specTypeVars: " + specializedTypeVars(symbol.info))
          if (specializedTypeVars(symbol.info).nonEmpty && name != nme.CONSTRUCTOR) {
            // log("!!! unifying " + (symbol, symbol.tpe) + " and " + (tree, tree.tpe))
            val env = unify(symbol.tpe, tree.tpe, emptyEnv, false)
            // log("!!! found env: " + env + "; overloads: " + overloads(symbol))
            debuglog("checking for rerouting: " + tree + " with sym.tpe: " + symbol.tpe + " tree.tpe: " + tree.tpe + " env: " + env)
            if (!env.isEmpty) {
              val specMember = overload(symbol, env)
              //log("!!! found member: " + specMember)
              if (specMember.isDefined) {
                // log("** routing " + tree + " to " + specMember.get.sym.fullName)
                localTyper.typedOperator(atPos(tree.pos)(Select(transform(qual), specMember.get.sym.name)))
              } else {
                val qual1 = transform(qual)
                val specMember = qual1.tpe.member(specializedName(symbol, env)).suchThat(_.tpe matches subst(env, symbol.tpe))
                if (specMember ne NoSymbol) {
                  // log("** using spec member " + specMember + ": " + specMember.tpe)
                  val tree1 = atPos(tree.pos)(Select(qual1, specMember))
                  if (specMember.isMethod)
                    localTyper.typedOperator(tree1)
                  else
                    localTyper.typed(tree1)
                } else
                  treeCopy.Select(tree, qual1, name)
              }
            } else
              super.transform(tree)
          } else overloads(symbol).find(_.sym.info =:= symbol.info) match {
              case Some(specMember) =>
                val qual1 = transform(qual)
                debuglog("** routing " + tree + " to " + specMember.sym.fullName + " tree: " + Select(qual1, specMember.sym))
                localTyper.typedOperator(atPos(tree.pos)(Select(qual1, specMember.sym)))
              case None =>
                super.transform(tree)
          }

        case PackageDef(pid, stats) =>
          tree.symbol.info // make sure specializations have been performed
          atOwner(tree, symbol) {
            val specMembers = implSpecClasses(stats) map localTyper.typed
            treeCopy.PackageDef(tree, pid, transformStats(stats ::: specMembers, symbol.moduleClass))
          }

        case Template(parents, self, body) =>
          val specMembers = makeSpecializedMembers(tree.symbol.enclClass) ::: (implSpecClasses(body) map localTyper.typed)
          if (!symbol.isPackageClass)
            (new CollectMethodBodies)(tree)
          val parents1 = map2(currentOwner.info.parents, parents)((tpe, parent) =>
            TypeTree(tpe) setPos parent.pos)
          
          treeCopy.Template(tree,
            parents1    /*currentOwner.info.parents.map(tpe => TypeTree(tpe) setPos parents.head.pos)*/ ,
            self,
            atOwner(currentOwner)(transformTrees(body ::: specMembers)))

        case ddef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) if info.isDefinedAt(symbol) =>
          // log("--> method: " + ddef + " in " + ddef.symbol.owner + ", " + info(symbol))
          if (symbol.isConstructor) {

            val t = atOwner(symbol) {
              val superRef: Tree = Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR)
              forwardCtorCall(tree.pos, superRef, vparamss, symbol.owner)
            }
            if (symbol.isPrimaryConstructor) localTyper typed {
                atPos(symbol.pos)(treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt, Block(List(t), Literal(Constant()))))
            } else {
              // duplicate the original constructor
              duplicateBody(ddef, info(symbol).target)
            }
          } else info(symbol) match {

            case Implementation(target) =>
              assert(body.isDefinedAt(target), "sym: " + symbol.fullName + " target: " + target.fullName)
              // we have an rhs, specialize it
              val tree1 = duplicateBody(ddef, target)
              debuglog("implementation: " + tree1)
              val DefDef(mods, name, tparams, vparamss, tpt, rhs) = tree1
              treeCopy.DefDef(tree1, mods, name, tparams, vparamss, tpt, transform(rhs))

            case NormalizedMember(target) =>
              log("Normalized member: " + symbol + ", target: " + target)
              if (target.isDeferred || conflicting(typeEnv(symbol))) {
                treeCopy.DefDef(
                  tree, mods, name, tparams, vparamss, tpt,
                  localTyper typed gen.mkSysErrorCall("boom! you stepped on a bug. This method should never be called.")
                )
              }
              else {
                // we have an rhs, specialize it
                val tree1 = duplicateBody(ddef, target)
                debuglog("implementation: " + tree1)
                val DefDef(mods, name, tparams, vparamss, tpt, rhs) = tree1
                treeCopy.DefDef(tree1, mods, name, tparams, vparamss, tpt, transform(rhs))
              }

            case SpecialOverride(target) =>
              assert(body.isDefinedAt(target), "sym: " + symbol.fullName + " target: " + target.fullName)
              //debuglog("moving implementation, body of target " + target + ": " + body(target))
              log("%s is param accessor? %b".format(ddef.symbol, ddef.symbol.isParamAccessor))
              // we have an rhs, specialize it
              val tree1 = addBody(ddef, target)
              (new ChangeOwnerTraverser(target, tree1.symbol))(tree1.rhs)
              debuglog("changed owners, now: " + tree1)
              val DefDef(mods, name, tparams, vparamss, tpt, rhs) = tree1
              treeCopy.DefDef(tree1, mods, name, tparams, vparamss, tpt, transform(rhs))


            case SpecialOverload(original, env) =>
              debuglog("completing specialized " + symbol.fullName + " calling " + original)
              log("special overload " + original + " -> " + env)
              val t = DefDef(symbol, { vparamss =>
                val fun = Apply(Select(This(symbol.owner), original),
                                makeArguments(original, vparamss.head))

                log("inside defdef: " + symbol + "; type: " + symbol.tpe + "; owner: " + symbol.owner)
                gen.maybeMkAsInstanceOf(fun,
                  symbol.owner.thisType.memberType(symbol).finalResultType,
                  symbol.owner.thisType.memberType(original).finalResultType)
              })
              log("created special overload tree " + t)
              debuglog("created " + t)
              localTyper.typed(t)

            case fwd @ Forward(_) =>
              log("forward: " + fwd + ", " + ddef)
              val rhs1 = forwardCall(tree.pos, gen.mkAttributedRef(symbol.owner.thisType, fwd.target), vparamss)
              log("-->d completed forwarder to specialized overload: " + fwd.target + ": " + rhs1)
              localTyper.typed(treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt, rhs1))

            case SpecializedAccessor(target) =>
              val rhs1 = if (symbol.isGetter)
                gen.mkAttributedRef(target)
              else
                Assign(gen.mkAttributedRef(target), Ident(vparamss.head.head.symbol))
              log("specialized accessor: " + target + " -> " + rhs1)
              localTyper.typed(treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt, rhs1))

            case Abstract(targ) =>
              log("abstract: " + targ)
              val DefDef(mods, name, tparams, vparamss, tpt, rhs) = tree
              val t = treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt, rhs)
              localTyper.typed(t)
          }

        case ValDef(mods, name, tpt, rhs) if symbol.hasFlag(SPECIALIZED) && !symbol.isParamAccessor =>
          assert(body.isDefinedAt(symbol.alias), body)
          val tree1 = treeCopy.ValDef(tree, mods, name, tpt, body(symbol.alias).duplicate)
          debuglog("now typing: " + tree1 + " in " + tree.symbol.owner.fullName)
          val d = new Duplicator
          val ValDef(mods1, name1, tpt1, rhs1) = d.retyped(
            localTyper.context1.asInstanceOf[d.Context],
            tree1,
            symbol.alias.enclClass,
            symbol.enclClass,
            typeEnv(symbol.alias) ++ typeEnv(tree.symbol)
          )
          val t = treeCopy.ValDef(tree1, mods1, name1, tpt1, transform(rhs1))
          log("valdef " + tree + " -> " + t)
          t

//          val tree1 =
//            treeCopy.ValDef(tree, mods, name, tpt,
//              localTyper.typed(
//                Apply(Select(Super(currentClass, nme.EMPTY), symbol.alias.getter(symbol.alias.owner)),
//                      List())))
//          debuglog("replaced ValDef: " + tree1 + " in " + tree.symbol.owner.fullName)
//          tree1

        case Apply(sel @ Select(sup @ Super(qual, name), name1), args)
          if (sup.symbol.info.parents != atPhase(phase.prev)(sup.symbol.info.parents)) =>

          def parents = sup.symbol.info.parents
          debuglog(tree + " parents changed from: " + atPhase(phase.prev)(parents) + " to: " + parents)

          val res = localTyper.typed(
            Apply(Select(Super(qual, name) setPos sup.pos, name1) setPos sel.pos, transformTrees(args)) setPos tree.pos)
          debuglog("retyping call to super, from: " + symbol + " to " + res.symbol)
          res

        case _ =>
          super.transform(tree)
      }
    }

    private def reskolemize(tparams: List[TypeDef]): (List[Symbol], List[Symbol]) = {
      val saved = tparams map (_.symbol)
      localTyper skolemizeTypeParams tparams
      (saved, tparams map (_.symbol))
    }

    private def duplicateBody(tree: DefDef, source: Symbol) = {
      val symbol = tree.symbol
      val meth   = addBody(tree, source)

      val d = new Duplicator
      log("-->d DUPLICATING: " + meth)
      d.retyped(
        localTyper.context1.asInstanceOf[d.Context],
        meth,
        source.enclClass,
        symbol.enclClass,
        typeEnv(source) ++ typeEnv(symbol)
      )
    }

    /** Put the body of 'source' as the right hand side of the method 'tree'.
     *  The destination method gets fresh symbols for type and value parameters,
     *  and the body is updated to the new symbols, and owners adjusted accordingly.
     *  However, if the same source tree is used in more than one place, full re-typing
     *  is necessary. @see method duplicateBody
     */
    private def addBody(tree: DefDef, source: Symbol): DefDef = {
      val symbol = tree.symbol
      debuglog("specializing body of" + symbol.fullName + ": " + symbol.info)
      val DefDef(mods, name, tparams, vparamss, tpt, _) = tree
//      val (_, origtparams) = splitParams(source.typeParams)
      val env = typeEnv(symbol)
      val boundTvars = env.keySet
      val origtparams = source.typeParams.filter(tparam => !boundTvars(tparam) || !isScalaValueType(env(tparam)))
      debuglog("substituting " + origtparams + " for " + symbol.typeParams)

      // skolemize type parameters
      val (oldtparams, newtparams) = reskolemize(tparams)

      // create fresh symbols for value parameters to hold the skolem types
      val vparamss1 = List(for (vdef <- vparamss.head; param = vdef.symbol) yield {
        ValDef(param cloneSymbol symbol substInfo (oldtparams, newtparams))
      })

      // replace value and type parameters of the old method with the new ones
      // log("Adding body for " + tree.symbol + " - origtparams: " + origtparams + "; tparams: " + tparams)
      // log("Type vars of: " + source + ": " + source.typeParams)
      // log("Type env of: " + tree.symbol + ": " + boundTvars)
      // log("newtparams: " + newtparams)
      val symSubstituter = new ImplementationAdapter(
        parameters(source).flatten ::: origtparams,
        vparamss1.flatten.map(_.symbol) ::: newtparams,
        source.enclClass,
        false) // don't make private fields public
      val tmp = symSubstituter(body(source).duplicate)
      tpt.tpe = tpt.tpe.substSym(oldtparams, newtparams)

      treeCopy.DefDef(tree, mods, name, tparams, vparamss1, tpt, tmp)
    }

    /** Create trees for specialized members of 'sClass', based on the
     *  symbols that are already there.
     */
    private def makeSpecializedMembers(sClass: Symbol): List[Tree] = {
      // add special overrides first
//      if (!specializedClass.hasFlag(SPECIALIZED))
//        for (m <- specialOverrides(specializedClass)) specializedClass.info.decls.enter(m)
      val mbrs = new mutable.ListBuffer[Tree]
      var hasSpecializedFields = false

      for (m <- sClass.info.decls
             if m.hasFlag(SPECIALIZED)
                 && (m.sourceFile ne null)
                 && satisfiable(typeEnv(m), !sClass.hasFlag(SPECIALIZED))) {
        log("creating tree for " + m.fullName)
        if (m.isMethod)  {
          if (info(m).target.hasAccessorFlag) hasSpecializedFields = true
          if (m.isClassConstructor) {
            val origParamss = parameters(info(m).target)
            val vparams = (
              map2(m.info.paramTypes, origParamss(0))((tp, sym) =>
                m.newValue(specializedName(sym, typeEnv(sClass)), sym.pos, sym.flags) setInfo tp
              )
            )
            // param accessors for private members (the others are inherited from the generic class)
            if (m.isPrimaryConstructor) {
              for (param <- vparams ; if sClass.info.nonPrivateMember(param.name) == NoSymbol) {
                val acc = param.cloneSymbol(sClass, param.flags | PARAMACCESSOR | PRIVATE)
                sClass.info.decls.enter(acc)
                mbrs += ValDef(acc, EmptyTree).setType(NoType).setPos(m.pos)
              }
            }

            // ctor
            mbrs += atPos(m.pos)(DefDef(m, Modifiers(m.flags), mmap(List(vparams))(ValDef), EmptyTree))
          } else {
            mbrs += atPos(m.pos)(DefDef(m, { paramss => EmptyTree }))
          }
        } else if (m.isValue) {
          mbrs += ValDef(m, EmptyTree).setType(NoType).setPos(m.pos)
        } else if (m.isClass) {
//           mbrs  +=
//              ClassDef(m, Template(m.info.parents map TypeTree, emptyValDef, List())
//                         .setSymbol(m.newLocalDummy(m.pos)))
//            log("created synthetic class: " + m.fullName)
        }
      }
      if (hasSpecializedFields) {
        val isSpecializedInstance = sClass :: sClass.parentSymbols exists (_ hasFlag SPECIALIZED)
        val sym = sClass.newMethod(nme.SPECIALIZED_INSTANCE, sClass.pos) setInfoAndEnter MethodType(Nil, BooleanClass.tpe)

        mbrs += atPos(sym.pos) {
          DefDef(sym, Literal(Constant(isSpecializedInstance)).setType(BooleanClass.tpe)).setType(NoType)
        }
      }
      mbrs.toList
    }

    /** Create specialized class definitions */
    def implSpecClasses(trees: List[Tree]): List[Tree] = {
      val buf = new mutable.ListBuffer[Tree]
      for (tree <- trees)
        tree match {
          case ClassDef(_, _, _, impl) =>
            tree.symbol.info // force specialization
            for (((sym1, env), specCls) <- specializedClass if sym1 == tree.symbol) {
              val parents = specCls.info.parents.map(TypeTree)
              buf +=
                ClassDef(specCls, atPos(impl.pos)(Template(parents, emptyValDef, List()))
                           .setSymbol(specCls.newLocalDummy(sym1.pos))) setPos tree.pos
              log("created synthetic class: " + specCls + " of " + sym1 + " in env: " + env)
            }
          case _ =>
        }
      buf.toList
    }
  }

  private def forwardCall(pos: util.Position, receiver: Tree, paramss: List[List[ValDef]]): Tree = {
    val argss = mmap(paramss)(x => Ident(x.symbol))
    atPos(pos) { (receiver /: argss) (Apply) }
  }

  /** Forward to the generic class constructor. If the current class initializes
   *  specialized fields corresponding to parameters, it passes null to the superclass
   *  constructor. This saves the boxing cost for initializing generic fields that are
   *  never used.
   *
   *  For example:
   *  {{{
   *    case class Tuple2[T, U](x: T, y: U)
   *
   *    class Tuple2$II {
   *      val _x$I: Int = ..
   *      def x = _x$I
   *      // same for y
   *      def this(x: Int, y: Int) {
   *        super.this(null.asInstanceOf[Int], null.asInstanceOf[Int])
   *      }
   *    }
   *  }}
   */
  private def forwardCtorCall(pos: util.Position, receiver: Tree, paramss: List[List[ValDef]], clazz: Symbol): Tree = {

    /** A constructor parameter `f` initializes a specialized field
     *  iff:
     *    - it is specialized itself
     *    - there is a getter for the original (non-specialized) field in the same class
     *    - there is a getter for the specialized field in the same class
     */
    def initializesSpecializedField(f: Symbol) = (
         (f.name endsWith nme.SPECIALIZED_SUFFIX_NAME)
      && clazz.info.member(nme.originalName(f.name)).isPublic
      && clazz.info.decl(f.name).suchThat(_.isGetter) != NoSymbol
    )

    val argss = mmap(paramss)(x =>
      if (initializesSpecializedField(x.symbol))
        gen.mkAsInstanceOf(Literal(Constant(null)), x.symbol.tpe)
      else
        Ident(x.symbol)
    )
    atPos(pos) { (receiver /: argss) (Apply) }
  }

  /** Add method m to the set of symbols for which we need an implementation tree
   *  in the tree transformer.
   *
   *  @note This field is part of the specializeTypes subcomponent, so any symbols
   *        that here are not garbage collected at the end of a compiler run!
   */
  def addConcreteSpecMethod(m: Symbol) {
    if (currentRun.compiles(m)) concreteSpecMethods += m
  }

  private def makeArguments(fun: Symbol, vparams: List[Symbol]): List[Tree] = (
    //! TODO: make sure the param types are seen from the right prefix
    map2(fun.info.paramTypes, vparams)((tp, arg) => gen.maybeMkAsInstanceOf(Ident(arg), tp, arg.tpe))
  )
  private def findSpec(tp: Type): Type = tp match {
    case TypeRef(pre, sym, _ :: _) => specializedType(tp)
    case _                         => tp
  }

  class SpecializationTransformer(unit: CompilationUnit) extends Transformer {
    informProgress("specializing " + unit)
    override def transform(tree: Tree) = {
      val resultTree = if (settings.nospecialization.value) tree
      else atPhase(phase.next)(specializeCalls(unit).transform(tree))

      // Remove the final modifier and @inline annotation from anything in the
      // original class (since it's being overridden in at least onesubclass).
      //
      // We do this here so that the specialized subclasses will correctly copy
      // final and @inline.
      info.foreach {
        case (sym, SpecialOverload(target, _)) => {
          sym.resetFlag(FINAL)
          target.resetFlag(FINAL)
          sym.removeAnnotation(ScalaInlineClass)
          target.removeAnnotation(ScalaInlineClass)
        }
        case _ => {}
      }

      resultTree
    }
  }

  def printSpecStats() {
    println("    concreteSpecMembers: %7d".format(concreteSpecMethods.size))
    println("    overloads:           %7d".format(overloads.size))
    println("    typeEnv:             %7d".format(typeEnv.size))
    println("    info:                %7d".format(info.size))
  }
}
