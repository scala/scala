/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package tools.nsc
package transform

import scala.annotation._
import scala.collection.mutable, mutable.{Buffer, HashMap, ListBuffer}
import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.Reporting.WarningCategory
import scala.util.chaining._

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
 *       class A\$mcI\$sp(protected val d: Int) extends A[Int] {
 *         def foo(that: A[Int]) = foo\$mcI\$sp(that)
 *         def foo(that: A[Int]) = that.d
 *       }
 *     }}}
 *
 *     Above, `A\$mcI\$sp` cannot access `d`, so the method cannot be typechecked.
 */
abstract class SpecializeTypes extends InfoTransform with TypingTransformers {
  import global._
  import definitions._
  import Flags._

  private val inlineFunctionExpansion = settings.Ydelambdafy.value == "inline"

  /** the name of the phase: */
  val phaseName: String = "specialize"

  /** The following flags may be set by this phase: */
  override def phaseNewFlags: Long = notPRIVATE

  /** This phase changes base classes. */
  override def changesBaseClasses = true
  override def keepsTypeParams = true

  type TypeEnv = Map[Symbol, Type]
  def emptyEnv: TypeEnv = Map.empty[Symbol, Type]

  private implicit val typeOrdering: Ordering[Type] = Ordering[String] on ("" + _.typeSymbol.name)

  /** For a given class and concrete type arguments, give its specialized class */
  val specializedClass = perRunCaches.newAnyRefMap[Symbol, HashMap[TypeEnv, Symbol]]()

  // read-only map, where missing value defaults to empty immutable.Map
  def specializationOf(sym: Symbol) = specializedClass.getOrElse(sym, Map.empty[TypeEnv, Symbol])

  /** Map a method symbol to a list of its specialized overloads in the same class. */
  private val overloads = perRunCaches.newMap[Symbol, List[Overload]]() withDefaultValue Nil

  /** Map a symbol to additional information on specialization. */
  private val info = perRunCaches.newMap[Symbol, SpecializedInfo]()

  /** Map class symbols to the type environments where they were created. */
  private val typeEnv = perRunCaches.newMap[Symbol, TypeEnv]() withDefaultValue emptyEnv

  //    Key: a specialized class or method
  //  Value: a map from tparams in the original class to tparams in the specialized class.
  private val anyrefSpecCache = perRunCaches.newMap[Symbol, mutable.Map[Symbol, Symbol]]()

  // holds mappings from members to the type variables in the class
  // that they were already specialized for, so that they don't get
  // specialized twice (this is for AnyRef specializations)
  private val wasSpecializedForTypeVars = perRunCaches.newMap[Symbol, Set[Symbol]]() withDefaultValue Set()

  /** Concrete methods that use a specialized type, or override such methods. */
  private val concreteSpecMethods = perRunCaches.newWeakSet[Symbol]()

  private def specializedOn(sym: Symbol): List[Symbol] = {
    val GroupOfSpecializable = currentRun.runDefinitions.GroupOfSpecializable
    def expandGroup(tp: Type): List[Symbol] =
      tp.baseType(GroupOfSpecializable) match {
        case TypeRef(_, GroupOfSpecializable, arg :: Nil) => arg.typeArgs.map(_.typeSymbol)
        case _                                            => tp.typeSymbol :: Nil
      }
    sym.getAnnotation(SpecializedClass) match {
      case Some(AnnotationInfo(_, Nil, _))  => specializableTypes.map(_.typeSymbol)
      case Some(AnnotationInfo(_, args, _)) => args.map(_.tpe).flatMap(expandGroup)
      case _                                => Nil
    }
  }

  private def hasNewParents(tree: Tree) = {
    val parents = tree.symbol.info.parents
    val prev    = enteringPrevPhase(tree.symbol.info.parents)
    (parents != prev) && {
      debuglog(s"$tree parents changed from: $prev to: $parents")
      true
    }
  }

  // If we replace `isBoundedGeneric` with (tp <:< AnyRefTpe),
  // then pos/spec-List.scala fails - why? Does this kind of check fail
  // for similar reasons? Does `sym.isAbstractType` make a difference?
  private def isSpecializedAnyRefSubtype(tp: Type, sym: Symbol) = {
    specializedOn(sym).exists(s => !isPrimitiveValueClass(s)) &&
    !isPrimitiveValueClass(tp.typeSymbol) &&
    isBoundedGeneric(tp)
    //(tp <:< AnyRefTpe)
  }

  object TypeEnv {
    /** Return a new type environment binding specialized type parameters of sym to
     *  the given args. Expects the lists to have the same length.
     */
    def fromSpecialization(sym: Symbol, args: List[Type]): TypeEnv = {
      ifDebug(assert(sym.info.typeParams.sizeCompare(args) == 0, "" + sym + " args: " + args))

      emptyEnv ++ collectMap2(sym.info.typeParams, args)((k, _) => k.isSpecialized)
    }

    /** Does typeenv `t1` include `t2`? All type variables in `t1`
     *  are defined in `t2` and:
     *  - are bound to the same type, or
     *  - are an AnyRef specialization and `t2` is bound to a subtype of AnyRef
     */
    def includes(t1: TypeEnv, t2: TypeEnv) = t1 forall {
      case (sym, tpe) =>
        t2 get sym exists { t2tp =>
          (tpe == t2tp) || !(isPrimitiveValueType(tpe) || isPrimitiveValueType(t2tp)) // u.t.b. (t2tp <:< AnyRefTpe)
        }
    }

    /** Reduce the given environment to contain mappings only for type variables in tps. */
    def restrict(env: TypeEnv, tps: Set[Symbol]): TypeEnv =
      env.view.filterKeys(tps).toMap

    /** Is the given environment a valid specialization for sym?
     *  It is valid if each binding is from a @specialized type parameter in sym (or its owner)
     *  to a type for which `sym` is specialized.
     */
    def isValid(env: TypeEnv, sym: Symbol): Boolean = {
      env forall { case (tvar, tpe) =>
        tvar.isSpecialized && (concreteTypes(tvar) contains tpe) && {
          (sym.typeParams contains tvar) ||
          (sym.owner != rootMirror.RootClass && (sym.owner.typeParams contains tvar))
        }
      }
    }
  }

  case class Overload(sym: Symbol, env: TypeEnv) {
    override def toString = "specialized overload " + sym + " in " + env
    def matchesSym(sym1: Symbol)  = sym.info =:= sym1.info
    def matchesEnv(env1: TypeEnv) = TypeEnv.includes(env, env1)
  }
  private def newOverload(method: Symbol, specializedMethod: Symbol, env: TypeEnv) = {
    assert(!specializedMethod.isOverloaded, specializedMethod.defString)
    val om = Overload(specializedMethod, env)
    overloads(method) ::= om
    om
  }

  /** Just to mark uncheckable */
  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new SpecializationPhase(prev)
  class SpecializationPhase(prev: scala.tools.nsc.Phase) extends InfoPhase(prev) {
    override def checkable = false
    override def run(): Unit = {
      super.run()
      exitingSpecialize {
        FunctionClass.seq.take(MaxFunctionAritySpecialized + 1).foreach(_.info)
        TupleClass.seq.take(MaxTupleAritySpecialized).foreach(_.info)
      }

      // Remove the final modifier and @inline annotation from anything in the
      // original class (since it's being overridden in at least one subclass).
      //
      // We do this here so that the specialized subclasses will correctly copy
      // final and @inline.
      //
      // TODO Try to move this logic back to the info transform.
      info.foreach {
        case (sym, SpecialOverload(target, _)) =>
          sym.resetFlag(FINAL)
          target.resetFlag(FINAL)
          sym.removeAnnotation(ScalaInlineClass)
          target.removeAnnotation(ScalaInlineClass)
        case _ =>
      }
    }
  }

  protected def newTransformer(unit: CompilationUnit): AstTransformer =
    new SpecializationTransformer(unit)

  sealed abstract class SpecializedInfo {
    def target: Symbol

    /** Are type bounds of @specialized type parameters of 'target' now in 'env'? */
    def typeBoundsIn(env: TypeEnv) = false

    /** A degenerated method has @specialized type parameters that appear only in
     *  type bounds of other @specialized type parameters (and not in its result type).
     */
    def degenerate = false
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

  /** Symbol is a special overload of the super accessor. Treated like an abstract method with no specialized overload. */
  case class SpecialSuperAccessor(t: Symbol) extends SpecializedInfo {
    def target = t
  }

  /** Symbol is a specialized accessor for the `target` field. */
  case class SpecializedAccessor(target: Symbol) extends SpecializedInfo

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
        tvar.isSpecialized && (specializedTypeVars(tvar.info.bounds) exists env.isDefinedAt)
      }
    }

    override lazy val degenerate = {
      val stvTypeParams = specializedTypeVars(target.info.typeParams map (_.info))
      val stvResult     = specializedTypeVars(target.info.resultType)

      debuglog("degenerate: " + target + " stv tparams: " + stvTypeParams + " stv info: " + stvResult)

      (stvTypeParams diff stvResult).nonEmpty
    }
  }

  /** Has `clazz` any type parameters that need be specialized? */
  def hasSpecializedParams(clazz: Symbol) =
    clazz.info.typeParams exists (_.isSpecialized)

  /** Return specialized type parameters. */
  def specializedParams(sym: Symbol): List[Symbol] =
    sym.info.typeParams filter (_.isSpecialized)

  /** Given an original class symbol and a list of types its type parameters are instantiated at
   *  returns a list of type parameters that should remain in the TypeRef when instantiating a
   *  specialized type.
   */
  def survivingArgs(sym: Symbol, args: List[Type]): List[Type] =
    for ((tvar, tpe) <- sym.info.typeParams.zip(args) if !tvar.isSpecialized || !isPrimitiveValueType(tpe))
      yield tpe

  /** Is `member` potentially affected by specialization? This is a gross overapproximation,
    * but it should be okay for use outside of specialization.
    */
  def possiblySpecialized(sym: Symbol) = specializedTypeVars(sym).nonEmpty

  /** Refines possiblySpecialized taking into account the instantiation of the specialized type variables at `site` */
  def isSpecializedIn(sym: Symbol, site: Type) =
    specializedTypeVars(sym) exists { tvar =>
      val concretes = concreteTypes(tvar)
      (concretes contains AnyRefTpe) || (concretes contains site.memberType(tvar))
    }


  val specializedType = new TypeMap {
    override def apply(tp: Type): Type = tp match {
      case TypeRef(pre, sym, args) if args.nonEmpty =>
        val pre1 = this(pre)
        // when searching for a specialized class, take care to map all
        // type parameters that are subtypes of AnyRef to AnyRef
        val args1 = map2(args, sym.info.typeParams)((tp, orig) =>
          if (isSpecializedAnyRefSubtype(tp, orig)) AnyRefTpe
          else tp
        )
        specializationOf(sym).get(TypeEnv.fromSpecialization(sym, args1)) match {
          case Some(sym1) => typeRef(pre1, sym1, survivingArgs(sym, args))
          case None       => typeRef(pre1, sym, args)
        }
      case _ => tp
    }
  }

  def specializedFunctionName(sym: Symbol, args: List[Type]) = exitingSpecialize {
    require(isFunctionSymbol(sym), sym)
    specializationOf(sym).get(TypeEnv.fromSpecialization(sym, args)) match {
      case Some(x) => x.name
      case None    => sym.name
    }
  }

  /** Return the specialized name of 'sym' in the given environment. It
   *  guarantees the same result regardless of the map order by sorting
   *  type variables alphabetically.
   *
   *  !!! Is this safe in the face of the following?
   *    scala> trait T { def foo[A] = 0}; object O extends T { override def foo[B] = 0 }
   */
  private def specializedName(sym: Symbol, env: TypeEnv): TermName = {
    val tvars = (
      if (sym.isClass) env.keySet
      else specializedTypeVars(sym).intersect(env.keySet)
    )
    specializedName(sym.name, tvars, env)
  }

  private def specializedName(name: Name, tvars: Set[Symbol], env: TypeEnv): TermName = {
    val (methparams, others) = tvars.toList sortBy ("" + _.name) partition (_.owner.isMethod)
    // debuglog("specName(" + sym + ") env: " + env + " tvars: " + tvars)

    specializedName(name, methparams map env, others map env)
  }

  /** Specialize name for the two list of types. The first one denotes
   *  specialization on method type parameters, the second on outer environment.
   */
  private def specializedName(name: Name, types1: List[Type], types2: List[Type]): TermName = (
    if (name == nme.CONSTRUCTOR || (types1.isEmpty && types2.isEmpty))
      name.toTermName
    else if (nme.isSetterName(name))
      specializedName(name.getterName, types1, types2).setterName
    else if (nme.isLocalName(name))
      specializedName(name.getterName, types1, types2).localName
    else {
      val (base, cs, ms) = nme.splitSpecializedName(name)
      newTermName(base.toString + "$"
                  + "m" + ms + types1.map(t => abbrvTag(t.typeSymbol)).mkString("", "", "")
                  + "c" + cs + types2.map(t => abbrvTag(t.typeSymbol)).mkString("", "", "$sp"))
    }
  )

  private lazy val specializableTypes = ScalaValueClasses.map(_.tpe).sorted

  /** If the symbol is the companion of a value class, the value class.
   *  Otherwise, AnyRef.
   */
  def specializesClass(sym: Symbol): Symbol = {
    val c = sym.companionClass
    if (isPrimitiveValueClass(c)) c else AnyRefClass
  }

  /** Return the types `sym` should be specialized at. This may be some of the primitive types
   *  or AnyRef. AnyRef means that a new type parameter T will be generated later, known to be a
   *  subtype of AnyRef (T <: AnyRef).
   *  These are in a meaningful order for stability purposes.
   */
  def concreteTypes(sym: Symbol): List[Type] = {
    val types = if (!sym.isSpecialized)
      Nil // no @specialized Annotation
    else
      specializedOn(sym).map(s => specializesClass(s).tpe).sorted

    if (isBoundedGeneric(sym.tpe) && (types contains AnyRefTpe))
      runReporting.warning(sym.pos, s"$sym is always a subtype of $AnyRefTpe.", WarningCategory.Other, sym)

    types
  }

  /** Return a list of all type environments for all specializations
   *  of @specialized types in `tps`.
   */
  private def specializations(tps: List[Symbol]): List[TypeEnv] = {
    // the keys in each TypeEnv
    val keys: List[Symbol] = tps filter (_.isSpecialized)
    // creating each permutation of concrete types
    def loop(ctypes: List[List[Type]]): List[List[Type]] = ctypes match {
      case Nil         => Nil
      case set :: Nil  => set map (_ :: Nil)
      case set :: sets => for (x <- set ; xs <- loop(sets)) yield x :: xs
    }
    // zip the keys with each permutation to create a TypeEnv.
    // If we don't exclude the "all AnyRef" specialization, we will
    // incur duplicate members and crash during mixin.
    loop(keys map concreteTypes) filterNot (_ forall (_ <:< AnyRefTpe)) map (xss => Map(keys zip xss: _*))
  }

  /** Does the given 'sym' need to be specialized in the environment 'env'?
   *  Specialization is needed for
   *    - members with specialized type parameters found in the given environment
   *    - constructors of specialized classes
   *    - normalized members whose type bounds appear in the environment
   *  But suppressed for:
   *    - any member with the @unspecialized annotation, or which has an
   *      enclosing member with the annotation.
   */
  private def needsSpecialization(env: TypeEnv, sym: Symbol): Boolean = (
    !hasUnspecializableAnnotation(sym) && (
         specializedTypeVars(sym).intersect(env.keySet).diff(wasSpecializedForTypeVars(sym)).nonEmpty
      || sym.isClassConstructor && sym.enclClass.typeParams.exists(_.isSpecialized)
      || isNormalizedMember(sym) && info(sym).typeBoundsIn(env)
    )
  )

  private def hasUnspecializableAnnotation(sym: Symbol): Boolean =
    sym.ownersIterator.exists(_.hasAnnotation(UnspecializedClass))

  def isNormalizedMember(m: Symbol) = m.isSpecialized && info.get(m).exists {
    case NormalizedMember(_)  => true
    case _                    => false
  }
  def specializedTypeVars(tpes: List[Type]): Set[Symbol] = {
    val result = ListBuffer.empty[Symbol]
    tpes.foreach(specializedTypeVarsBuffer(_, result))
    result.toSet
  }
  def specializedTypeVars(sym: Symbol): Set[Symbol] = {
    val result = ListBuffer.empty[Symbol]
    specializedTypeVarsBuffer(sym, result)
    result.toSet
  }

  /** Return the set of @specialized type variables mentioned by the given type.
   *  It only counts type variables that appear:
   *    - naked
   *    - as arguments to type constructors in @specialized positions
   *      (arrays are considered as Array[@specialized T])
   */
  def specializedTypeVars(tpe: Type): Set[Symbol] = {
    val result = ListBuffer.empty[Symbol]
    specializedTypeVarsBuffer(tpe, result)
    result.toSet
  }

  def specializedTypeVarsBuffer(sym: Symbol, result: Buffer[Symbol]): Unit =
    if (!neverHasTypeParameters(sym))
      enteringTyper(specializedTypeVarsBuffer(sym.info, result))

  /** Return the set of @specialized type variables mentioned by the given type.
    *  It only counts type variables that appear:
    *    - naked
    *    - as arguments to type constructors in @specialized positions
    *      (arrays are considered as Array[@specialized T])
    */
  def specializedTypeVarsBuffer(tpe: Type, result: Buffer[Symbol]): Unit = tpe match {
    case TypeRef(pre, sym, args) =>
      if (sym.isAliasType)
        specializedTypeVarsBuffer(tpe.dealiasWiden, result)
      else if (sym.isTypeParameter && sym.isSpecialized || (sym.isTypeSkolem && sym.deSkolemize.isSpecialized))
        result += sym
      else if (sym == ArrayClass)
        args.foreach(tp => specializedTypeVarsBuffer(tp, result))
      else if (!args.isEmpty)
        enteringTyper {
          foreach2(sym.typeParams, args) { (tp, arg) =>
            if (tp.isSpecialized) {
              specializedTypeVarsBuffer(arg, result)
            } else if (sym == ValueOfClass) { // scala/bug#11489, we only update it for ValueOf 
              arg.typeSymbol.annotations.foreach {
                case lzai: LazyAnnotationInfo if lzai.symbol == SpecializedClass =>
                  specializedTypeVarsBuffer(arg, result)
                case _                                                           =>
              }
            }
          }
        }
    case PolyType(tparams, resTpe)   => specializedTypeVarsBuffer(resTpe, result);  tparams.foreach(sym => specializedTypeVarsBuffer(sym.info, result))
    // since this method may be run at phase typer (before uncurry, where NMTs are eliminated)
    case NullaryMethodType(resTpe)   => specializedTypeVarsBuffer(resTpe, result)
    case MethodType(argSyms, resTpe) => specializedTypeVarsBuffer(resTpe, result); argSyms.foreach(sym => specializedTypeVarsBuffer(sym.tpe, result))
    case ExistentialType(_, res)     => specializedTypeVarsBuffer(res, result)
    case AnnotatedType(_, tp)        => specializedTypeVarsBuffer(tp, result)
    case TypeBounds(lo, hi)          => specializedTypeVarsBuffer(lo, result); specializedTypeVarsBuffer(hi, result)
    case RefinedType(parents, _)     => parents.foreach(p => specializedTypeVarsBuffer(p, result))
    case _                           => ()
  }

  /** Returns the type parameter in the specialized class `sClass` that corresponds to type parameter
   *  `tparam` in the original class. It will create it if needed or use the one from the cache.
   */
  private def typeParamSubAnyRef(tparam: Symbol, sClass: Symbol): Type = {
    val sClassMap = anyrefSpecCache.getOrElseUpdate(sClass, mutable.Map[Symbol, Symbol]())

    sClassMap.getOrElseUpdate(tparam,
      tparam.cloneSymbol(sClass, tparam.flags, tparam.name append tpnme.SPECIALIZED_SUFFIX)
        modifyInfo (info => TypeBounds(info.lowerBound, AnyRefTpe))
    ).tpe
  }

  /** Cleans the anyrefSpecCache of all type parameter symbols of a class.
   */
  private def cleanAnyRefSpecCache(clazz: Symbol, decls: List[Symbol]): Unit = {
    // remove class type parameters and those of normalized members.
    clazz :: decls foreach (anyrefSpecCache remove _)
  }

  /** Type parameters that survive when specializing in the specified environment. */
  def survivingParams(params: List[Symbol], env: TypeEnv) =
    params filter {
      p =>
      !p.isSpecialized ||
      !env.contains(p) ||
      !isPrimitiveValueType(env(p))
    }

  /** Produces the symbols from type parameters `syms` of the original owner,
   *  in the given type environment `env`. The new owner is `nowner`.
   *
   *  Non-specialized type parameters are cloned into new ones.
   *  Type parameters specialized on AnyRef have preexisting symbols.
   *
   *  For instance, a @specialized(AnyRef) T, will become T\$sp <: AnyRef.
   */
  def produceTypeParameters(syms: List[Symbol], nowner: Symbol, env: TypeEnv) = {
    val cloned = for (s <- syms) yield if (!env.contains(s)) s.cloneSymbol(nowner) else env(s).typeSymbol
    // log("producing type params: " + cloned.map(t => (t, t.tpe.upperBound)))
    foreach2(syms, cloned) { (orig, cln) =>
      cln.removeAnnotation(SpecializedClass)
      if (env.contains(orig))
        cln modifyInfo (info => TypeBounds(info.lowerBound, AnyRefTpe))
    }
    cloned.foreach(_.substInfo(syms, cloned))
    cloned
  }

  /** Maps AnyRef bindings from a raw environment (holding AnyRefs) into type parameters from
   *  the specialized symbol (class (specialization) or member (normalization)), leaves everything else as-is.
   */
  private def mapAnyRefsInSpecSym(env: TypeEnv, origsym: Symbol, specsym: Symbol): TypeEnv = env transform {
    case (sym, AnyRefTpe) if sym.owner == origsym => typeParamSubAnyRef(sym, specsym)
    case (_, v) => v
  }

  /** Maps AnyRef bindings from a raw environment (holding AnyRefs) into type parameters from
   *  the original class, leaves everything else as-is.
   */
  private def mapAnyRefsInOrigCls(env: TypeEnv, origcls: Symbol): TypeEnv = env transform {
    case (sym, AnyRefTpe) if sym.owner == origcls => sym.tpe
    case (_, v) => v
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
    def toSpecializedClass(env0: TypeEnv, normMembers: List[Symbol]): Symbol = {
      /* It gets hard to follow all the clazz and cls, and specializedClass
       * was both already used for a map and mucho long.  So "sClass" is the
       * specialized subclass of "clazz" throughout this file.
       */
      val clazzName = specializedName(clazz, env0).toTypeName
      // scala/bug#5545: Eliminate classes with the same name loaded from the bytecode already present - all we need to do is
      // to force .info on them, as their lazy type will be evaluated and the symbols will be eliminated. Unfortunately
      // evaluating the info after creating the specialized class will mess the specialized class signature, so we'd
      // better unlink the class-file backed symbol before creating the new class symbol
      val bytecodeClazz = clazz.owner.info.decl(clazzName)
      // debuglog("Specializing " + clazz + ", but found " + bytecodeClazz + " already there")
      def unlink(sym: Symbol): Unit = if (sym != NoSymbol) {
        devWarningIf(sym.hasCompleteInfo)("Stale specialized symbol has been accessed: " + sym)
        sym.setInfo(NoType)
        sym.owner.info.decls.unlink(sym)
      }
      unlink(bytecodeClazz)
      val companionModule = bytecodeClazz.companionModule
      unlink(companionModule.moduleClass)
      unlink(companionModule)

      val sClass = {
        val sc = clazz.owner.newClass(clazzName, clazz.pos, (clazz.flags | SPECIALIZED) & ~CASE)
        sc.setAnnotations(clazz.annotations)
        sc
      }

      def cloneInSpecializedClass(member: Symbol, flagFn: Long => Long, newName: Name = null) =
        member.cloneSymbol(sClass, flagFn(member.flags | SPECIALIZED), newName)

      clazz.sourceFile match {
        case null =>
        case file =>
          sClass.associatedFile = file
          currentRun.symSource(sClass) = file // needed later on by mixin
      }

      val env = mapAnyRefsInSpecSym(env0, clazz, sClass)
      typeEnv(sClass) = env
      specializedClass.getOrElseUpdate(clazz, HashMap.empty).update(env0, sClass)

      val decls1                        = newScope  // declarations of the newly specialized class 'sClass'
      var oldClassTParams: List[Symbol] = Nil       // original unspecialized type parameters
      var newClassTParams: List[Symbol] = Nil       // unspecialized type parameters of 'specializedClass' (cloned)

      // has to be a val in order to be computed early. It is later called
      // within 'enteringPhase(next)', which would lead to an infinite cycle otherwise
      val specializedInfoType: Type = {
        oldClassTParams = survivingParams(clazz.info.typeParams, env)
        newClassTParams = produceTypeParameters(oldClassTParams, sClass, env) map subst(env)
        // log("new tparams " + newClassTParams.zip(newClassTParams map {s => (s.tpe, s.tpe.upperBound)}) + ", in env: " + env)

        def applyContext(tpe: Type) =
          subst(env, tpe).instantiateTypeParams(oldClassTParams, newClassTParams.map(_.tpeHK))

        /* Return a list of specialized parents to be re-mixed in a specialized subclass.
         * Assuming env = [T -> Int] and
         *   class Integral[@specialized T] extends Numeric[T]
         * and Numeric[U] is specialized on U, this produces List(Numeric$mcI).
         *
         * so that class Integral$mci extends Integral[Int] with Numeric$mcI.
         */
        def specializedParents(parents: List[Type]): List[Type] = {
          var res: List[Type] = Nil
          // log(specializedClass + ": seeking specialized parents of class with parents: " + parents.map(_.typeSymbol))
          for (p <- parents) {
            val stp = exitingSpecialize(specializedType(p))
            if (stp != p)
              if (p.typeSymbol.isTrait) res ::= stp
              else if (currentRun.compiles(clazz))
                // TODO change to error
                runReporting.warning(clazz.pos,
                  s"${p.typeSymbol} must be a trait. Specialized version of $clazz will inherit generic $p",
                  WarningCategory.Other,
                  clazz)
          }
          res
        }

        var parents = List(applyContext(enteringTyper(clazz.tpe_*)))
        // log("!!! Parents: " + parents + ", sym: " + parents.map(_.typeSymbol))
        if (parents.head.typeSymbol.isTrait)
          parents = parents.head.parents.head :: parents
        val extraSpecializedMixins = specializedParents(clazz.info.parents map applyContext)
        if (extraSpecializedMixins.nonEmpty)
          debuglog("extra specialized mixins for %s: %s".format(clazz.name.decode, extraSpecializedMixins.mkString(", ")))
        // If the class being specialized has a self-type, the self type may
        // require specialization.  First exclude classes whose self types have
        // the same type constructor as the class itself, since they will
        // already be covered.  Then apply the current context to the self-type
        // as with the parents and assign it to typeOfThis.
        if (clazz.typeOfThis.typeConstructor ne clazz.typeConstructor) {
          sClass.typeOfThis = applyContext(clazz.typeOfThis)
          debuglog("Rewriting self-type for specialized class:\n" +
              "    " +  clazz.defStringSeenAs(clazz.typeOfThis) + "\n" +
              " => " + sClass.defStringSeenAs(sClass.typeOfThis)
          )
        }
        GenPolyType(newClassTParams, ClassInfoType(parents ::: extraSpecializedMixins, decls1, sClass))
      }

      exitingSpecialize(sClass setInfo specializedInfoType)
      val fullEnv = outerEnv ++ env

      /* Enter 'sym' in the scope of the current specialized class. Its type is
       * mapped through the active environment, binding type variables to concrete
       * types. The existing typeEnv for `sym` is composed with the current active
       * environment
       */
      def enterMember(sym: Symbol): Symbol = {
        typeEnv(sym) = fullEnv ++ typeEnv(sym) // append the full environment
        sym.modifyInfo(_.substThis(clazz, sClass).instantiateTypeParams(oldClassTParams, newClassTParams.map(_.tpeHK)))
        // we remove any default parameters. At this point, they have been all
        // resolved by the type checker. Later on, erasure re-typechecks everything and
        // chokes if it finds default parameters for specialized members, even though
        // they are never needed.
        foreachParamss(sym)(_ resetFlag DEFAULTPARAM)
        decls1 enter subst(fullEnv)(sym)
      }

      /* Create and enter in scope an overridden symbol m1 for `m` that forwards
       * to `om`. `om` is a fresh, special overload of m1 that is an implementation
       * of `m`. For example, for a
       *
       * class Foo[@specialized A] {
       *   def m(x: A) = <body> // m
       * }
       * , for class Foo$I extends Foo[Int], this method enters two new symbols in
       * the scope of Foo$I:
       *
       *   def m(x: Int) = m$I(x) // m1
       *   def m$I(x: Int) = <body>/adapted to env {A -> Int} // om
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

        newOverload(specMember, om, typeEnv(om))
        enterMember(om)
      }

      @tailrec def isTraitValSetter(sym: Symbol): Boolean =
        sym.isSetter && sym.getterIn(sym.owner).isStable &&
          (sym.hasFlag(SYNTHESIZE_IMPL_IN_SUBCLASS) || isTraitValSetter(sym.nextOverriddenSymbol))

      for (m <- normMembers) {
        if (!needsSpecialization(fullEnv, m)) {
          if (m.isValue && !m.isMutable && !m.isMethod && !m.isDeferred && !m.isLazy && !m.isParamAccessor) {
            // non-specialized `val` fields are made mutable (in Constructors) and assigned from the
            // constructors of specialized subclasses. See PR scala/scala#9704.
            clazz.primaryConstructor.updateAttachment(ConstructorNeedsFence)
            sClass.primaryConstructor.updateAttachment(ConstructorNeedsFence)
          }
        } else if (satisfiable(fullEnv)) {
          if (!m.isDeferred)
            addConcreteSpecMethod(m)
          // specialized members have to be overridable.
          if (m.isPrivate)
            m.resetFlag(PRIVATE).setFlag(PROTECTED)

          if (m.isConstructor) {
            val specCtor = enterMember(cloneInSpecializedClass(m, x => x))
            info(specCtor) = Forward(m)
          }
          else if (isNormalizedMember(m)) { // methods added by normalization
            val NormalizedMember(original) = info(m): @unchecked
            if (nonConflicting(env ++ typeEnv(m))) {
              if (info(m).degenerate) {
                debuglog("degenerate normalized member " + m.defString)
                val specMember = enterMember(cloneInSpecializedClass(m, _ & ~DEFERRED))

                info(specMember) = Implementation(original)
                typeEnv(specMember) = env ++ typeEnv(m)
              } else {
                val om = forwardToOverload(m)
                debuglog("normalizedMember " + m + " om: " + om + " " + pp(typeEnv(om)))
              }
            }
            else
              debuglog("conflicting env for " + m + " env: " + env)
          }
          else if (m.isDeferred && m.isSpecialized) { // abstract methods
            val specMember = enterMember(cloneInSpecializedClass(m, _ | DEFERRED))
            // debuglog("deferred " + specMember.fullName + " remains abstract")

            info(specMember) = Abstract(specMember)
            // was: new Forward(specMember) {
            //   override def target = m.owner.info.member(specializedName(m, env))
            // }
          } else if (m.hasFlag(SUPERACCESSOR)) { // basically same as abstract case
            // we don't emit a specialized overload for the super accessor because we can't jump back and forth
            // between specialized and non-specialized methods during an invokespecial for the super call,
            // so, we must jump immediately into the non-specialized world to find our super
            val specMember = enterMember(cloneInSpecializedClass(m, f => f))

            // rebindSuper in mixins knows how to rejigger this
            // (basically it skips this specialized class in the base class seq, and then also never rebinds to a specialized method)
            specMember.asInstanceOf[TermSymbol].referenced = m.alias

            info(specMember) = SpecialSuperAccessor(specMember)
          } else if (m.isMethod && !m.isDeferred && (!m.isAccessor || m.isLazy || isTraitValSetter(m))) { // other concrete methods
            forwardToOverload(m)
          } else if (m.isValue && !m.isMethod) { // concrete value definition
            def mkAccessor(field: Symbol, name: Name) = {
              val newFlags = (SPECIALIZED | m.getterIn(clazz).flags) & ~(LOCAL | CASEACCESSOR | PARAMACCESSOR)
              // we rely on the super class to initialize param accessors
              val sym = sClass.newMethod(name.toTermName, field.pos, newFlags)
              info(sym) = SpecializedAccessor(field)
              sym
            }

            def overrideIn(clazz: Symbol, sym: Symbol) = {
              val newFlags = (sym.flags | OVERRIDE | SPECIALIZED) & ~(DEFERRED | CASEACCESSOR | PARAMACCESSOR)
              val sym1 = sym.cloneSymbol(clazz, newFlags)
              sym1.modifyInfo(_.asSeenFrom(clazz.tpe, sym1.owner))
            }

            val specVal = specializedOverload(sClass, m, env)

            addConcreteSpecMethod(m)
            specVal.asInstanceOf[TermSymbol].setAlias(m)

            enterMember(specVal)
            // create accessors

            if (m.isLazy) {
              // no getters needed (we'll specialize the compute method and accessor separately), can stay private
              // m.setFlag(PRIVATE) -- TODO: figure out how to leave the non-specialized lazy var private
              // (the implementation needs it to be visible while duplicating and retypechecking,
              //  but it really could be private in bytecode)
              specVal.setFlag(PRIVATE)
            }
            else if (nme.isLocalName(m.name)) {
              val specGetter = mkAccessor(specVal, specVal.getterName) setInfo MethodType(Nil, specVal.info)
              val origGetter = overrideIn(sClass, m.getterIn(clazz))
              info(origGetter) = Forward(specGetter)
              enterMember(specGetter)
              enterMember(origGetter)
              debuglog(s"specialize accessor in ${sClass.name.decode}: ${origGetter.name.decode} -> ${specGetter.name.decode}")

              clazz.caseFieldAccessors.find(_.name.startsWith(m.name)) foreach { cfa =>
                val cfaGetter = overrideIn(sClass, cfa)
                info(cfaGetter) = SpecializedAccessor(specVal)
                enterMember(cfaGetter)
                debuglog("override case field accessor %s -> %s".format(m.name.decode, cfaGetter.name.decode))
              }

              if (specVal.isVariable && m.setterIn(clazz) != NoSymbol) {
                val specSetter = mkAccessor(specVal, specGetter.setterName)
                  .resetFlag(STABLE)
                specSetter.setInfo(MethodType(specSetter.newSyntheticValueParams(List(specVal.info)),
                  UnitTpe))
                val origSetter = overrideIn(sClass, m.setterIn(clazz))
                info(origSetter) = Forward(specSetter)
                enterMember(specSetter)
                enterMember(origSetter)
              }
            }
            else { // if there are no accessors, specialized methods will need to access this field in specialized subclasses
              m.resetFlag(PRIVATE)
              specVal.resetFlag(PRIVATE)
              debuglog("no accessors for %s/%s, specialized methods must access field in subclass".format(
                m.name.decode, specVal.name.decode))
            }
          }
          else if (m.isClass) {
            val specClass: Symbol = cloneInSpecializedClass(m, x => x)
            typeEnv(specClass) = fullEnv
            specClass setName specializedName(specClass, fullEnv).toTypeName
            enterMember(specClass)
            debuglog("entered specialized class " + specClass.fullName)
            info(specClass) = SpecializedInnerClass(m, fullEnv)
          }
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

          if (normalizedMember.isMethod && !normalizedMember.isScala3Defined) {
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

    val subclasses = specializations(clazz.info.typeParams).filter(satisfiable(_))
    subclasses foreach {
      env =>
      val spc      = toSpecializedClass(env, decls1)
      val existing = clazz.owner.info.decl(spc.name)

      // a symbol for the specialized class already exists if there's a classfile for it.
      // keeping both crashes the compiler on test/files/pos/spec-Function1.scala
      if (existing != NoSymbol)
        clazz.owner.info.decls.unlink(existing)

      exitingSpecialize(clazz.owner.info.decls enter spc) //!!! assumes fully specialized classes
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
   *     m\$I[ U](x: Int, y: U),
   *     m\$D[ U](x: Double, y: U)
   *     // etc.
   */
  private def normalizeMember(owner: Symbol, sym: Symbol, outerEnv: TypeEnv): List[Symbol] = {
    sym :: (
      if (!sym.isMethod || sym.isScala3Defined || enteringTyper(sym.typeParams.isEmpty)) Nil
      else if (sym.hasDefault) {
        /* Specializing default getters is useless, also see scala/bug#7329 . */
        sym.resetFlag(SPECIALIZED)
        Nil
      } else {
        // debuglog("normalizeMember: " + sym.fullNameAsName('.').decode)
        var specializingOn = specializedParams(sym)
        val unusedStvars   = specializingOn filterNot specializedTypeVars(sym.info)

        // I think the last condition should be !sym.isArtifact, but that made the
        // compiler start warning about Tuple1.scala and Tuple2.scala claiming
        // their type parameters are used in non-specializable positions.  Why is
        // unusedStvars.nonEmpty for these classes???
        if (unusedStvars.nonEmpty && currentRun.compiles(sym) && !sym.isSynthetic) {
          runReporting.warning(sym.pos,
            "%s %s unused or used in non-specializable positions.".format(
              unusedStvars.mkString("", ", ", ""),
              if (unusedStvars.lengthIs == 1) "is" else "are"),
            WarningCategory.Other,
            sym)
          unusedStvars foreach (_ removeAnnotation SpecializedClass)
          specializingOn = specializingOn filterNot (unusedStvars contains _)
        }
        for (env0 <- specializations(specializingOn) if needsSpecialization(env0, sym)) yield {
          // !!! Can't this logic be structured so that the new symbol's name is
          // known when the symbol is cloned? It is much cleaner not to be mutating
          // names after the fact.  And it adds about a billion lines of
          // "Renaming value _1 in class Tuple2 to _1$mcZ$sp" to obscure the small
          // number of other (important) actual symbol renamings.
          val tps          = survivingParams(sym.info.typeParams, env0)
          val specMember   = sym.cloneSymbol(owner, (sym.flags | SPECIALIZED) & ~DEFERRED)  // <-- this needs newName = ...
          val env          = mapAnyRefsInSpecSym(env0, sym, specMember)
          val (keys, vals) = env.toList.unzip

          specMember setName specializedName(sym, env)  // <-- but the name is calculated based on the cloned symbol
          // debuglog("%s normalizes to %s%s".format(sym, specMember,
          //   if (tps.isEmpty) "" else " with params " + tps.mkString(", ")))

          typeEnv(specMember) = outerEnv ++ env
          val tps1 = produceTypeParameters(tps, specMember, env)
          tps1 foreach (_ modifyInfo (_.instantiateTypeParams(keys, vals)))

          // the cloneInfo is necessary so that method parameter symbols are cloned at the new owner
          val methodType = sym.info.resultType.instantiateTypeParams(keys ++ tps, vals ++ tps1.map(_.tpeHK)).cloneInfo(specMember)
          specMember setInfo GenPolyType(tps1, methodType)

          debuglog("%s expands to %s in %s".format(sym, specMember.name.decode, pp(env)))
          info(specMember) = NormalizedMember(sym)
          newOverload(sym, specMember, env)
          specMember
        }
      }
    )
  }

  // concise printing of type env
  private def pp(env: TypeEnv): String = {
    env.toList.sortBy(_._1.name).map {
      case (k, v) =>
        val vsym = v.typeSymbol
        if (k == vsym) "" + k.name
        else "" + k.name + ":" + vsym.name
    }.mkString("env(", ", ", ")")
  }

  /** Specialize member `m` w.r.t. to the outer environment and the type
   *  parameters of the innermost enclosing class.
   *
   *  Turns 'private' into 'protected' for members that need specialization.
   *
   *  Return a list of symbols that are specializations of 'sym', owned by 'owner'.
   */
  private def specializeMember(owner: Symbol, sym: Symbol, outerEnv: TypeEnv, tps: List[Symbol]): List[Symbol] = {
    def specializeOn(tparams: List[Symbol]): List[Symbol] = specializations(tparams) map { spec0 =>
      val spec = mapAnyRefsInOrigCls(spec0, owner)
      if (sym.isPrivate) {
        sym.resetFlag(PRIVATE).setFlag(PROTECTED)
        debuglog("Set %s to private[%s]".format(sym, sym.enclosingPackage))
      }

      val specMember = subst(outerEnv)(specializedOverload(owner, sym, spec))
      typeEnv(specMember) = typeEnv(sym) ++ outerEnv ++ spec
      wasSpecializedForTypeVars(specMember) ++= spec collect { case (s, tp) if s.tpe == tp => s }

      val wasSpec = wasSpecializedForTypeVars(specMember)
      if (wasSpec.nonEmpty)
        debuglog("specialized overload for %s in %s".format(specMember, pp(typeEnv(specMember))))

      newOverload(sym, specMember, spec)
      info(specMember) = SpecialOverload(sym, typeEnv(specMember))
      specMember
    }

    if (!sym.isMethod || sym.isConstructor || hasUnspecializableAnnotation(sym) || sym.isSuperAccessor
      || sym.isScala3Defined) { // Scala 3 does not have specialised methods yet.
    // ) {
      Nil
    } else {
      val stvars = specializedTypeVars(sym)
      if (stvars.nonEmpty)
        debuglog("specialized %s on %s".format(sym.fullLocationString, stvars.map(_.name).mkString(", ")))

      if (!sym.isDeferred)
        addConcreteSpecMethod(sym)

      specializeOn(tps filter stvars)
    }
  }

  /** Return the specialized overload of `m`, in the given environment. */
  private def specializedOverload(owner: Symbol, sym: Symbol, env: TypeEnv, nameSymbol: Symbol = NoSymbol): Symbol = {
    val newFlags = (sym.flags | SPECIALIZED) & ~(DEFERRED | CASEACCESSOR | LAZY)
    // this method properly duplicates the symbol's info
    val specname = specializedName(nameSymbol orElse sym, env)
    ( sym.cloneSymbol(owner, newFlags, newName = specname)
        modifyInfo (info => subst(env, info.asSeenFrom(owner.thisType, sym.owner)))
    )
  }

  /** For each method m that overrides an inherited method m', add a special
   *  overload method `om` that overrides the corresponding overload in the
   *  superclass. For the following example:
   *
   *  class IntFun extends Function1[Int, Int] {
   *    def apply(x: Int): Int = ..
   *  }
   *
   *  this method will return List('apply\$mcII\$sp')
   */
  private def specialOverrides(clazz: Symbol) = logResultIf[List[Symbol]]("specialized overrides in " + clazz, _.nonEmpty) {
    /* Return the overridden symbol in syms that needs a specialized overriding symbol,
     * together with its specialization environment. The overridden symbol may not be
     * the closest to 'overriding', in a given hierarchy.
     *
     * An method m needs a special override if
     *   * m overrides a method whose type contains specialized type variables
     *   * there is a valid specialization environment that maps the overridden method type to m's type.
     */
    @nowarn("cat=lint-nonlocal-return")
    def needsSpecialOverride(overriding: Symbol): (Symbol, TypeEnv) = {
      def checkOverriddenTParams(overridden: Symbol): Unit = {
        foreach2(overridden.info.typeParams, overriding.info.typeParams) { (baseTvar, derivedTvar) =>
          val missing = concreteTypes(baseTvar).toSet diff concreteTypes(derivedTvar).toSet
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
          val stvars = specializedTypeVars(overridden.info)
          if (stvars.nonEmpty) {
            debuglog("specialized override of %s by %s%s".format(overridden.fullLocationString, overriding.fullLocationString,
              if (stvars.isEmpty) "" else stvars.map(_.name).mkString("(", ", ", ")")))

            if (currentRun compiles overriding)
              checkOverriddenTParams(overridden)

            val env    = unify(overridden.info, overriding.info, emptyEnv, tparams = true)
            def atNext = exitingSpecialize(overridden.owner.info.decl(specializedName(overridden, env)))

            if (TypeEnv.restrict(env, stvars).nonEmpty && TypeEnv.isValid(env, overridden) && atNext != NoSymbol) {
              debuglog("  " + pp(env) + " found " + atNext)
              return (overridden, env)
            }
          }
        }
      }
      (NoSymbol, emptyEnv)
    }
    (clazz.info.decls flatMap { overriding =>
      needsSpecialOverride(overriding) match {
        case (NoSymbol, _)     =>
          // run/t4996.scala, see the amazing commit message in 9733f56
          if (overriding.isSuperAccessor) {
            val alias = overriding.alias
            debuglog(s"checking special overload for super accessor: ${overriding.fullName}, alias for ${alias.fullName}")
            needsSpecialOverride(alias) match {
              case (NoSymbol, _) => None
              case (overridden, env) =>
                val om = specializedOverload(clazz, overriding, env, overridden)
                om.setName(nme.superName(om.name))
                om.asInstanceOf[TermSymbol].setAlias(info(alias).target)
                om.owner.info.decls.enter(om)
                info(om) = SpecialSuperAccessor(om)
                om.makeNotPrivate(om.owner)
                newOverload(overriding, om, env)
                Some(om)
            }
          } else None
        case (overridden, env) =>
          val om = specializedOverload(clazz, overriding, env, overridden)
          clazz.info.decls.enter(om)
          foreachWithIndex(om.paramss) { (params, i) =>
            foreachWithIndex(params) { (param, j) =>
              param.name = overriding.paramss(i)(j).name // scala/bug#6555 Retain the parameter names from the subclass.
            }
          }
          debuglog(s"specialized overload $om for ${overriding.name.decode} in ${pp(env)}: ${om.info}")
          om.setFlag(overriding.flags & (ABSOVERRIDE | SYNCHRONIZED))
          om.withAnnotations(overriding.annotations.filter(_.symbol == ScalaStrictFPAttr))
          typeEnv(om) = env
          addConcreteSpecMethod(overriding)
          if (overriding.isDeferred) { // abstract override
            debuglog("abstract override " + overriding.fullName + " with specialized " + om.fullName)
            info(om) = Forward(overriding)
          }
          else {
            // if the override is a normalized member, 'om' gets the
            // implementation from its original target, and adds the
            // environment of the normalized member (that is, any
            // specialized /method/ type parameter bindings)
            info get overriding match {
              case Some(NormalizedMember(target)) =>
                typeEnv(om) = env ++ typeEnv(overriding)
                info(om) = Forward(target)
              case _ =>
                info(om) = SpecialOverride(overriding)
            }
            info(overriding) = Forward(om setPos overriding.pos)
          }

          newOverload(overriding, om, env)
          ifDebug(exitingSpecialize(assert(
            overridden.owner.info.decl(om.name) != NoSymbol,
            "Could not find " + om.name + " in " + overridden.owner.info.decls))
          )
          Some(om)
      }
    }).toList
  }

  case object UnifyError extends scala.util.control.ControlThrowable
  private[this] def unifyError(tp1: Any, tp2: Any): Nothing = {
    log("unifyError" + ((tp1, tp2)))
    throw UnifyError
  }

  /** Return the most general type environment that specializes tp1 to tp2.
   *  It only allows binding of type parameters annotated with @specialized.
   *  Fails if such an environment cannot be found.
   *
   *  If `strict` is true, a UnifyError is thrown if unification is impossible.
   *
   *  If `tparams` is true, then the methods tries to unify over type params in polytypes as well.
   */
  private def unify(tp1: Type, tp2: Type, env: TypeEnv, strict: Boolean = false, tparams: Boolean = false): TypeEnv = (tp1, tp2) match {
    case (TypeRef(_, sym1, _), _) if sym1.isSpecialized =>
      debuglog(s"Unify $tp1, $tp2")
      if (isPrimitiveValueClass(tp2.typeSymbol) || isSpecializedAnyRefSubtype(tp2, sym1))
        env + ((sym1, tp2))
      else if (isSpecializedAnyRefSubtype(tp2, sym1))
        env + ((sym1, tp2))
      else if (strict)
        unifyError(tp1, tp2)
      else
        env
    case (TypeRef(_, _, args1), TypeRef(_, _, args2)) =>
      if (args1.nonEmpty || args2.nonEmpty)
        debuglog(s"Unify types $tp1 and $tp2")

      if (strict && args1.length != args2.length) unifyError(tp1, tp2)
      val e = unify(args1, args2, env, strict)
      if (e.nonEmpty) debuglog(s"unified to: $e")
      e
    case (TypeRef(_, sym1, _), _) if sym1.isTypeParameterOrSkolem =>
      env
    case (MethodType(params1, res1), MethodType(params2, res2)) =>
      if (strict && params1.length != params2.length) unifyError(tp1, tp2)
      debuglog(s"Unify methods $tp1 and $tp2")
      val env1 = unifyAux(res1, res2, env, strict)
      if (params1.isEmpty) env1
      else
        foldLeft2(params1, params2)(env1){ (e, p1, p2) => unifyAux(p1.tpe, p2.tpe, e, strict) }
    case (PolyType(tparams1, res1), PolyType(tparams2, res2)) =>
      debuglog(s"Unify polytypes $tp1 and $tp2")
      if (strict && tparams1.length != tparams2.length)
        unifyError(tp1, tp2)
      else if (tparams && tparams1.sizeCompare(tparams2) == 0) {
        val env1 = unifyAux(res1, res2, env, strict)
        if (tparams1.isEmpty) env1
        else
          foldLeft2(tparams1, tparams2)(env1){ (e, tp1, tp2) => unifyAux(tp1.info, tp2.info, e, strict) }
      }
      else
        unify(res1, res2, env, strict = strict, tparams = false)
    case (TypeBounds(lo1, hi1), TypeBounds(lo2, hi2)) =>
      val env1 = unifyAux(lo1, lo2, env, strict)
      unifyAux(hi1, hi2, env1, strict)
    case (PolyType(_, res), other)                    => unify(res, other, env, strict, tparams = false)
    case (ThisType(_), ThisType(_))                   => env
    case (_, SingleType(_, _))                        => unify(tp1, tp2.underlying, env, strict, tparams = false)
    case (SingleType(_, _), _)                        => unify(tp1.underlying, tp2, env, strict, tparams = false)
    case (ThisType(_), _)                             => unify(tp1.widen, tp2, env, strict, tparams = false)
    case (_, ThisType(_))                             => unify(tp1, tp2.widen, env, strict, tparams = false)
    case (RefinedType(_, _), RefinedType(_, _))       => env
    case (AnnotatedType(_, tp1), tp2)                 => unify(tp2, tp1, env, strict, tparams = false)
    case (ExistentialType(_, res1), _)                => unify(tp2, res1, env, strict, tparams = false)
    case _ =>
      debuglog(s"don't know how to unify $tp1 [${tp1.getClass}] with $tp2 [${tp2.getClass}]")
      env
  }

  private def unify(tp1: List[Type], tp2: List[Type], env: TypeEnv, strict: Boolean): TypeEnv =
    if (tp1.isEmpty || tp2.isEmpty) env
    else foldLeft2(tp1, tp2)(env) { (env, arg1, arg2) =>
      unifyAux(arg1, arg2, env, strict)
    }

  private def unifyAux(arg1: Type, arg2: Type, env: TypeEnv, strict: Boolean): TypeEnv =
    if (!strict) unify(arg1, arg2, env, strict, tparams = false)
    else {
      val nenv = unify(arg1, arg2, emptyEnv, strict, tparams = false)
      if (env.keySet.intersect(nenv.keySet).isEmpty) env ++ nenv
      else {
        debuglog(s"could not unify: u($arg1, $arg2) yields $nenv, env: $env")
        unifyError(arg1, arg2)
      }
    }

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
      if (decl.isConstructor) MethodType(subst(env, info).params, decl.owner.tpe_*)
      else subst(env, info)
    )

  private def unspecializableClass(tp: Type) = (
       isRepeatedParamType(tp)  // ???
    || tp.typeSymbol.isJavaDefined
    || tp.typeSymbol.isPackageClass
  )

  /** Type transformation. It is applied to all symbols, compiled or loaded.
   *  If it is a 'no-specialization' run, it is applied only to loaded symbols.
   */
  override def transformInfo(sym: Symbol, tpe: Type): Type = {
    if (settings.nospecialization.value && currentRun.compiles(sym)) {
      tpe
    } else tpe.resultType match {
      case cinfo @ ClassInfoType(parents, decls, clazz) if !unspecializableClass(cinfo) =>
        if (!currentRun.compiles(sym) && isPast(ownPhase)) {
          // Skip specialization info transform for third party classes that aren't referenced directly
          // from the tree or by the specialization info transform itself that are run up to the end of
          // the specialization phase.
          //
          // As a special case, we unconditionally specialize Function and Tuple classes above in `Phase#apply`
          // as the backend needs to know about these for code it inlines to enable box- and null-check elimination.
          tpe
        } else {
          val tparams = tpe.typeParams
          if (tparams.isEmpty)
            exitingSpecialize(parents map (_.typeSymbol.info))

          val parents1 = parents mapConserve specializedType
          if (parents ne parents1) {
            debuglog("specialization transforms %s%s parents to %s".format(
              if (tparams.nonEmpty) "(poly) " else "", clazz, parents1)
            )
          }
          val newScope = newScopeWith(specializeClass(clazz, typeEnv(clazz)) ++ specialOverrides(clazz): _*)
          // If tparams.isEmpty, this is just the ClassInfoType.
          GenPolyType(tparams, ClassInfoType(parents1, newScope, clazz))
        }
      case _ =>
        tpe
    }
  }

  /** Is any type variable in `env` conflicting with any if its type bounds, when
   *  type bindings in `env` are taken into account?
   *
   *  A conflicting type environment could still be satisfiable.
   */
  def nonConflicting(env: TypeEnv) = env forall { case (tvar, tpe) =>
    (subst(env, tvar.info.lowerBound) <:< tpe) && (tpe <:< subst(env, tvar.info.upperBound))
  }

  /** The type environment is sound w.r.t. to all type bounds or only soft
   *  conflicts appear. An environment is sound if all bindings are within
   *  the bounds of the given type variable. A soft conflict is a binding
   *  that does not fall within the bounds, but whose bounds contain
   *  type variables that are @specialized, (that could become satisfiable).
   */
  def satisfiable(env: TypeEnv, warnings: Boolean = false): Boolean = {
    def matches(tpe1: Type, tpe2: Type): Boolean = (tpe2 == AnyTpe) || { // opt for common case of unbounded type parameter
      val t1 = subst(env, tpe1)
      val t2 = subst(env, tpe2)
      ((t1 <:< t2)
        || specializedTypeVars(t1).nonEmpty
        || specializedTypeVars(t2).nonEmpty)
     }

    env forall { case (tvar, tpe) =>
      matches(tvar.info.lowerBound, tpe) && matches(tpe, tvar.info.upperBound) || {
        if (warnings)
          runReporting.warning(tvar.pos, s"Bounds prevent specialization of $tvar", WarningCategory.Other, tvar)

        debuglog("specvars: " +
          tvar.info.lowerBound + ": " +
          specializedTypeVars(tvar.info.lowerBound) + " " +
          subst(env, tvar.info.upperBound) + ": " +
          specializedTypeVars(subst(env, tvar.info.upperBound))
        )
        false
      }
    }
  }

  def satisfiabilityConstraints(env: TypeEnv): Option[TypeEnv] = {
    val noconstraints = Some(emptyEnv)
    def matches(tpe1: Type, tpe2: Type): Option[TypeEnv] = {
      val t1 = subst(env, tpe1)
      val t2 = subst(env, tpe2)
      // log("---------> " + tpe1 + " matches " + tpe2)
      // log(t1 + ", " + specializedTypeVars(t1))
      // log(t2 + ", " + specializedTypeVars(t2))
      // log("unify: " + unify(t1, t2, env, false, false) + " in " + env)
      if (t1 <:< t2) noconstraints
      else if (specializedTypeVars(t1).nonEmpty) Some(unify(t1, t2, env) -- env.keys)
      else if (specializedTypeVars(t2).nonEmpty) Some(unify(t2, t1, env) -- env.keys)
      else None
    }

    env.foldLeft[Option[TypeEnv]](noconstraints) {
      case (constraints, (tvar, tpe)) =>
        val loconstraints = matches(tvar.info.lowerBound, tpe)
        val hiconstraints = matches(tpe, tvar.info.upperBound)
        val allconstraints = for (c <- constraints; l <- loconstraints; h <- hiconstraints) yield c ++ l ++ h
        allconstraints
    }
  }

  /** This duplicator additionally performs casts of expressions if that is allowed by the `casts` map. */
  class Duplicator(casts: Map[Symbol, Type]) extends {
    val global: SpecializeTypes.this.global.type = SpecializeTypes.this.global
  } with typechecker.Duplicators {
    private val (castfrom, castto) = casts.unzip
    private object CastMap extends SubstTypeMap(castfrom.toList, castto.toList)

    @nowarn("""cat=deprecation&origin=scala\.tools\.nsc\.transform\.SpecializeTypes\.Duplicator\.BodyDuplicator""")
    final type SpecializeBodyDuplicator = BodyDuplicator

    @nowarn("msg=shadowing a nested class of a parent is deprecated")
    @deprecated("use SpecializeBodyDuplicator instead", since = "2.13.4")
    class BodyDuplicator(_context: Context) extends super.BodyDuplicator(_context) {
      override def castType(tree: Tree, pt: Type): Tree = {
        tree modifyType fixType
        // log(" tree type: " + tree.tpe)
        val ntree = if (tree.tpe != null && !(tree.tpe <:< pt)) {
          val casttpe = CastMap(tree.tpe)
          if (casttpe <:< pt) gen.mkCast(tree, casttpe)
          else if (casttpe <:< CastMap(pt)) gen.mkCast(tree, pt)
          else tree
        } else tree

        ntree.clearType()
      }
    }

    private class SpecializeNamer(context: Context) extends Namer(context) {
      // Avoid entering synthetic trees during specialization because the duplicated trees already contain them.
      override def enterSyntheticSym(tree: Tree): Symbol = tree.symbol
    }

    override protected def newBodyDuplicator(context: Context): SpecializeBodyDuplicator =
      new SpecializeBodyDuplicator(context)

    override def newNamer(context: Context): Namer =
      new SpecializeNamer(context)
  }

  /** Introduced to fix scala/bug#7343: Phase ordering problem between Duplicators and Specialization.
   * brief explanation: specialization rewires class parents during info transformation, and
   * the new info then guides the tree changes. But if a symbol is created during duplication,
   * which runs after specialization, its info is not visited and thus the corresponding tree
   * is not specialized. One manifestation is the following:
   * {{{
   * object Test {
   *   class Parent[@specialized(Int) T]
   *
   *   def spec_method[@specialized(Int) T](t: T, expectedXSuper: String) = {
   *     class X extends Parent[T]()
   *     // even in the specialized variant, the local X class
   *     // doesn't extend Parent\$mcI\$sp, since its symbol has
   *     // been created after specialization and was not seen
   *     // by specialization's info transformer.
   *     ...
   *   }
   * }
   * }}}
   * We fix this by forcing duplication to take place before specialization.
   *
   * Note: The constructors phase (which also uses duplication) comes after erasure and uses the
   * post-erasure typer => we must protect it from the beforeSpecialization phase shifting.
   */
  class SpecializationDuplicator(casts: Map[Symbol, Type]) extends Duplicator(casts) {
    override def retyped(context: Context, tree: Tree, oldThis: Symbol, newThis: Symbol, env: scala.collection.Map[Symbol, Type]): Tree =
      enteringSpecialize(super.retyped(context, tree, oldThis, newThis, env))

  }

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
      if (currentOwner.isAnonymousFunction) {
        if (inlineFunctionExpansion) devWarning("anonymous function made it to specialization even though inline expansion is set.")
        false
      }
      else (currentClass == sym.owner.enclClass) && (currentClass != targetClass)

    private def shouldMakePublic(sym: Symbol): Boolean =
      sym.hasFlag(PRIVATE | PROTECTED) && (addressFields || !nme.isLocalName(sym.name))

    /** All private members that are referenced are made protected,
     *  in order to be accessible from specialized subclasses.
     */
    override def transform(tree: Tree): Tree = tree match {
      case Select(_, _) =>
        val sym = tree.symbol
        if (sym.isPrivate) debuglog(
          "seeing private member %s, currentClass: %s, owner: %s, isAccessible: %b, isLocalName: %b".format(
            sym, currentClass, sym.owner.enclClass, isAccessible(sym), nme.isLocalName(sym.name))
          )
        if (shouldMakePublic(sym) && !isAccessible(sym)) {
          debuglog(s"changing private flag of $sym")
          sym.makeNotPrivate(sym.owner)
        }
        super.transform(tree)

      case _ =>
        super.transform(tree)
    }
  }

  /** Return the generic class corresponding to this specialized class. */
  def originalClass(clazz: Symbol): Symbol =
    if (clazz.isSpecialized) {
      val (originalName, _, _) = nme.splitSpecializedName(clazz.name)
      clazz.owner.info.decl(originalName).suchThat(_.isClass)
    } else NoSymbol

  def illegalSpecializedInheritance(clazz: Symbol): Boolean = (
       clazz.isSpecialized
    && originalClass(clazz).parentSymbolsIterator.exists(p => hasSpecializedParams(p) && !p.isTrait)
  )

  class SpecializationTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transformUnit(unit: CompilationUnit): Unit = if (!settings.nospecialization.value) {
      informProgress("specializing " + unit)
      try {
        exitingSpecialize(super.transformUnit(unit))
      } catch {
        case te: TypeError =>
          reporter.error(te.pos, te.msg)
      }
    }

    /** Map a specializable method to its rhs, when not deferred. */
    val body = HashMap.empty[Symbol, Tree]

    /** Map a specializable method to its value parameter symbols. */
    val parameters = HashMap.empty[Symbol, List[Symbol]]

    /** Collect method bodies that are concrete specialized methods.
     */
    class CollectMethodBodies extends InternalTraverser {
      override def traverse(tree: Tree) = tree match {
        case DefDef(_, _, _, vparams :: Nil, _, rhs) =>
          if (concreteSpecMethods(tree.symbol) || tree.symbol.isConstructor) {
            // debuglog("!!! adding body of a defdef %s, symbol %s: %s".format(tree, tree.symbol, rhs))
            body(tree.symbol) = rhs
            //          body(tree.symbol) = tree // whole method
            parameters(tree.symbol) = vparams.map(_.symbol)
            concreteSpecMethods -= tree.symbol
          } // no need to descend further down inside method bodies

        case ValDef(mods, name, tpt, rhs) if concreteSpecMethods(tree.symbol) =>
          body(tree.symbol) = rhs
          // log("!!! adding body of a valdef " + tree.symbol + ": " + rhs)
          //super.traverse(tree)
        case _ =>
          tree.traverse(this)
      }
    }

    def doesConform(origSymbol: Symbol, treeType: Type, memberType: Type, env: TypeEnv) = {
      (treeType =:= memberType) || { // anyref specialization
        memberType match {
          case PolyType(_, resTpe) =>
            debuglog(s"Conformance for anyref - polytype with result type: $resTpe and $treeType\nOrig. sym.: $origSymbol")
            try {
              val e = unify(origSymbol.tpe, memberType, emptyEnv, strict = true, tparams = false)
              debuglog(s"obtained env: $e")
              e.keySet == env.keySet
            } catch {
              case _: Throwable =>
                debuglog("Could not unify.")
                false
            }
          case _ => false
        }
      }
    }

    override def transform(tree: Tree): Tree = {
      val symbol = tree.symbol
      /* The specialized symbol of 'tree.symbol' for tree.tpe, if there is one */
      def specSym(qual: Tree): Symbol = {
        val env = unify(symbol.tpe, tree.tpe, emptyEnv, strict = false, tparams = false)
        def isMatch(member: Symbol) = {
          val memberType = qual.tpe memberType member

          val residualTreeType = tree match {
            case TypeApply(fun, _) if fun.symbol == symbol =>
              // scala/bug#6308 Handle methods with only some type parameters specialized.
              //         drop the specialized type parameters from the PolyType, and
              //         substitute in the type environment.
              val GenPolyType(tparams, tpe) = fun.tpe
              val (from, to) = env.toList.unzip
              val residualTParams = tparams.filterNot(env.contains)
              GenPolyType(residualTParams, tpe).substituteTypes(from, to)
            case _ => tree.tpe
          }

          (
               doesConform(symbol, residualTreeType, memberType, env)
            && TypeEnv.includes(typeEnv(member), env)
          )
        }
        if (env.isEmpty) NoSymbol
        else qual.tpe member specializedName(symbol, env) suchThat isMatch
      }

      def matchingSymbolInPrefix(pre: Type, member: Symbol, env: TypeEnv): Symbol = {
        pre member specializedName(member, env) suchThat (_.tpe matches subst(env, member.tpe))
      }

      def transformSelect(sel: Select) = {
        val Select(qual, name) = sel
        debuglog(s"specializing Select(sym=${symbol.defString}, tree.tpe=${tree.tpe})")

        val qual1                     = transform(qual)
        def copySelect                = treeCopy.Select(tree, qual1, name)
        def newSelect(member: Symbol) = atPos(tree.pos)(Select(qual1, member))
        def typedOp(member: Symbol)   = localTyper typedOperator newSelect(member)
        def typedTree(member: Symbol) = localTyper typed newSelect(member)

        val ignoreEnv = specializedTypeVars(symbol.info).isEmpty || name == nme.CONSTRUCTOR
        if (ignoreEnv) overloads(symbol) find (_ matchesSym symbol) match {
          case Some(Overload(member, _)) => typedOp(member)
          case _                         => copySelect
        }
        else {
          val env = unify(symbol.tpe, tree.tpe, emptyEnv, strict = false, tparams = false)
          overloads(symbol) find (_ matchesEnv env) match {
            case Some(Overload(member, _)) => typedOp(member)
            case _ =>
              matchingSymbolInPrefix(qual1.tpe, symbol, env) match {
                case NoSymbol                  => copySelect
                case member if member.isMethod => typedOp(member)
                case member                    => typedTree(member)
              }
          }
        }
      }

      /* Computes residual type parameters after rewiring, like "String" in the following example:
       * {{{
       *   def specMe[@specialized T, U](t: T, u: U) = ???
       *   specMe[Int, String](1, "2") => specMe\$mIc\$sp[String](1, "2")
       * }}}
       */
      def computeResidualTypeVars(@unused baseTree: Tree, specMember: Symbol, specTree: Tree, baseTargs: List[Tree], env: TypeEnv): Tree = {
        val residualTargs = symbol.info.typeParams zip baseTargs collect {
          case (tvar, targ) if !env.contains(tvar) || !isPrimitiveValueClass(env(tvar).typeSymbol) => targ
        }
        ifDebug(assert(residualTargs.sizeCompare(specMember.info.typeParams) == 0,
          "residual: %s, tparams: %s, env: %s".format(residualTargs, specMember.info.typeParams, env))
        )

        val tree1 = gen.mkTypeApply(specTree, residualTargs)
        debuglog(s"rewrote $tree to $tree1")
        localTyper.typedOperator(atPos(tree.pos)(tree1)) // being polymorphic, it must be a method
      }

      curTree = tree
      tree match {
        case Apply(Select(New(tpt), nme.CONSTRUCTOR), args) =>
          // OPT: avoid ObjectRef due to capture of patmat var in by-name expression
          val tpt1 = tpt
          val args1 = args
          debuglog(s"Attempting to specialize new $tpt1(${args1.mkString(", ")})")

          val found = specializedType(tpt.tpe)
          if (found.typeSymbol ne tpt.tpe.typeSymbol) { // the ctor can be specialized
            val inst = New(found, transformTrees(args): _*)
            localTyper.typedPos(tree.pos)(inst)
          }
          else
            super.transform(tree)

        case Apply(sel @ Select(sup @ Super(qual, name), name1), args) if hasNewParents(sup) =>
          val sup1  = Super(qual, name) setPos sup.pos
          val tree1 = Apply(Select(sup1, name1) setPos sel.pos, transformTrees(args))
          val res   = localTyper.typedPos(tree.pos)(tree1)
          debuglog(s"retyping call to super, from: $symbol to ${res.symbol}")
          res

        // This rewires calls to specialized methods defined in a class (which have a receiver)
        // class C {
        //   def foo[@specialized T](t: T): T = t
        //   C.this.foo(3) // TypeApply(Select(This(C), foo), List(Int)) => C.this.foo$mIc$sp(3)
        // }
        case TypeApply(sel @ Select(qual, name), targs)
                if (specializedTypeVars(symbol.info).nonEmpty && name != nme.CONSTRUCTOR) =>
          debuglog("checking typeapp for rerouting: " + tree + " with sym.tpe: " + symbol.tpe + " tree.tpe: " + tree.tpe)
          val qual1 = transform(qual)
          log(">>> TypeApply: " + tree + ", qual1: " + qual1)
          specSym(qual1) match {
            case NoSymbol =>
              // See pos/exponential-spec.scala - can't call transform on the whole tree again.
              treeCopy.TypeApply(tree, treeCopy.Select(sel, qual1, name), transformTrees(targs))
            case specMember =>
              debuglog("found " + specMember.fullName)
              val targs1 = targs // OPT: avoid ObjectRef due to capture of patmat var in by-name expression
              ifDebug(assert(symbol.info.typeParams.sizeCompare(targs1) == 0, "" + symbol.info.typeParams + " / " + targs))

              val env = typeEnv(specMember)
              computeResidualTypeVars(tree, specMember, gen.mkAttributedSelect(qual1, specMember), targs, env)
          }

        // This rewires calls to specialized methods defined in the local scope. For example:
        // def outerMethod = {
        //   def foo[@specialized T](t: T): T = t
        //   foo(3) // TypeApply(Ident(foo), List(Int)) => foo$mIc$sp(3)
        // }
        case TypeApply(Ident(name), targs) if name != nme.CONSTRUCTOR =>
          val env = unify(symbol.tpe, tree.tpe, emptyEnv, strict = false, tparams = false)
          if (env.isEmpty) super.transform(tree)
          else {
            overloads(symbol) find (_ matchesEnv env) match {
              case Some(Overload(specMember, _)) => computeResidualTypeVars(tree, specMember, Ident(specMember), targs, env)
              case _ => super.transform(tree)
            }
          }

        case Select(Super(_, _), _) if illegalSpecializedInheritance(currentClass) =>
          val pos = tree.pos
          debuglog(pos.source.file.name+":"+pos.line+": not specializing call to super inside illegal specialized inheritance class.\n" + pos.lineContent)
          tree

        case sel @ Select(_, _) =>
          transformSelect(sel)

        case PackageDef(pid, stats) =>
          tree.symbol.info // make sure specializations have been performed
          atOwner(tree, symbol) {
            val specMembers = implSpecClasses(stats) map localTyper.typed
            treeCopy.PackageDef(tree, pid, transformStats(stats ::: specMembers, symbol.moduleClass))
          }

        case Template(parents, self, body) =>
          def transformTemplate = {
            val specMembers = makeSpecializedMembers(tree.symbol.enclClass) ::: implSpecClasses(body).map(localTyper.typed)
            if (!symbol.isPackageClass)
              new CollectMethodBodies()(tree)
            // currentOwner.info.parents.map(tpe => TypeTree(tpe) setPos parents.head.pos)
            val parents1 = map2Conserve(parents, currentOwner.info.parents)((parent, tpe) =>
              parent match {
                case tt @ TypeTree() if tpe eq tt.tpe => tt
                case _ => TypeTree(tpe) setPos parent.pos
              }
            )
            treeCopy.Template(tree, parents1, self, atOwner(currentOwner)(transformTrees(body ::: specMembers)))
          }
          transformTemplate

        case ddef @ DefDef(_, _, _, _, _, _) if info.isDefinedAt(symbol) =>
        def transformDefDef(ddef: DefDef) = {
          val vparamss = ddef.vparamss
          if (symbol.isConstructor) {
            val t = atOwner(symbol)(forwardCtorCall(tree.pos, gen.mkSuperInitCall, vparamss, symbol.owner))
            def check(fwd: Tree): Unit = if (settings.unitSpecialization) {
              val Apply(_, args) = fwd: @unchecked
              args.zip(vparamss.flatten).find {
                case (arg, param) if (arg.tpe =:= UnitTpe) && param.symbol.name.endsWith(nme.SPECIALIZED_SUFFIX) =>
                  val msg = "Class parameter is specialized for type Unit. Consider using `@specialized(Specializable.Arg)` instead."
                  runReporting.warning(arg.pos, msg, WarningCategory.LintUnitSpecialization, param.symbol.owner)
                  true
                case _ => false
              }: Unit
            }
            if (symbol.isPrimaryConstructor)
              localTyper.typedPos(symbol.pos)(deriveDefDef(tree)(_ => Block(List(t), Literal(Constant(()))))).tap(_ => check(t))
            else // duplicate the original constructor
              duplicateBody(ddef, info(symbol).target)
          }
          else info(symbol) match {
            case Implementation(target) =>
              assert(body.isDefinedAt(target), "sym: " + symbol.fullName + " target: " + target.fullName)
              // we have an rhs, specialize it
              val tree1 = duplicateBody(ddef, target)
              debuglog("implementation: " + tree1)
              deriveDefDef(tree1)(transform)

            case NormalizedMember(target) =>
              logResult("constraints")(satisfiabilityConstraints(typeEnv(symbol))) match {
                case Some(constraint) if !target.isDeferred =>
                  // we have an rhs, specialize it
                  val tree1 = duplicateBody(ddef, target, constraint)
                  debuglog("implementation: " + tree1)
                  deriveDefDef(tree1)(transform)
                case _ =>
                  deriveDefDef(tree)(_ => localTyper typed gen.mkThrowNewRuntimeException("Fatal error in code generation: this should never be called."))
              }

            case SpecialOverride(target) =>
              assert(body.isDefinedAt(target), "sym: " + symbol.fullName + " target: " + target.fullName)
              //debuglog("moving implementation, body of target " + target + ": " + body(target))
              log("%s is param accessor? %b".format(ddef.symbol, ddef.symbol.isParamAccessor))
              // we have an rhs, specialize it
              val tree1 = addBody(ddef, target)
              (new ChangeOwnerTraverser(target, tree1.symbol))(tree1.rhs)
              debuglog("changed owners, now: " + tree1)
              deriveDefDef(tree1)(transform)

            case SpecialOverload(original, env) =>
              debuglog("completing specialized " + symbol.fullName + " calling " + original)
              debuglog("special overload " + original + " -> " + env)
              val t = DefDef(symbol, { vparamss: List[List[Symbol]] =>
                val fun = Apply(Select(This(symbol.owner), original),
                                makeArguments(original, vparamss.head))

                debuglog("inside defdef: " + symbol + "; type: " + symbol.tpe + "; owner: " + symbol.owner)
                gen.maybeMkAsInstanceOf(fun,
                  symbol.owner.thisType.memberType(symbol).finalResultType,
                  symbol.owner.thisType.memberType(original).finalResultType)
              })
              debuglog("created special overload tree " + t)
              debuglog("created " + t)
              localTyper.typed(t)

            case fwd @ Forward(_) =>
              debuglog("forward: " + fwd + ", " + ddef)
              val rhs1 = forwardCall(tree.pos, gen.mkAttributedRef(symbol.owner.thisType, fwd.target), vparamss)
              debuglog("-->d completed forwarder to specialized overload: " + fwd.target + ": " + rhs1)
              localTyper.typed(deriveDefDef(tree)(_ => rhs1))

            case SpecializedAccessor(target) =>
              val rhs1 = if (symbol.isGetter)
                gen.mkAttributedRef(target)
              else
                Assign(gen.mkAttributedRef(target), Ident(vparamss.head.head.symbol))
              debuglog("specialized accessor: " + target + " -> " + rhs1)
              localTyper.typed(deriveDefDef(tree)(_ => rhs1))

            case Abstract(targ) =>
              debuglog("abstract: " + targ)
              localTyper.typed(deriveDefDef(tree)(rhs => rhs))

            case SpecialSuperAccessor(_) => // same as abstract method
              debuglog(s"special super accessor: $tree with $symbol -> ${symbol.alias} in ${symbol.alias.owner} (in $currentClass)")
              localTyper.typed(deriveDefDef(tree)(rhs => rhs))

            case x @ SpecializedInnerClass(_, _) => throw new MatchError(x) // ?!?
          }
          } // end transformDefDef
          expandInnerNormalizedMembers(transformDefDef(ddef))

        case DefDef(_, _, _, _, _, _) =>
          val tree1 = expandInnerNormalizedMembers(tree)
          super.transform(tree1)

        case ValDef(_, _, _, _) if symbol.hasFlag(SPECIALIZED) && !symbol.isParamAccessor =>
          def transformValDef = {
          assert(body.isDefinedAt(symbol.alias), body)
          val tree1 = deriveValDef(tree)(_ => body(symbol.alias).duplicate)
          debuglog("now typing: " + tree1 + " in " + tree.symbol.owner.fullName)

          val d = new SpecializationDuplicator(emptyEnv)
          val newValDef = d.retyped(
            localTyper.context1.asInstanceOf[d.Context],
            tree1,
            symbol.alias.enclClass,
            symbol.enclClass,
            typeEnv(symbol.alias) ++ typeEnv(tree.symbol)
          )
          deriveValDef(newValDef)(transform)
          }
          transformValDef
        case _ =>
          super.transform(tree)
      }
    }

    /**
     * This performs method specialization inside a scope other than a {class, trait, object}: could be another method
     * or a value. This specialization is much simpler, since there is no need to record the new members in the class
     * signature, their signatures are only visible locally. It works according to the usual logic:
     *  - we use normalizeMember to create the specialized symbols
     *  - we leave DefDef stubs in the tree that are later filled in by tree duplication and adaptation
     * @see duplicateBody
     */
    private def expandInnerNormalizedMembers(tree: Tree) = tree match {
      case ddef @ DefDef(_, _, _, vparams :: Nil, _, rhs)
           if ddef.symbol.owner.isMethod &&
           specializedTypeVars(ddef.symbol.info).nonEmpty &&
           !ddef.symbol.hasFlag(SPECIALIZED) =>

        val sym = ddef.symbol
        val owner = sym.owner
        val norm = normalizeMember(owner, sym, emptyEnv)

        if (norm.lengthIs > 1) {
          // record the body for duplication
          body(sym) = rhs
          parameters(sym) = vparams.map(_.symbol)
          // to avoid revisiting the member, we can set the SPECIALIZED
          // flag. nobody has to see this anyway :)
          sym.setFlag(SPECIALIZED)
          // create empty bodies for specializations
          localTyper.typed(Block(norm.tail.map(sym => DefDef(sym, (_: List[List[Symbol]]) => EmptyTree)), ddef))
        } else
          tree
      case _ =>
        tree
    }

    /** Duplicate the body of the given method `tree` to the new symbol `source`.
     *
     *  Knowing that the method can be invoked only in the `castmap` type environment,
     *  this method will insert casts for all the expressions of types mappend in the
     *  `castmap`.
     */
    private def duplicateBody(tree: DefDef, source: Symbol, castmap: TypeEnv = emptyEnv) = {
      val symbol = tree.symbol
      val meth   = addBody(tree, source)

      val d = new SpecializationDuplicator(castmap)
      debuglog("-->d DUPLICATING: " + meth)
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
      debuglog("specializing body of" + symbol.defString)
      val DefDef(_, _, tparams, vparams :: Nil, tpt, _) = tree: @unchecked
      val env = typeEnv(symbol)

      val srcVparams = parameters(source)
      val srcTparams = source.typeParams.filter(tparam => !env.contains(tparam) || !isPrimitiveValueType(env(tparam)))
      if (settings.isDebug && (srcTparams.nonEmpty || symbol.typeParams.nonEmpty))
        debuglog("substituting " + srcTparams + " for " + symbol.typeParams)

      // skolemize type parameters
      val oldTparams = tparams.map(_.symbol)
      val newTparams = deriveFreshSkolems(oldTparams)
      map2(tparams, newTparams)(_ setSymbol _)

      // create fresh symbols for value parameters to hold the skolem types
      val oldVparams = vparams.map(_.symbol)
      val newVparams = cloneSymbolsAtOwnerAndModify(oldVparams, symbol, _.substSym(oldTparams, newTparams))

      val srcParams = srcVparams ::: srcTparams
      val oldParams = oldVparams ::: oldTparams
      val newParams = newVparams ::: newTparams

      // replace value and type parameters of the old method with the new ones
      // log("Adding body for " + tree.symbol + " - origtparams: " + origtparams + "; tparams: " + tparams)
      // log("Type vars of: " + source + ": " + source.typeParams)
      // log("Type env of: " + tree.symbol + ": " + boundTvars)
      // log("newtparams: " + newtparams)
      // don't make private fields public
      val substituter = new ImplementationAdapter(srcParams, newParams, source.enclClass, addressFields = false)
      val newRhs = substituter(body(source).duplicate)
      tpt.modifyType(_.substSym(oldParams, newParams))
      copyDefDef(tree)(vparamss = newVparams.map(ValDef.apply) :: Nil, rhs = newRhs)
    }

    /** Create trees for specialized members of 'sClass', based on the
     *  symbols that are already there.
     */
    private def makeSpecializedMembers(sClass: Symbol): List[Tree] = {
      // add special overrides first
//      if (!specializedClass.hasFlag(SPECIALIZED))
//        for (m <- specialOverrides(specializedClass)) specializedClass.info.decls.enter(m)
      val mbrs = ListBuffer.empty[Tree]
      var hasSpecializedFields = false

      for (m <- sClass.info.decls
             if m.hasFlag(SPECIALIZED)
                 && (m.sourceFile ne null)
                 && satisfiable(typeEnv(m), !sClass.hasFlag(SPECIALIZED))) {
        debuglog("creating tree for " + m.fullName)
        if (m.isMethod)  {
          if (info(m).target.hasAccessorFlag) hasSpecializedFields = true
          if (m.isClassConstructor) {
            val origParams = parameters(info(m).target)
            val vparams = (
              map2(m.info.paramTypes, origParams)((tp, sym) =>
                m.newValue(specializedName(sym, typeEnv(sClass)), sym.pos, sym.flags) setInfo tp
              )
            )
            // param accessors for private members (the others are inherited from the generic class)
            if (m.isPrimaryConstructor)
              for (param <- vparams if sClass.info.nonPrivateMember(param.name) == NoSymbol) {
                val acc = param.cloneSymbol(sClass, param.flags | PARAMACCESSOR | PrivateLocal)
                sClass.info.decls.enter(acc)
                mbrs += ValDef(acc, EmptyTree).setType(NoType).setPos(m.pos)
              }
            // ctor
            mbrs += DefDef(m, Modifiers(m.flags), mmap(List(vparams))(ValDef.apply), EmptyTree)
          } else {
            mbrs += DefDef(m, (_: List[List[Symbol]]) => EmptyTree)
          }
        } else if (m.isValue) {
          mbrs += ValDef(m).setType(NoType)
        } else if (m.isClass) {
//           mbrs  +=
//              ClassDef(m, Template(m.info.parents map TypeTree, noSelfType, List())
//                         .setSymbol(m.newLocalDummy(m.pos)))
//            log("created synthetic class: " + m.fullName)
        }
      }
      if (hasSpecializedFields) {
        val isSpecializedInstance = (sClass hasFlag SPECIALIZED) || sClass.parentSymbolsIterator.exists(_ hasFlag SPECIALIZED)
        val sym = sClass.newMethod(nme.SPECIALIZED_INSTANCE, sClass.pos) setInfoAndEnter MethodType(Nil, BooleanTpe)

        mbrs += DefDef(sym, Literal(Constant(isSpecializedInstance)).setType(BooleanTpe)).setType(NoType)
      }
      mbrs.toList
    }

    /** Create specialized class definitions */
    def implSpecClasses(trees: List[Tree]): List[Tree] =
      trees.flatMap {
        case tree @ ClassDef(_, _, _, impl) =>
          val sym1 = tree.symbol
          sym1.info // force specialization
          val specMap = specializationOf(sym1)
          if (specMap.isEmpty) Nil
          else specMap.iterator.map {
            case (env, specCls) =>
              debuglog(s"created synthetic class: $specCls of $sym1 in ${pp(env)}")
              val parents = specCls.info.parents.map(TypeTree)
              ClassDef(specCls, atPos(impl.pos)(Template(parents, noSelfType, List()))
                .setSymbol(specCls.newLocalDummy(sym1.pos))) setPos tree.pos
          }.toList
        case _ => Nil
      }.sortBy(_.name.decoded)
  }

  private def forwardCall(pos: scala.reflect.internal.util.Position, receiver: Tree, paramss: List[List[ValDef]]): Tree = {
    val argss = mmap(paramss)(x => Ident(x.symbol))
    atPos(pos) { argss.foldLeft(receiver)(Apply.apply) }
  }

  /** Forward to the generic class constructor. If the current class initializes
   *  specialized fields corresponding to parameters, it passes null to the superclass
   *  constructor.
   *
   *  For example:
   *  {{{
   *    case class Tuple2[T, U](x: T, y: U)
   *
   *    class Tuple2\$II {
   *      val _x\$I: Int = ..
   *      def x = _x\$I
   *      // same for y
   *      def this(x: Int, y: Int) {
   *        super.this(null.asInstanceOf[Int], null.asInstanceOf[Int])
   *      }
   *    }
   *  }}}
   *
   *  Note that erasure first transforms `null.asInstanceOf[Int]` to `unbox(null)`, which is 0.
   *  Then it adapts the argument `unbox(null)` of type Int to the erased parameter type of Tuple2,
   *  which is Object, so it inserts a `box` call and we get `box(unbox(null))`, which is
   *  `new Integer(0)` (not `null`).
   *
   *  However it does not make sense to create an Integer instance to be stored in the generic field
   *  of the superclass: that field is never used. Therefore we mark the `null` tree with the
   *  [[SpecializedSuperConstructorCallArgument]] attachment and special-case erasure to replace
   *  `box(unbox(null))` by `null` in this case.
   */
  private def forwardCtorCall(pos: scala.reflect.internal.util.Position, receiver: Tree, paramss: List[List[ValDef]], clazz: Symbol): Tree = {
    log(s"forwardCtorCall($pos, $receiver, $paramss, $clazz)")

    /* A constructor parameter `f` initializes a specialized field
     * iff:
     *   - it is specialized itself
     *   - there is a getter for the original (non-specialized) field in the same class
     *   - there is a getter for the specialized field in the same class
     */
    def initializesSpecializedField(f: Symbol) = (
         f.name.endsWith(nme.SPECIALIZED_SUFFIX)
      && clazz.info.member(f.unexpandedName).isPublic
      && clazz.info.decl(f.name).suchThat(_.isGetter) != NoSymbol
    )

    val argss = mmap(paramss)(x =>
      if (initializesSpecializedField(x.symbol))
        gen.mkAsInstanceOf(Literal(Constant(null)).updateAttachment(SpecializedSuperConstructorCallArgument), x.symbol.tpe)
      else
        Ident(x.symbol)
    )
    atPos(pos) { argss.foldLeft(receiver)(Apply.apply) }
  }

  /** Add method m to the set of symbols for which we need an implementation tree
   *  in the tree transformer.
   *
   *  @note This field is part of the specializeTypes subcomponent, so any symbols
   *        that here are not garbage collected at the end of a compiler run!
   */
  def addConcreteSpecMethod(m: Symbol): Unit = {
    if (currentRun.compiles(m)) concreteSpecMethods += m
  }

  private def makeArguments(fun: Symbol, vparams: List[Symbol]): List[Tree] = (
    //! TODO: make sure the param types are seen from the right prefix
    map2(fun.info.paramTypes, vparams)((tp, arg) => gen.maybeMkAsInstanceOf(Ident(arg), tp, arg.tpe))
  )

  object SpecializedSuperConstructorCallArgument
}
