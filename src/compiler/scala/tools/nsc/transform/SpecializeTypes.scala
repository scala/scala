/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Iulian Dragos
 */
// $Id$

package scala.tools.nsc
package transform

import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.util.FreshNameCreator

import scala.collection.{mutable, immutable}

/** Specialize code on types.
 */
abstract class SpecializeTypes extends InfoTransform with TypingTransformers {
  import global._
  import Flags._
  /** the name of the phase: */
  val phaseName: String = "specialize"

  /** This phase changes base classes. */
  override def changesBaseClasses = true
  override def keepsTypeParams = true

  /** Concrete types for specialization */
//  final lazy val concreteTypes = List(definitions.IntClass.tpe, definitions.DoubleClass.tpe)

  type TypeEnv = immutable.Map[Symbol, Type]
  def emptyEnv: TypeEnv = immutable.ListMap.empty[Symbol, Type]

  object TypeEnv {
    /** Return a new type environment binding specialized type parameters of sym to
     *  the given args. Expects the lists to have the same length.
     */
    def fromSpecialization(sym: Symbol, args: List[Type]): TypeEnv = {
      assert(sym.info.typeParams.length == args.length, sym + " args: " + args)
      var env = emptyEnv
      for ((tvar, tpe) <- sym.info.typeParams.zip(args) if tvar.hasAnnotation(SpecializedClass))
        env = env + ((tvar, tpe))
      env
    }

    /** Is this typeenv included in `other'? All type variables in this environment
     *  are defined in `other' and bound to the same type.
     */
    def includes(t1: TypeEnv, t2: TypeEnv) = {
      t1 forall { kv =>
        t2.get(kv._1) match {
          case Some(v2) => v2 == kv._2
          case _ => false
        }
      }
    }

    /** Reduce the given environment to contain mappings only for type variables in tps. */
    def reduce(env: TypeEnv, tps: immutable.Set[Symbol]): TypeEnv = {
      env filter { kv => tps.contains(kv._1)}
    }

    /** Is the given environment a valid specialization for sym?
     *  It is valid if each binding is from a @specialized type parameter in sym (or its owner)
     *  to a type for which `sym' is specialized.
     */
    def isValid(env: TypeEnv, sym: Symbol): Boolean = {
      def validBinding(tvar: Symbol, tpe: Type, sym: Symbol) =
        (tvar.hasAnnotation(SpecializedClass)
         && sym.typeParams.contains(tvar)
         && concreteTypes(tvar).contains(tpe))
      env forall { binding =>
        val (tvar, tpe) = binding
//         log("isValid: " + env + " sym: " + sym + " sym.tparams: " + sym.typeParams)
//         log("Flag " + tvar + ": " + tvar.hasAnnotation(SpecializedClass))
//         log("tparams contains: " + sym.typeParams.contains(tvar))
//         log("concreteTypes: " + concreteTypes.contains(tpe))
        (validBinding(tvar, tpe, sym)
         || ((sym.owner != definitions.RootClass)
             && validBinding(tvar, tpe, sym.owner)))
      }
    }
  }

  /** For a given class and concrete type arguments, give its specialized class */
  val specializedClass: mutable.Map[(Symbol, TypeEnv), Symbol] = new mutable.LinkedHashMap

  /** Returns the generic class that was specialized to 'cls', or
   *  'cls' itself if cls is not a specialized subclass.
   */
  def genericClass(cls: Symbol): Symbol =
    if (cls.hasFlag(SPECIALIZED))
      cls.info.parents.head.typeSymbol
    else
      cls

  /** Map a method symbol to a list of its specialized overloads in the same class. */
  private val overloads: mutable.Map[Symbol, List[Overload]] = new mutable.HashMap[Symbol, List[Overload]] {
    override def default(key: Symbol): List[Overload] = Nil
  }

  case class Overload(sym: Symbol, env: TypeEnv) {
    override def toString: String =
      "specialized overload " + sym + " in " + env
  }

  /** The annotation used to mark specialized type parameters. */
  lazy val SpecializedClass = definitions.getClass("scala.specialized")

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
  }

  /** Symbol is a special overloaded method of 'original', in the environment env. */
  case class SpecialOverload(original: Symbol, env: TypeEnv) extends SpecializedInfo {
    def target = original
  }

  /** Symbol is a method that should be forwarded to 't' */
  case class Forward(t: Symbol) extends SpecializedInfo {
    def target = t
  }

  /** Symbol is a specialized accessor for the `target' field. */
  case class SpecializedAccessor(target: Symbol) extends SpecializedInfo

  /** Symbol is a specialized method whose body should be the target's method body. */
  case class Implementation(target: Symbol) extends SpecializedInfo

  /** An Inner class that specializes on a type parameter of the enclosing class. */
  case class SpecializedInnerClass(target: Symbol, env: TypeEnv) extends SpecializedInfo

  /** Symbol is a normalized member of 'target'. */
  case class NormalizedMember(target: Symbol) extends SpecializedInfo {

    /** Type bounds of a @specialized type var are now in the environment. */
    override def typeBoundsIn(env: TypeEnv): Boolean = {
      target.info.typeParams exists { tvar =>
        (tvar.hasAnnotation(SpecializedClass)
         && (specializedTypeVars(tvar.info.bounds) exists env.isDefinedAt))
      }
    }

    override lazy val degenerate = {
      log("degenerate: " + target +
              " stv tparams: " + specializedTypeVars(target.info.typeParams map (_.info)) +
              " stv info: " + specializedTypeVars(target.info.resultType))
      !(specializedTypeVars(target.info.typeParams map (_.info))
        -- specializedTypeVars(target.info.resultType)).isEmpty
    }
  }

  /** Map a symbol to additional information on specialization. */
  private val info: mutable.Map[Symbol, SpecializedInfo] = new mutable.HashMap[Symbol, SpecializedInfo]

  /** Has `clazz' any type parameters that need be specialized? */
  def hasSpecializedParams(clazz: Symbol): Boolean =
    !specializedParams(clazz).isEmpty

  /** Return specialized type parameters. */
  def specializedParams(sym: Symbol): List[Symbol] =
    splitParams(sym.info.typeParams)._1

  def splitParams(tps: List[Symbol]) =
    tps.partition(_.hasAnnotation(SpecializedClass))

  def unspecializedArgs(sym: Symbol, args: List[Type]): List[Type] =
    for ((tvar, tpe) <- sym.info.typeParams.zip(args) if !tvar.hasAnnotation(SpecializedClass))
      yield tpe

  val specializedType = new TypeMap {
    override def apply(tp: Type): Type = tp match {
      case TypeRef(pre, sym, args) if !args.isEmpty =>
        val pre1 = this(pre)
        val args1 = args// map this
        val unspecArgs = unspecializedArgs(sym, args)
        specializedClass.get((sym, TypeEnv.fromSpecialization(sym, args1))) match {
          case Some(sym1) =>
            assert(sym1.info.typeParams.length == unspecArgs.length, sym1)
            typeRef(pre1, sym1, unspecArgs)
          case None =>
            typeRef(pre1, sym, args1)
        }
      case _ => tp // mapOver(tp)
    }
  }

  /** Return the specialized overload of sym in the given env, if any. */
  def overload(sym: Symbol, env: TypeEnv) =
    overloads(sym).find(ov => TypeEnv.includes(ov.env, env))

  /** Return the specialized name of 'sym' in the given environment. It
   *  guarantees the same result regardless of the map order by sorting
   *  type variables alphabetically.
   */
  private def specializedName(sym: Symbol, env: TypeEnv): Name = {
    val tvars = if (sym.isClass) env.keySet
                else specializedTypeVars(sym.info).intersect(env.keySet)
    val (methparams, others) = tvars.toList.partition(_.owner.isMethod)
    val tvars1 = methparams sortBy (_.name.toString)
    val tvars2 = others sortBy (_.name.toString)
    if (settings.debug.value) log("specName(" + sym + ") env " + env)
    specializedName(sym.name, tvars1 map env, tvars2 map env)
  }

  /** Specialize name for the two list of types. The first one denotes
   *  specialization on method type parameters, the second on outer environment.
   */
  private def specializedName(name: Name, types1: List[Type], types2: List[Type]): Name = {
    def split: (String, String, String) = {
      if (name.endsWith("$sp")) {
        val name1 = name.subName(0, name.length - 3)
        val idxC = name1.lastPos('c')
        val idxM = name1.lastPos('m', idxC)
        (name1.subName(0, idxM - 1).toString,
         name1.subName(idxC + 1, name1.length).toString,
         name1.subName(idxM + 1, idxC).toString)
      } else
        (name.toString, "", "")
    }

    if (nme.INITIALIZER == name || (types1.isEmpty && types2.isEmpty))
      name
    else if (nme.isSetterName(name))
      nme.getterToSetter(specializedName(nme.setterToGetter(name), types1, types2))
    else if (nme.isLocalName(name))
      nme.getterToLocal(specializedName(nme.localToGetter(name), types1, types2))
    else {
      val (base, cs, ms) = split
      newTermName(base + "$"
                  + "m" + ms + types1.map(t => definitions.abbrvTag(t.typeSymbol)).mkString("", "", "")
                  + "c" + cs + types2.map(t => definitions.abbrvTag(t.typeSymbol)).mkString("", "", "$sp"))
    }
  }

  lazy val primitiveTypes = List(
    definitions.UnitClass.tpe,
    definitions.BooleanClass.tpe,
    definitions.ByteClass.tpe,
    definitions.ShortClass.tpe,
    definitions.CharClass.tpe,
    definitions.IntClass.tpe,
    definitions.LongClass.tpe,
    definitions.FloatClass.tpe,
    definitions.DoubleClass.tpe)

   /** Return the concrete types `sym' should be specialized at.
   */
  def concreteTypes(sym: Symbol): List[Type] =
    sym.getAnnotation(SpecializedClass) match {
      case Some(AnnotationInfo(_, args, _)) =>
        args match {
          case Nil =>
            log(sym + " specialized on everything")
            primitiveTypes.toList
          case _ =>
            val tpes = args.map(_.symbol.companionClass.tpe)
            log(sym + " specialized on " + tpes)
            tpes
        }
      case _ =>
        Nil
    }

  /** Return a list of all type environments for all specializations
   *  of @specialized types in `tps'.
   */
  private def specializations(tps: List[Symbol]): List[TypeEnv] = {
    val stps = tps filter (_.hasAnnotation(SpecializedClass))
    val env = immutable.HashMap.empty[Symbol, Type]
    count(stps, concreteTypes _) map { tps =>
      immutable.HashMap.empty[Symbol, Type] ++ (stps zip tps)
    }
  }

  /** Generate all arrangements with repetitions from the list of values,
   *  with 'pos' positions. For example, count(2, List(1, 2)) yields
   *  List(List(1, 1), List(1, 2), List(2, 1), List(2, 2))
   */
  private def count[A, V](xs: List[A], values: A => List[V]): List[List[V]] = {
    if (xs.isEmpty) Nil
    else if (xs.tail.isEmpty) values(xs.head) map (_ :: Nil)
    else for (v <- values(xs.head); vs <- count(xs.tail, values)) yield v :: vs
  }

  /** Does the given tpe need to be specialized in the environment 'env'? */
  private def needsSpecialization(env: TypeEnv, sym: Symbol): Boolean = {
    def needsIt(tpe: Type): Boolean =  tpe match {
      case TypeRef(pre, sym, args) =>
        (env.keysIterator.contains(sym)
         || (args exists needsIt))
      case PolyType(tparams, resTpe) => needsIt(resTpe)
      case MethodType(argTpes, resTpe) =>
        (argTpes exists (sym => needsIt(sym.tpe))) || needsIt(resTpe)
      case ClassInfoType(parents, stats, sym) =>
        stats.toList exists (s => needsIt(s.info))
      case _ => false
    }

    (needsIt(sym.info)
     || (isNormalizedMember(sym) && info(sym).typeBoundsIn(env)))

  }

  def isNormalizedMember(m: Symbol): Boolean =
    (m.hasFlag(SPECIALIZED) && (info.get(m) match {
      case Some(NormalizedMember(_)) => true
      case _ => false
    }))


  private def specializedTypeVars(tpe: List[Type]): immutable.Set[Symbol] =
    tpe.foldLeft(immutable.ListSet.empty[Symbol]: immutable.Set[Symbol]) {
      (s, tp) => s ++ specializedTypeVars(tp)
    }

  private def specializedTypeVars(sym: Symbol): immutable.Set[Symbol] =
    specializedTypeVars(atPhase(currentRun.typerPhase)(sym.info))

  /** Return the set of @specialized type variables mentioned by the given type. */
  private def specializedTypeVars(tpe: Type): immutable.Set[Symbol] = tpe match {
    case TypeRef(pre, sym, args) =>
      if (sym.isTypeParameter && sym.hasAnnotation(SpecializedClass))
        specializedTypeVars(args) + sym
      else if (sym.isTypeSkolem && sym.deSkolemize.hasAnnotation(SpecializedClass)) {
        println("cought skolem without @specialized")
        specializedTypeVars(args) + sym
      } else
        specializedTypeVars(args)
    case PolyType(tparams, resTpe) =>
      specializedTypeVars(tparams map (_.info)) ++ specializedTypeVars(resTpe)
    case MethodType(argSyms, resTpe) =>
      specializedTypeVars(argSyms map (_.tpe)) ++ specializedTypeVars(resTpe)
    case ExistentialType(_, res) => specializedTypeVars(res)
    case AnnotatedType(_, tp, _) => specializedTypeVars(tp)
    case TypeBounds(hi, lo) => specializedTypeVars(hi) ++ specializedTypeVars(lo)
    case _ => immutable.ListSet.empty[Symbol]
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
    def specializedClass(env: TypeEnv, normMembers: List[Symbol]): Symbol = {
      val cls = clazz.owner.newClass(clazz.pos, specializedName(clazz, env))
                              .setFlag(SPECIALIZED | clazz.flags)
                              .resetFlag(CASE)
      cls.sourceFile = clazz.sourceFile
      currentRun.symSource(cls) = clazz.sourceFile // needed later on by mixin

      typeEnv(cls) = env
      this.specializedClass((clazz, env)) = cls

      // declarations of the newly specialized class 'cls'
      val decls1 = new Scope

      // original unspecialized type parameters
      var oldClassTParams: List[Symbol] = Nil

      // unspecialized type parameters of 'cls' (cloned)
      var newClassTParams: List[Symbol] = Nil

      // has to be a val in order to be computed early. It is later called
      // within 'atPhase(next)', which would lead to an infinite cycle otherwise
      val specializedInfoType: Type = {
        val (_, unspecParams) = splitParams(clazz.info.typeParams)
        oldClassTParams = unspecParams
        newClassTParams = cloneSymbols(unspecParams, cls)

        def applyContext(tpe: Type) =
          subst(env, tpe).subst(unspecParams, newClassTParams map (_.tpe))

        /** Return a list of specialized parents to be re-mixed in a specialized subclass.
         *  Assuming env = [T -> Int] and
         *    class Integral[@specialized T] extends Numeric[T]
         *  and Numeric[U] is specialized on U, this produces List(Numeric$mcI).
         *
         *  so that class Integral$mci extends Integral[Int] with Numeric$mcI.
         */
        def specializedParents(parents: List[Type]): List[Type] = {
          val res = new mutable.ListBuffer[Type]
          for (p <- parents) {
            val stp = specializedType(p)
            if (stp != p)
              if (p.typeSymbol.isTrait) res += stp
              else if (currentRun.compiles(clazz))
                reporter.warning(clazz.pos, p.typeSymbol + " must be a trait. Specialized version of "
                  + clazz + " will inherit generic " + p)
          }
          res.reverse.toList
        }

        var parents = List(applyContext(atPhase(currentRun.typerPhase)(clazz.tpe)))
        if (parents.head.typeSymbol.isTrait)
          parents = parents.head.parents.head :: parents
        val extraSpecializedMixins = specializedParents(clazz.info.parents.map(applyContext))
        log("extraSpecializedMixins: " + extraSpecializedMixins)
        val infoType = ClassInfoType(parents ::: extraSpecializedMixins, decls1, cls)
        if (newClassTParams.isEmpty) infoType else PolyType(newClassTParams, infoType)
      }

      atPhase(phase.next)(cls.setInfo(specializedInfoType))

      val fullEnv = outerEnv ++ env

      /** Enter 'sym' in the scope of the current specialized class. It's type is
       *  mapped through the active environment, binding type variables to concrete
       *  types. The existing typeEnv for `sym' is composed with the current active
       *  environment
       */
      def enterMember(sym: Symbol): Symbol = {
        typeEnv(sym) = fullEnv ++ typeEnv(sym) // append the full environment
        sym.setInfo(sym.info.substThis(clazz, ThisType(cls)).subst(oldClassTParams, newClassTParams map (_.tpe)))
        decls1.enter(subst(fullEnv)(sym))
      }

      /** Create and enter in scope an overridden symbol m1 for `m' that forwards
       *  to `om'. `om' is a fresh, special overload of m1 that is an implementation
       *  of `m'. For example, for a
       *
       *  class Foo[@specialized A] {
       *    def m(x: A) = <body>
       *  }
       *  , for class Foo$I extends Foo[Int], this method enters two new symbols in
       *  the scope of Foo$I:
       *
       *    def m(x: Int) = m$I(x)
       *    def m$I(x: Int) = <body>/adapted to env {A -> Int}
       */
      def forwardToOverload(m: Symbol): Symbol = {
        val specMember = enterMember(m.cloneSymbol(cls)).setFlag(OVERRIDE | SPECIALIZED).resetFlag(DEFERRED | CASEACCESSOR)
        val om = specializedOverload(cls, m, env).setFlag(OVERRIDE)

        var original = info.get(m) match {
          case Some(NormalizedMember(tg)) => tg
          case _ => m
        }

        info(specMember)  = Forward(om)
        info(om) = if (original.isDeferred) Forward(original) else Implementation(original)
        log("forwardToOverlad: " + " original.isDeferred: " + original.isDeferred + " info(om): " + info(om))
        typeEnv(om) = env ++ typeEnv(m) // add the environment for any method tparams
        overloads(specMember) = Overload(om, typeEnv(om)) :: overloads(specMember)

        enterMember(om)
      }

      log("specializedClass: " + cls)
      for (m <- normMembers if needsSpecialization(outerEnv ++ env, m) && satisfiable(fullEnv)) {
        if (settings.debug.value) log(" * looking at: " + m)
        if (!m.isDeferred) concreteSpecMethods += m

        // specialized members have to be overridable.
        if (m.hasFlag(PRIVATE))
          m.resetFlag(PRIVATE).setFlag(PROTECTED)

        if (m.isConstructor) {
          val specCtor = enterMember(m.cloneSymbol(cls).setFlag(SPECIALIZED))
          info(specCtor) = Forward(m)

        } else if (isNormalizedMember(m)) {  // methods added by normalization
          val NormalizedMember(original) = info(m)
          if (!conflicting(env ++ typeEnv(m))) {
            if (info(m).degenerate) {
              log("degenerate normalized member " + m + " info(m): " + info(m))
              val specMember = enterMember(m.cloneSymbol(cls)).setFlag(SPECIALIZED).resetFlag(DEFERRED)
              info(specMember) = Implementation(original)
              typeEnv(specMember) = env ++ typeEnv(m)
            } else {
              val om = forwardToOverload(m)
              log("normalizedMember " + m + " om: " + om + " typeEnv(om): " + typeEnv(om))
            }
          } else
            log("conflicting env for " + m + " env: " + env)

        } else if (m.isDeferred) { // abstract methods
          val specMember = enterMember(m.cloneSymbol(cls)).setFlag(SPECIALIZED).resetFlag(DEFERRED)
          if (settings.debug.value) log("deferred " + specMember.fullName + " is forwarded")

          info(specMember) = new Forward(specMember) {
            override def target = m.owner.info.member(specializedName(m, env))
          }

        } else if (m.isMethod && !m.hasFlag(ACCESSOR)) { // other concrete methods
          forwardToOverload(m)

        } else if (m.isValue && !m.isMethod) { // concrete value definition
          def mkAccessor(field: Symbol, name: Name) = {
            val sym = cls.newMethod(field.pos, name)
                .setFlag(SPECIALIZED | m.getter(clazz).flags)
                .resetFlag(LOCAL | PARAMACCESSOR | CASEACCESSOR | LAZY) // we rely on the super class to initialize param accessors
            info(sym) = SpecializedAccessor(field)
            sym
          }

          def overrideIn(clazz: Symbol, sym: Symbol) = {
            val sym1 = sym.cloneSymbol(clazz)
                          .setFlag(OVERRIDE | SPECIALIZED)
                          .resetFlag(DEFERRED | CASEACCESSOR | ACCESSOR | PARAMACCESSOR | LAZY)
            sym1.setInfo(sym1.info.asSeenFrom(clazz.tpe, sym1.owner))
          }

          val specVal = specializedOverload(cls, m, env)

          concreteSpecMethods += m
          specVal.asInstanceOf[TermSymbol].setAlias(m)

          enterMember(specVal)
          // create accessors
          if (settings.debug.value)
            log("m: " + m + " isLocal: " + nme.isLocalName(m.name) + " specVal: " + specVal.name + " isLocal: " + nme.isLocalName(specVal.name))
          if (nme.isLocalName(m.name)) {
            val specGetter = mkAccessor(specVal, nme.localToGetter(specVal.name)).setInfo(MethodType(List(), specVal.info))
            val origGetter = overrideIn(cls, m.getter(clazz))
            info(origGetter) = Forward(specGetter)
            enterMember(specGetter)
            enterMember(origGetter)
            log("created accessors: " + specGetter + " orig: " + origGetter)

            clazz.caseFieldAccessors.find(_.name.startsWith(m.name)) foreach { cfa =>
              val cfaGetter = overrideIn(cls, cfa)
              info(cfaGetter) = SpecializedAccessor(specVal)
              enterMember(cfaGetter)
              log("found case field accessor for " + m + " added override " + cfaGetter);
            }

            if (specVal.isVariable && m.setter(clazz) != NoSymbol) {
              val specSetter = mkAccessor(specVal, nme.getterToSetter(specGetter.name))
                .resetFlag(STABLE)
              specSetter.setInfo(MethodType(specSetter.newSyntheticValueParams(List(specVal.info)),
                                            definitions.UnitClass.tpe))
              val origSetter = overrideIn(cls, m.setter(clazz))
              info(origSetter) = Forward(specSetter)
              enterMember(specSetter)
              enterMember(origSetter)
            }
          } else { // if there are no accessors, specialized methods will need to access this field in specialized subclasses
            m.resetFlag(PRIVATE)
            specVal.resetFlag(PRIVATE)
          }
        } else if (m.isClass) {
          val specClass: Symbol = m.cloneSymbol(cls).setFlag(SPECIALIZED)
          typeEnv(specClass) = fullEnv
          specClass.name = specializedName(specClass, fullEnv)
          enterMember(specClass)
          log("entered specialized class " + specClass.fullName)
          info(specClass) = SpecializedInnerClass(m, fullEnv)
        }
      }
      cls
    }

    log("specializeClass " + clazz.fullName)
    val decls1 = (clazz.info.decls.toList flatMap { m: Symbol =>
      if (m.isAnonymousClass) List(m) else {
        normalizeMember(m.owner, m, outerEnv) flatMap { normalizedMember =>
          val ms = specializeMember(m.owner, normalizedMember, outerEnv, clazz.info.typeParams)
//          atPhase(currentRun.typerPhase)(println("normalizedMember.info: " + normalizedMember.info)) // bring the info to the typer phase
          if (normalizedMember.isMethod) {
            val newTpe = subst(outerEnv, normalizedMember.info)
            if (newTpe != normalizedMember.info) // only do it when necessary, otherwise the method type might be at a later phase already
              normalizedMember.updateInfo(newTpe) :: ms
            else
              normalizedMember :: ms
          } else
            normalizedMember :: ms
        }
      }
    })

    var hasSubclasses = false
    for (env <- specializations(clazz.info.typeParams) if satisfiable(env)) {
      val spc = specializedClass(env, decls1)
      log("entered " + spc + " in " + clazz.owner)
      hasSubclasses = true
      atPhase(phase.next)(clazz.owner.info.decls enter spc) //!! assumes fully specialized classes
    }
    if (hasSubclasses) clazz.resetFlag(FINAL)
    decls1
  }

  /** Expand member `sym' to a set of normalized members. Normalized members
   *  are monomorphic or polymorphic only in non-specialized types.
   *
   *  Given method m[@specialized T, U](x: T, y: U) it returns
   *     m[T, U](x: T, y: U),
   *     m$I[ U](x: Int, y: U),
   *     m$D[ U](x: Double, y: U)
   */
  private def normalizeMember(owner: Symbol, sym: Symbol, outerEnv: TypeEnv): List[Symbol] = {
    if (settings.debug.value) log("normalizeMember: " + sym.fullName)
    if (sym.isMethod && !atPhase(currentRun.typerPhase)(sym.typeParams.isEmpty)) {
      val (stps, tps) = splitParams(sym.info.typeParams)
      val res = sym :: (for (env <- specializations(stps) if needsSpecialization(env, sym)) yield {
        val keys = env.keysIterator.toList;
        val vals = env.valuesIterator.toList
        val specMember =  sym.cloneSymbol(owner).setFlag(SPECIALIZED).resetFlag(DEFERRED)
        specMember.name = specializedName(sym, env)

        typeEnv(specMember) = outerEnv ++ env
        val tps1 = cloneSymbols(tps)
        for (tp <- tps1) tp.setInfo(tp.info.subst(keys, vals))
        // the cloneInfo is necessary so that method parameter symbols are cloned at the new owner
        val methodType = sym.info.resultType.subst(keys ::: tps, vals ::: (tps1 map (_.tpe))).cloneInfo(specMember)

        specMember.setInfo(polyType(tps1, methodType))

        log("expanded member: " + sym  + ": " + sym.info + " -> " + specMember + ": " + specMember.info + " env: " + env)
        info(specMember) = NormalizedMember(sym)
        overloads(sym) = Overload(specMember, env) :: overloads(sym)
        specMember
      })
      //stps foreach (_.removeAttribute(SpecializedClass))
      res
    } else List(sym)
  }

  /** Specialize member `m' w.r.t. to the outer environment and the type parameters of
   *  the innermost enclosing class.
   *
   *  Turns 'private' into 'protected' for members that need specialization.
   *
   *  Return a list of symbols that are specializations of 'sym', owned by 'owner'.
   */
  private def specializeMember(owner: Symbol, sym: Symbol, outerEnv: TypeEnv, tps: List[Symbol]): List[Symbol] = {
    def specializeOn(tparams: List[Symbol]): List[Symbol] =
      for (spec <- specializations(tparams)) yield {
        if (sym.hasFlag(PRIVATE)) sym.resetFlag(PRIVATE).setFlag(PROTECTED)
        val specMember = subst(outerEnv)(specializedOverload(owner, sym, spec))
        typeEnv(specMember) = outerEnv ++ spec
        overloads(sym) = Overload(specMember, spec) :: overloads(sym)
        specMember
      }

    if (sym.isMethod) {
//      log("specializeMember " + sym + " with own stps: " + specializedTypes(sym.info.typeParams))
      val tps1 = if (sym.isConstructor) tps filter (tp => sym.info.paramTypes.contains(tp)) else tps
      val tps2 = tps1 intersect specializedTypeVars(sym).toList
      if (!sym.isDeferred) concreteSpecMethods += sym

      specializeOn(tps2) map {m => info(m) = SpecialOverload(sym, typeEnv(m)); m}
    } else
      List()
  }

  /** Return the specialized overload of `m', in the given environment. */
  private def specializedOverload(owner: Symbol, sym: Symbol, env: TypeEnv): Symbol = {
    val specMember = sym.cloneSymbol(owner) // this method properly duplicates the symbol's info
    specMember.name = specializedName(sym, env)

    specMember.setInfo(subst(env, specMember.info))
      .setFlag(SPECIALIZED)
      .resetFlag(DEFERRED | CASEACCESSOR | ACCESSOR | LAZY)
  }

  /** For each method m that overrides inherited method m', add a special
   *  overload method `om' that overrides the corresponding overload in the
   *  superclass. For the following example:
   *
   *  class IntFun extends Function1[Int, Int] {
   *     def apply(x: Int): Int = ..
   *  }
   *
   *  this method will return List('apply$spec$II')
   */
  private def specialOverrides(clazz: Symbol): List[Symbol] = {
    log("specialOverrides(" + clazz + ")")
    val opc = new overridingPairs.Cursor(clazz)
    val oms = new mutable.ListBuffer[Symbol]
    while (opc.hasNext) {
      if (settings.debug.value)
        log("\toverriding pairs: " + opc.overridden.fullName + ": " + opc.overridden.info
               + " overriden by " + opc.overriding.fullName + ": " + opc.overriding.info)
      if (opc.overriding.owner == clazz && !specializedTypeVars(opc.overridden.info).isEmpty) {
        if (settings.debug.value) log("\t\tspecializedTVars: " + specializedTypeVars(opc.overridden.info))
        val env = unify(opc.overridden.info, opc.overriding.info, emptyEnv)
        if (settings.debug.value)
          log("\t\tenv: " + env + "isValid: "
                + TypeEnv.isValid(env, opc.overridden)
                + " exists: " + opc.overridden.owner.info.decl(specializedName(opc.overridden, env)))
        if (!env.isEmpty
            && TypeEnv.isValid(env, opc.overridden)
            && opc.overridden.owner.info.decl(specializedName(opc.overridden, env)) != NoSymbol) {
          log("Added specialized overload for " + opc.overriding.fullName + " in env: " + env)
          val om = specializedOverload(clazz, opc.overridden, env)
          typeEnv(om) = env
          if (!opc.overriding.isDeferred) {
            concreteSpecMethods += opc.overriding
            // if the override is a normalized member, 'om' gets the implementation from
            // its original target, and adds the environment of the normalized member (that is,
            // any specialized /method/ type parameter bindings)
            info(om) = info.get(opc.overriding) match {
              case Some(NormalizedMember(target)) =>
                typeEnv(om) = env ++ typeEnv(opc.overriding)
                Implementation(target)
              case _ => Implementation(opc.overriding)
            }
            info(opc.overriding)  = Forward(om)
            log("typeEnv(om) = " + typeEnv(om))
            om setPos opc.overriding.pos // set the position of the concrete, overriding member
          }
          overloads(opc.overriding) = Overload(om, env) :: overloads(opc.overriding)
          oms += om
          atPhase(phase.next)(
            assert(opc.overridden.owner.info.decl(om.name) != NoSymbol,
                   "Could not find " + om.name + " in " + opc.overridden.owner.info.decls))
        }
      }
      opc.next
    }
    oms.toList
  }

  /** Return the most general type environment that specializes tp1 to tp2.
   *  It only allows binding of type parameters annotated with @specialized.
   *  Fails if such an environment cannot be found.
   */
  private def unify(tp1: Type, tp2: Type, env: TypeEnv): TypeEnv = {
//    println("\tunify \t" + tp1 + "\n\t\t" + tp2)
    (tp1, tp2) match {
    case (TypeRef(_, sym1, _), _) if sym1.hasAnnotation(SpecializedClass) =>
      if (definitions.isValueClass(tp2.typeSymbol))
        env + ((sym1, tp2))
      else
        env
    case (TypeRef(_, sym1, args1), TypeRef(_, sym2, args2)) =>
      unify(args1, args2, env)
    case (TypeRef(_, sym1, _), _) if sym1.isTypeParameterOrSkolem =>
      env
    case (MethodType(params1, res1), MethodType(params2, res2)) =>
      unify(res1 :: (params1 map (_.tpe)), res2 :: (params2 map (_.tpe)), env)
    case (PolyType(tparams1, res1), PolyType(tparams2, res2)) =>
      unify(res1, res2, env)
    case (PolyType(_, res), other) =>
      unify(res, other, env)
    case (ThisType(_), ThisType(_)) => env
    case (_, SingleType(_, _)) => unify(tp1, tp2.underlying, env)
    case (SingleType(_, _), _) => unify(tp1.underlying, tp2, env)
    case (ThisType(_), _) => unify(tp1.widen, tp2, env)
    case (_, ThisType(_)) => unify(tp1, tp2.widen, env)
    case (RefinedType(_, _), RefinedType(_, _)) => env
    case (AnnotatedType(_, tp1, _), tp2) => unify(tp2, tp1, env)
    case (ExistentialType(_, res1), _) => unify(tp2, res1, env)
  }
  }

  private def unify(tp1: List[Type], tp2: List[Type], env: TypeEnv): TypeEnv =
    tp1.zip(tp2).foldLeft(env) { (env, args) =>
      unify(args._1, args._2, env)
    }

  private def specializedTypes(tps: List[Symbol]) = tps.filter(_.hasAnnotation(SpecializedClass))

  /** Map class symbols to the type environments where they were created. */
  val typeEnv: mutable.Map[Symbol, TypeEnv] = new mutable.HashMap[Symbol, TypeEnv] {
    override def default(key: Symbol) = emptyEnv
  }

  /** Apply type bindings in the given environment `env' to all declarations.  */
  private def subst(env: TypeEnv, decls: List[Symbol]): List[Symbol] =
    decls map subst(env)

  /** Apply the type environment 'env' to the given type. All type
   *  bindings are supposed to be to primitive types. A type variable
   *  that is annotated with 'uncheckedVariance' is mapped to the corresponding
   *  primitive type losing the annotation.
   */
  private def subst(env: TypeEnv, tpe: Type): Type = {
    class FullTypeMap(from: List[Symbol], to: List[Type]) extends SubstTypeMap(from, to) {

      override def mapOver(tp: Type): Type = tp match {
        case ClassInfoType(parents, decls, clazz) =>
          val parents1 = parents mapConserve (this);
          val declsList = decls.toList
          val decls1 = mapOver(declsList);
          if ((parents1 eq parents) && (decls1 eq declsList)) tp
          else ClassInfoType(parents1, new Scope(decls1), clazz)
        case AnnotatedType(annots, atp, selfsym) =>
          val annots1 = mapOverAnnotations(annots)
          val atp1 = this(atp)
          if ((annots1 eq annots) && (atp1 eq atp)) tp
          else if (annots1.isEmpty) atp1
          else if (atp1 ne atp) {
            val annots2 = annots1.filter(_.atp.typeSymbol != definitions.uncheckedVarianceClass)
            if (annots2.isEmpty) atp1
            else AnnotatedType(annots2, atp1, selfsym)
          } else
            AnnotatedType(annots1, atp1, selfsym)

        case _ => super.mapOver(tp)
      }
    }
    // disabled because of bugs in std. collections
    //val (keys, values) = env.iterator.toList.unzip
    val keys = env.keysIterator.toList
    val values = env.valuesIterator.toList
    (new FullTypeMap(keys, values))(tpe)
//    tpe.subst(keys, values)
  }

  private def subst(env: TypeEnv)(decl: Symbol): Symbol = {
    val tpe = subst(env, decl.info)
    decl.setInfo(if (decl.isConstructor) tpe match {
      case MethodType(args, resTpe) => MethodType(args, decl.owner.tpe)
    } else tpe)
  }

  /** Type transformation.
   */
  override def transformInfo(sym: Symbol, tpe: Type): Type = {
    val res = tpe match {
      case PolyType(targs, ClassInfoType(base, decls, clazz)) =>
        val parents = base map specializedType
        log("transformInfo (poly) " + clazz + " with parents1: " + parents + " ph: " + phase)
        PolyType(targs, ClassInfoType(parents, new Scope(specializeClass(clazz, typeEnv(clazz))), clazz))

      case ClassInfoType(base, decls, clazz) if !clazz.isPackageClass =>
        atPhase(phase.next)(base.map(_.typeSymbol.info))
        val parents = base map specializedType
        log("transformInfo " + clazz + " with parents1: " + parents + " ph: " + phase)
        val res = ClassInfoType(base map specializedType, new Scope(specializeClass(clazz, typeEnv(clazz))), clazz)
        res

      case _ =>
        tpe
    }
    res

  }

  def conflicting(env: TypeEnv): Boolean = {
    val silent = (pos: Position, str: String) => ()
    conflicting(env, silent)
  }

  /** Is any type variable in `env' conflicting with any if its type bounds, when
   *  type bindings in `env' are taken into account?
   *
   *  A conflicting type environment could still be satisfiable.
   */
  def conflicting(env: TypeEnv, warn: (Position, String) => Unit): Boolean =
    env exists { case (tvar, tpe) =>
      if (!((subst(env, tvar.info.bounds.lo) <:< tpe)
            && (tpe <:< subst(env, tvar.info.bounds.hi)))) {
        warn(tvar.pos, "Bounds prevent specialization for " + tvar)
        true
      } else false
  }

  /** The type environment is sound w.r.t. to all type bounds or only soft
   *  conflicts appear. An environment is sound if all bindings are within
   *  the bounds of the given type variable. A soft conflict is a binding
   *  that does not fall within the bounds, but whose bounds contain
   *  type variables that are @specialized, (that could become satisfiable).
   */
  def satisfiable(env: TypeEnv, warn: (Position, String) => Unit): Boolean = {
    def matches(tpe1: Type, tpe2: Type): Boolean = {
      val t1 = subst(env, tpe1)
      val t2 = subst(env, tpe2)
      ((t1 <:< t2)
        || !specializedTypeVars(t1).isEmpty
        || !specializedTypeVars(t2).isEmpty)
     }

    env forall { case (tvar, tpe) =>
      ((matches(tvar.info.bounds.lo, tpe)
       && matches(tpe, tvar.info.bounds.hi))
       || { warn(tvar.pos, "Bounds prevent specialization of " + tvar);
             log("specvars: "
                     + tvar.info.bounds.lo + ": " + specializedTypeVars(tvar.info.bounds.lo)
                     + " " + subst(env, tvar.info.bounds.hi) + ": " + specializedTypeVars(subst(env, tvar.info.bounds.hi)))
            false })
    }
  }

  def satisfiable(env: TypeEnv): Boolean = {
    val silent = (pos: Position, str: String) => ()
    satisfiable(env, silent)
  }

  import java.io.PrintWriter

  /*************************** Term transformation ************************************/

  class Duplicator extends {
    val global: SpecializeTypes.this.global.type = SpecializeTypes.this.global
  } with typechecker.Duplicators

  import global.typer.typed

  def specializeCalls(unit: CompilationUnit) = new TypingTransformer(unit) {
    /** Map a specializable method to it's rhs, when not deferred. */
    val body: mutable.Map[Symbol, Tree] = new mutable.HashMap

    /** Map a specializable method to its value parameter symbols. */
    val parameters: mutable.Map[Symbol, List[List[Symbol]]] = new mutable.HashMap

    /** Collect method bodies that are concrete specialized methods.
     */
    class CollectMethodBodies extends Traverser {
      override def traverse(tree: Tree) = tree match {
        case  DefDef(mods, name, tparams, vparamss, tpt, rhs) if concreteSpecMethods(tree.symbol) || tree.symbol.isConstructor =>
          log("adding body of " + tree.symbol)
          body(tree.symbol) = rhs
//          body(tree.symbol) = tree // whole method
          parameters(tree.symbol) = vparamss map (_ map (_.symbol))
          super.traverse(tree)
        case ValDef(mods, name, tpt, rhs) if concreteSpecMethods(tree.symbol) =>
          body(tree.symbol) = rhs
          super.traverse(tree)
        case _ =>
          super.traverse(tree)
      }
    }

    import posAssigner._

    override def transform(tree: Tree): Tree = {
      val symbol = tree.symbol

      /** The specialized symbol of 'tree.symbol' for tree.tpe, if there is one */
      def specSym(qual: Tree): Option[Symbol] = {
        val env = unify(symbol.tpe, tree.tpe, emptyEnv)
        log("checking for rerouting: " + tree + " with sym.tpe: " + symbol.tpe + " tree.tpe: " + tree.tpe + " env: " + env)
        if (!env.isEmpty) {  // a method?
          val specMember = overload(symbol, env)
          if (specMember.isDefined) Some(specMember.get.sym)
          else {  // a field?
	    val specMember = qual.tpe.member(specializedName(symbol, env))
	    if (specMember ne NoSymbol) Some(specMember)
            else None
          }
        } else None
      }

      def maybeTypeApply(fun: Tree, targs: List[Tree]) =
        if (targs.isEmpty)fun else TypeApply(fun, targs)

      curTree = tree
      tree match {
        case Apply(Select(New(tpt), nme.CONSTRUCTOR), args) =>
          if (findSpec(tpt.tpe).typeSymbol ne tpt.tpe.typeSymbol) {
            log("** instantiated specialized type: " + findSpec(tpt.tpe))
            atPos(tree.pos)(
              localTyper.typed(
                Apply(
                  Select(New(TypeTree(findSpec(tpt.tpe))), nme.CONSTRUCTOR),
                  transformTrees(args))))
          } else super.transform(tree)

        case TypeApply(Select(qual, name), targs)
                if (!specializedTypeVars(symbol.info).isEmpty && name != nme.CONSTRUCTOR) =>
          if (settings.debug.value) log("checking typeapp for rerouting: " + tree + " with sym.tpe: " + symbol.tpe + " tree.tpe: " + tree.tpe)
          val qual1 = transform(qual)
          specSym(qual1) match {
            case Some(specMember) =>
              if (settings.debug.value) log("found " + specMember)
              assert(symbol.info.typeParams.length == targs.length)
              val env = typeEnv(specMember)
              val residualTargs =
                for ((tvar, targ) <-symbol.info.typeParams.zip(targs) if !env.isDefinedAt(tvar))
                  yield targ
              assert(residualTargs.length == specMember.info.typeParams.length)
              val tree1 = maybeTypeApply(Select(qual1, specMember), residualTargs)
              log("rewrote " + tree + " to " + tree1)
              localTyper.typedOperator(atPos(tree.pos)(tree1)) // being polymorphic, it must be a method

            case None => super.transform(tree)
          }

        case Select(qual, name) =>
          if (settings.debug.value)
            log("looking at Select: " + tree + " sym: " + symbol + ": " + symbol.info + "[tree.tpe: " + tree.tpe + "]")
          //if (settings.debug.value) log("\toverloads: " + overloads.mkString("", "\n", ""))
          if (!specializedTypeVars(symbol.info).isEmpty && name != nme.CONSTRUCTOR) {
            if (settings.debug.value)
              log("checking for unification at " + tree + " with sym.tpe: " + symbol.tpe + " and tree.tpe: " + tree.tpe + " at " + tree.pos.line)
            val env = unify(symbol.tpe, tree.tpe, emptyEnv)
            log("checking for rerouting: " + tree + " with sym.tpe: " + symbol.tpe + " tree.tpe: " + tree.tpe + " env: " + env)
            if (!env.isEmpty) {
              val specMember = overload(symbol, env)
              if (specMember.isDefined) {
                if (settings.debug.value) log("** routing " + tree + " to " + specMember.get.sym.fullName)
                localTyper.typedOperator(atPos(tree.pos)(Select(transform(qual), specMember.get.sym.name)))
              } else {
                val qual1 = transform(qual)
                val specMember = qual1.tpe.member(specializedName(symbol, env))
                if (specMember ne NoSymbol) {
                  log("** using spec member " + specMember)
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
                if (settings.debug.value) log("** routing " + tree + " to " + specMember.sym.fullName + " tree: " + Select(qual1, specMember.sym.name))
                localTyper.typedOperator(atPos(tree.pos)(Select(qual1, specMember.sym.name)))
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
          val parents1 = currentOwner.info.parents.zipWithIndex.map {
            case (tpe, idx) => TypeTree(tpe) setPos parents(idx).pos
          }
          treeCopy.Template(tree,
            parents1    /*currentOwner.info.parents.map(tpe => TypeTree(tpe) setPos parents.head.pos)*/,
            self,
            atOwner(currentOwner)(transformTrees(body ::: specMembers)))

        case ddef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) if info.isDefinedAt(symbol) =>
          if (symbol.isConstructor) {
            val t = atOwner(symbol) {
              val superRef: Tree = Select(Super(nme.EMPTY.toTypeName, nme.EMPTY.toTypeName), nme.CONSTRUCTOR)
              forwardCall(tree.pos, superRef, vparamss)
            }
            val tree1 = atPos(symbol.pos)(treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt, Block(List(t), Literal(()))))
            localTyper.typed(tree1)
          } else info(symbol) match {

            case Implementation(target) =>
              assert(body.isDefinedAt(target), "sym: " + symbol.fullName + " target: " + target.fullName)
              // we have an rhs, specialize it
              val tree1 = duplicateBody(ddef, target)
              if (settings.debug.value) log("implementation: " + tree1)
              val DefDef(mods, name, tparams, vparamss, tpt, rhs) = tree1
              treeCopy.DefDef(tree1, mods, name, tparams, vparamss, tpt, transform(rhs))

            case NormalizedMember(target) =>
              log("normalized member " + symbol + " of " + target)

              if (target.isDeferred || conflicting(typeEnv(symbol))) {
/*
                val targs = makeTypeArguments(symbol, target)
                log("targs: " + targs)
                val call =
                  forwardCall(tree.pos,
                    TypeApply(
                      Select(This(symbol.owner), target),
                      targs map TypeTree),
                    vparamss)
                log("call: " + call)
                localTyper.typed(
                  treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt,
                              maybeCastTo(symbol.info.finalResultType,
                                          target.info.subst(target.info.typeParams, targs).finalResultType,
                                          call)))
*/

                treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt,
                  localTyper.typed(
                    Apply(gen.mkAttributedRef(definitions.Predef_error),
                          List(Literal("boom! you stepped on a bug. This method should never be called.")))))
              } else {
                // we have an rhs, specialize it
                val tree1 = duplicateBody(ddef, target)
                if (settings.debug.value) log("implementation: " + tree1)
                val DefDef(mods, name, tparams, vparamss, tpt, rhs) = tree1
                treeCopy.DefDef(tree1, mods, name, tparams, vparamss, tpt, transform(rhs))
              }

            case SpecialOverload(original, env) =>
              log("completing specialized " + symbol.fullName + " calling " + original)
              val t = DefDef(symbol, { vparamss =>
                val fun = Apply(Select(This(symbol.owner), original),
                                makeArguments(original, vparamss.head))

                gen.maybeMkAsInstanceOf(fun,
                  symbol.owner.thisType.memberType(symbol).finalResultType,
                  symbol.owner.thisType.memberType(original).finalResultType)
              })
              log("created " + t)
              localTyper.typed(t)

            case fwd @ Forward(_) =>
              val rhs1 = forwardCall(tree.pos, gen.mkAttributedRef(symbol.owner.thisType, fwd.target), vparamss)
              if (settings.debug.value)
                log("completed forwarder to specialized overload: " + fwd.target + ": " + rhs1)
              localTyper.typed(treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt, rhs1))

            case SpecializedAccessor(target) =>
              val rhs1 = if (symbol.isGetter)
                gen.mkAttributedRef(target)
              else
                Assign(gen.mkAttributedRef(target), Ident(vparamss.head.head.symbol))
              localTyper.typed(treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt, rhs1))
          }

        case ValDef(mods, name, tpt, rhs) if symbol.hasFlag(SPECIALIZED) =>
          assert(body.isDefinedAt(symbol.alias))
          val tree1 = treeCopy.ValDef(tree, mods, name, tpt, body(symbol.alias).duplicate)
          if (settings.debug.value) log("now typing: " + tree1 + " in " + tree.symbol.owner.fullName)
          val d = new Duplicator
          d.retyped(localTyper.context1.asInstanceOf[d.Context],
                    tree1,
                    symbol.alias.enclClass,
                    symbol.enclClass,
                    typeEnv(symbol.alias) ++ typeEnv(tree.symbol))

        case Apply(sel @ Select(sup @ Super(qual, name), name1), args) =>
          val res = localTyper.typed(
            Apply(Select(Super(qual, name) setPos sup.pos, name1) setPos sel.pos, transformTrees(args)) setPos tree.pos)
          log("retyping call to super, from: " + symbol + " to " + res.symbol)
          res

        case _ =>
          super.transform(tree)
      }
    }

    private def reskolemize(tparams: List[TypeDef]): (List[Symbol], List[Symbol]) = {
      val tparams1 = tparams map (_.symbol)
      localTyper.namer.skolemize(tparams)
      (tparams1, tparams map (_.symbol))
    }

    private def duplicateBody(tree: DefDef, target: Symbol): Tree = {
      val symbol = tree.symbol
      if (settings.debug.value) log("specializing body of" + symbol.fullName + ": " + symbol.info)
      val DefDef(mods, name, tparams, vparamss, tpt, _) = tree
      val (_, origtparams) = splitParams(target.typeParams)
      if (settings.debug.value) log("substituting " + origtparams + " for " + symbol.typeParams)

      // skolemize type parameters
      val (oldtparams, newtparams) = reskolemize(tparams)

      // create fresh symbols for value parameters to hold the skolem types
      val vparamss1 = List(for (vdef <- vparamss.head; param = vdef.symbol) yield {
        ValDef(param.cloneSymbol(symbol).setInfo(param.info.substSym(oldtparams, newtparams)))
      })

      // replace value and type parameters of the old method with the new ones
      val symSubstituter = new ImplementationAdapter(
        parameters(target).flatten ::: origtparams,
        vparamss1.flatten.map(_.symbol) ::: newtparams)
      val adapter = new AdaptSpecializedValues
      val tmp = symSubstituter(adapter(body(target).duplicate))
      tpt.tpe = tpt.tpe.substSym(oldtparams, newtparams)

      val meth = treeCopy.DefDef(tree, mods, name, tparams, vparamss1, tpt, tmp)

      log("now typing: " + meth + " in " + symbol.owner.fullName)
      val d = new Duplicator
      d.retyped(localTyper.context1.asInstanceOf[d.Context],
                meth,
                target.enclClass,
                symbol.enclClass,
                typeEnv(target) ++ typeEnv(symbol))
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
    class ImplementationAdapter(from: List[Symbol], to: List[Symbol]) extends TreeSymSubstituter(from, to) {
      override val symSubst = new SubstSymMap(from, to) {
        override def matches(sym1: Symbol, sym2: Symbol) =
          if (sym2.isTypeSkolem) sym2.deSkolemize eq sym1
          else sym1 eq sym2
      }

      /** All private members that are referenced are made protected,
       *  in order to be accessible from specialized subclasses.
       */
      override def transform(tree: Tree): Tree = tree match {
        case Select(qual, name) =>
          if (tree.symbol.hasFlag(PRIVATE | PROTECTED)) {
            log("changing private flag of " + tree.symbol + " privateWithin: " + tree.symbol.privateWithin)
//            tree.symbol.resetFlag(PRIVATE).setFlag(PROTECTED)
            tree.symbol.makeNotPrivate(tree.symbol.owner)
//            tree.symbol.resetFlag(PRIVATE | PROTECTED)
//            tree.symbol.privateWithin = NoSymbol
          }
          super.transform(tree)

        case _ =>
          super.transform(tree)
      }
    }

    /** Does the given tree need a cast to a type parameter's upper bound?
     *  A cast is needed for values of type A, where A is a specialized type
     *  variable with a non-trivial upper bound. When A is specialized, its
     *  specialization may not satisfy the upper bound. We generate casts to
     *  be able to type check code. Such methods will never be called, as they
     *  are not visible to the user. The compiler will insert such calls only when
     *  the bounds are satisfied.
     */
    private class AdaptSpecializedValues extends Transformer {
      private def needsCast(tree: Tree): Boolean = {
        val sym = tree.tpe.typeSymbol
        (sym.isTypeParameterOrSkolem
         && sym.hasAnnotation(SpecializedClass)
         && sym.info.bounds.hi != definitions.AnyClass.tpe
         /*&& !(tree.tpe <:< sym.info.bounds.hi)*/)
       }

      override def transform(tree: Tree): Tree = {
        val tree1 = super.transform(tree)
        if (needsCast(tree1)) {
//          log("inserting cast for " + tree1 + " tpe: " + tree1.tpe)
//          val tree2 = gen.mkAsInstanceOf(tree1, tree1.tpe.typeSymbol.info.bounds.hi)
//          log(" casted to: " + tree2)
          tree1
        } else
          tree1
      }
      def apply(t: Tree): Tree = transform(t)
    }

    def warn(clazz: Symbol)(pos: Position, err: String) =
      if (!clazz.hasFlag(SPECIALIZED))
        unit.warning(pos, err)

    /** Create trees for specialized members of 'cls', based on the
     *  symbols that are already there.
     */
    private def makeSpecializedMembers(cls: Symbol): List[Tree] = {
      // add special overrides first
      if (!cls.hasFlag(SPECIALIZED))
        for (m <- specialOverrides(cls)) cls.info.decls.enter(m)
      val mbrs = new mutable.ListBuffer[Tree]

      for (m <- cls.info.decls.toList
             if m.hasFlag(SPECIALIZED)
                 && (m.sourceFile ne null)
                 && satisfiable(typeEnv(m), warn(cls))) {
        log("creating tree for " + m.fullName)
        if (m.isMethod)  {
          if (m.isClassConstructor) {
            val origParamss = parameters(info(m).target)
            assert(origParamss.length == 1) // we are after uncurry

            val vparams =
              for ((tp, sym) <- m.info.paramTypes zip origParamss(0))
                yield m.newValue(sym.pos, specializedName(sym, typeEnv(cls)))
                       .setInfo(tp)
                       .setFlag(sym.flags)
            // param accessors for private members (the others are inherited from the generic class)
            for (param <- vparams if cls.info.nonPrivateMember(param.name) == NoSymbol;
                 val acc = param.cloneSymbol(cls).setFlag(PARAMACCESSOR | PRIVATE)) {
              log("param accessor for " + acc.fullName)
              cls.info.decls.enter(acc)
              mbrs += ValDef(acc, EmptyTree).setType(NoType).setPos(m.pos)
            }
            // ctor
            mbrs += atPos(m.pos)(DefDef(m, Modifiers(m.flags), List(vparams) map (_ map ValDef), EmptyTree))
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
      log(buf)
      buf.toList
    }
  }

  private def forwardCall(pos: util.Position, receiver: Tree, paramss: List[List[ValDef]]): Tree = {
    val argss = paramss map (_ map (x => Ident(x.symbol)))
    atPos(pos) { (receiver /: argss) (Apply) }
  }

  /** Concrete methods that use a specialized type, or override such methods. */
  private val concreteSpecMethods: mutable.Set[Symbol] = new mutable.HashSet

  /** Instantiate polymorphic function `target' with type parameters from `from'.
   *  For each type parameter `tp' in `target', its argument is:
   *    - a corresponding type parameter of `from', if tp is not bound in
   *      typeEnv(from)
   *    - the upper bound of tp, if the binding conflicts with tp's bounds
   *    - typeEnv(from)(tp), if the binding is not conflicting in its bounds
   */
  private def makeTypeArguments(from: Symbol, target: Symbol): List[Type] = {
    val owner = from.owner
    val env = typeEnv(from)
    for (tp <- owner.info.memberType(target).typeParams)
      yield
        if (!env.isDefinedAt(tp))
          typeRef(NoPrefix, from.info.typeParams.find(_.name == tp.name).get, Nil)
        else if ((env(tp) <:< tp.info.bounds.hi) && (tp.info.bounds.lo <:< env(tp)))
          env(tp)
        else tp.info.bounds.hi
  }

  private def makeArguments(fun: Symbol, vparams: List[Symbol]): List[Tree] = {
    def needsCast(tp1: Type, tp2: Type): Boolean =
      !(tp1 <:< tp2)

    //! TODO: make sure the param types are seen from the right prefix
    for ((tp, arg) <- fun.info.paramTypes zip vparams) yield {
      if (needsCast(arg.tpe, tp)) {
        //log("tp: " + tp + " " + tp.typeSymbol.owner)
        gen.mkAsInstanceOf(Ident(arg), tp)
      } else Ident(arg)
    }
  }

  private def findSpec(tp: Type): Type = tp match {
    case TypeRef(pre, sym, args) =>
      if (args.isEmpty) tp
      else {
        specializedType(tp)
        /*log("looking for " + specializedName(sym.name, args) + " in " + pre)
        val sym1 = pre.member(specializedName(sym.name, args))
        assert(sym1 != NoSymbol, "pre: " + pre.typeSymbol + " ph: " + phase + " with: " + pre.members)
        TypeRef(pre, sym1, Nil)*/
      }
    case _ => tp
  }

  class SpecializationTransformer(unit: CompilationUnit) extends Transformer {
    override def transform(tree: Tree) =
      atPhase(phase.next) {
        val res = specializeCalls(unit).transform(tree)
        res
      }
  }

}
