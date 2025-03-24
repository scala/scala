/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Scala Center, Lightbend dba Akka, and Mark Harrah
 *
 * Scala (https://www.scala-lang.org)
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package xsbt

import java.util.{ Arrays, Comparator }
import scala.tools.nsc.symtab.Flags
import xsbti.VirtualFile
import xsbti.api._

import scala.annotation.tailrec
import scala.tools.nsc.Global
import scala.PartialFunction.cond

/**
 * Extracts full (including private members) API representation out of Symbols and Types.
 *
 * API for each class is extracted separately. Inner classes are represented as an empty (without members)
 * member of the outer class and as a separate class with full API representation. For example:
 *
 * class A {
 *   class B {
 *     def foo: Int = 123
 *   }
 * }
 *
 * Is represented as:
 *
 * // className = A
 * class A {
 *   class B
 * }
 * // className = A.B
 * class A.B {
 *   def foo: Int
 * }
 *
 * Each compilation unit should be processed by a fresh instance of this class.
 *
 * NOTE: This class extract *full* API representation. In most of other places in the incremental compiler,
 * only non-private (accessible from other compilation units) members are relevant. Other parts of the
 * incremental compiler filter out private definitions before processing API structures. Check SameAPI for
 * an example.
 *
 */
class ExtractAPI[GlobalType <: Global](
    val global: GlobalType,
    // Tracks the source file associated with the CompilationUnit currently being processed by the API phase.
    // This is used when recording inheritance dependencies.
    sourceFile: VirtualFile
) extends Compat
    with ClassName
    with GlobalHelpers {

  import global._

  private def error(msg: String) = throw new RuntimeException(msg)

  // this cache reduces duplicate work both here and when persisting
  //   caches on other structures had minimal effect on time and cache size
  //   (tried: Definition, Modifier, Path, Id, String)
  private[this] val typeCache = perRunCaches.newMap[(Symbol, Type), xsbti.api.Type]()
  // these caches are necessary for correctness
  private[this] val structureCache = perRunCaches.newMap[Symbol, xsbti.api.Structure]()
  private[this] val classLikeCache =
    perRunCaches.newMap[(Symbol, Symbol), xsbti.api.ClassLikeDef]()
  private[this] val pending = perRunCaches.newSet[xsbti.api.Lazy[_]]()

  private[this] val emptyStringArray = Array.empty[String]

  private[this] val allNonLocalClassSymbols = perRunCaches.newSet[Symbol]()
  private[this] val allNonLocalClassesInSrc = perRunCaches.newSet[xsbti.api.ClassLike]()
  private[this] val _mainClasses = perRunCaches.newSet[String]()

  /**
   * Implements a work-around for https://github.com/sbt/sbt/issues/823
   *
   * The strategy is to rename all type variables bound by existential type to stable
   * names by assigning to each type variable a De Bruijn-like index. As a result, each
   * type variable gets name of this shape:
   *
   *   "existential_${nestingLevel}_${i}"
   *
   * where `nestingLevel` indicates nesting level of existential types and `i` variable
   * indicates position of type variable in given existential type.
   *
   * For example, let's assume we have the following classes declared:
   *
   *   class A[T]; class B[T,U]
   *
   * and we have type A[_] that is expanded by Scala compiler into
   *
   *   A[_$1] forSome { type _$1 }
   *
   * After applying our renaming strategy we get
   *
   *   A[existential_0_0] forSome { type existential_0_0 }
   *
   * Let's consider a bit more complicated example which shows how our strategy deals with
   * nested existential types:
   *
   *   A[_ <: B[_, _]]
   *
   * which gets expanded into:
   *
   *   A[_$1] forSome {
   *     type _$1 <: B[_$2, _$3] forSome { type _$2; type _$3 }
   *   }
   *
   * After applying our renaming strategy we get
   *
   *   A[existential_0_0] forSome {
   *     type existential_0_0 <: B[existential_1_0, existential_1_1] forSome {
   *       type existential_1_0; type existential_1_1
   *     }
   *   }
   *
   * Note how the first index (nesting level) is bumped for both existential types.
   *
   * This way, all names of existential type variables depend only on the structure of
   * existential types and are kept stable.
   *
   * Both examples presented above used placeholder syntax for existential types but our
   * strategy is applied uniformly to all existential types no matter if they are written
   * using placeholder syntax or explicitly.
   */
  private[this] object existentialRenamings {
    private var nestingLevel: Int = 0
    import scala.collection.mutable.Map
    private val renameTo: Map[Symbol, String] = Map.empty

    def leaveExistentialTypeVariables(typeVariables: Seq[Symbol]): Unit = {
      nestingLevel -= 1
      assert(nestingLevel >= 0, s"nestingLevel = $nestingLevel")
      typeVariables.foreach(renameTo.remove)
    }
    def enterExistentialTypeVariables(typeVariables: Seq[Symbol]): Unit = {
      nestingLevel += 1
      typeVariables.zipWithIndex foreach {
        case (tv, i) =>
          val newName = "existential_" + nestingLevel + "_" + i
          renameTo(tv) = newName
      }
    }
    def renaming(symbol: Symbol): Option[String] = renameTo.get(symbol)
  }

  /**
   * Construct a lazy instance from a by-name parameter that will null out references to once
   * the value is forced and therefore references to thunk's classes will be garbage collected.
   */
  private def lzy[S <: AnyRef](s: => S): xsbti.api.Lazy[S] = {
    val lazyImpl = xsbti.api.SafeLazy.apply(Message(s))
    pending += lazyImpl
    lazyImpl
  }

  /**
   * Force all lazy structures.  This is necessary so that we see the symbols/types at this phase and
   * so that we don't hold on to compiler objects and classes
   */
  @tailrec final def forceStructures(): Unit =
    if (pending.isEmpty)
      structureCache.clear()
    else {
      val toProcess = pending.toList
      pending.clear()
      toProcess foreach { _.get() }
      forceStructures()
    }

  private def thisPath(sym: Symbol) = path(pathComponents(sym, Constants.thisPath :: Nil))
  private def path(components: List[PathComponent]) =
    xsbti.api.Path.of(components.toArray[PathComponent])
  @tailrec
  private def pathComponents(sym: Symbol, postfix: List[PathComponent]): List[PathComponent] = {
    if (sym == NoSymbol || sym.isRoot || sym.isEmptyPackageClass || sym.isRootPackage) postfix
    else pathComponents(sym.owner, xsbti.api.Id.of(simpleName(sym)) :: postfix)
  }
  private def types(in: Symbol, t: List[Type]): Array[xsbti.api.Type] =
    t.toArray[Type].map(processType(in, _))
  private def projectionType(in: Symbol, pre: Type, sym: Symbol) = {
    if (pre == NoPrefix) {
      if (sym.isLocalClass || sym.isRoot || sym.isRootPackage) Constants.emptyType
      else if (sym.isTypeParameterOrSkolem || sym.isExistentiallyBound) reference(sym)
      else {
        // this appears to come from an existential type in an inherited member- not sure why isExistential is false here
        /*println("Warning: Unknown prefixless type: " + sym + " in " + sym.owner + " in " + sym.enclClass)
      println("\tFlags: " + sym.flags + ", istype: " + sym.isType + ", absT: " + sym.isAbstractType + ", alias: " + sym.isAliasType + ", nonclass: " + isNonClassType(sym))*/
        reference(sym)
      }
    } else if (sym.isRoot || sym.isRootPackage) Constants.emptyType
    else xsbti.api.Projection.of(processType(in, pre), simpleName(sym))
  }
  private def reference(sym: Symbol): xsbti.api.ParameterRef =
    xsbti.api.ParameterRef.of(tparamID(sym))

  // Constructing PrintWriters can cause lock contention in highly parallel code,
  // it's constructor looks up the "line.separator" system property which locks
  // on JDK 8.
  //
  // We can safely reuse a single instance, avoiding the lock contention and
  // also reducing allocations a little.
  private object ReusableTreePrinter {
    import java.io._
    private val buffer = new StringWriter()
    private val printWriter = new PrintWriter(buffer)
    private val treePrinter = newTreePrinter(printWriter)

    /** More efficient version of trees.mkString(start, sep, end) */
    def mkString(trees: List[Tree], start: String, sep: String, end: String): String = {
      var rest: List[Tree] = trees
      printWriter.append(start)
      while (rest != Nil) {
        treePrinter.printTree(rest.head)
        rest = rest.tail
        if (rest != Nil) {
          printWriter.append(sep)
        }
      }
      printWriter.append(end)
      val result = getAndResetBuffer()
      val benchmark = trees.mkString(start, sep, end)
      assert(result == benchmark, List(result, benchmark).mkString("[", "|", "]"))
      result
    }
    private def getAndResetBuffer(): String = {
      printWriter.flush()
      try buffer.getBuffer.toString
      finally buffer.getBuffer.setLength(0)
    }
  }

  // The compiler only pickles static annotations, so only include these in the API.
  // This way, the API is not sensitive to whether we compiled from source or loaded from classfile.
  // (When looking at the sources we see all annotations, but when loading from classes we only see the pickled (static) ones.)
  private def mkAnnotations(in: Symbol, as: List[AnnotationInfo]): Array[xsbti.api.Annotation] = {
    if (in == NoSymbol) ExtractAPI.emptyAnnotationArray
    else
      staticAnnotations(as) match {
        case Nil => ExtractAPI.emptyAnnotationArray
        case staticAs =>
          staticAs.map { a =>
            xsbti.api.Annotation.of(
              processType(in, a.atp),
              if (a.assocs.isEmpty)
                Array(
                  xsbti.api.AnnotationArgument
                    .of("", ReusableTreePrinter.mkString(a.args, "(", ",", ")"))
                ) // what else to do with a Tree?
              else
                a.assocs
                  .map {
                    case (name, value) =>
                      xsbti.api.AnnotationArgument.of(name.toString, value.toString)
                  }
                  .toArray[xsbti.api.AnnotationArgument]
            )
          }.toArray
      }
  }

  // HOT method, hand optimized to reduce allocations and needless creation of Names with calls to getterIn/setterIn
  // on non-fields.
  private def annotations(in: Symbol, s: Symbol): Array[xsbti.api.Annotation] = {
    val saved = phase
    phase = currentRun.typerPhase
    try {
      val base = if (s.hasFlag(Flags.ACCESSOR)) s.accessed else NoSymbol
      val b = if (base == NoSymbol) s else base
      // annotations from bean methods are not handled because:
      //  a) they are recorded as normal source methods anyway
      //  b) there is no way to distinguish them from user-defined methods
      if (b.hasGetter) {
        val annotations = collection.mutable.LinkedHashSet[xsbti.api.Annotation]()
        def add(sym: Symbol) = if (sym != NoSymbol) {
          val anns = mkAnnotations(in, sym.annotations)
          var i = 0
          while (i < anns.length) {
            annotations += anns(i)
            i += 1
          }
        }
        add(b)
        add(b.getterIn(b.enclClass))
        add(b.setterIn(b.enclClass))
        annotations.toArray
      } else {
        if (b.annotations.isEmpty) ExtractAPI.emptyAnnotationArray
        else mkAnnotations(in, b.annotations)
      }
    } finally {
      phase = saved
    }
  }

  private def viewer(s: Symbol) = (if (s.isModule) s.moduleClass else s).thisType

  private def defDef(in: Symbol, s: Symbol): xsbti.api.Def = {
    @tailrec
    def build(
        t: Type,
        typeParams: Array[xsbti.api.TypeParameter],
        valueParameters: List[xsbti.api.ParameterList]
    ): xsbti.api.Def = {
      def parameterList(syms: List[Symbol]): xsbti.api.ParameterList = {
        val isImplicitList = cond(syms) { case head :: _ => isImplicit(head) }
        xsbti.api.ParameterList.of(syms.map(parameterS).toArray, isImplicitList)
      }
      t match {
        case PolyType(typeParams0, base) =>
          assert(typeParams.isEmpty, typeParams.toString)
          assert(valueParameters.isEmpty, valueParameters.toString)
          build(base, typeParameters(in, typeParams0), Nil)
        case MethodType(params, resultType) =>
          build(resultType, typeParams, parameterList(params) :: valueParameters)
        case NullaryMethodType(resultType) =>
          build(resultType, typeParams, valueParameters)
        case returnType =>
          val retType = processType(in, dropConst(returnType))
          xsbti.api.Def.of(
            simpleName(s),
            getAccess(s),
            getModifiers(s),
            annotations(in, s),
            typeParams,
            valueParameters.reverse.toArray,
            retType
          )
      }
    }
    def parameterS(s: Symbol): xsbti.api.MethodParameter = {
      val tp: global.Type = s.info
      makeParameter(simpleName(s), tp, tp.typeSymbol, s)
    }

    def makeParameter(
        name: String,
        tpe: Type,
        ts: Symbol,
        paramSym: Symbol
    ): xsbti.api.MethodParameter = {
      import xsbti.api.ParameterModifier._
      val (t, special) =
        if (ts == definitions.RepeatedParamClass) // || s == definitions.JavaRepeatedParamClass)
          (tpe.typeArgs.head, Repeated)
        else if (ts == definitions.ByNameParamClass)
          (tpe.typeArgs.head, ByName)
        else
          (tpe, Plain)
      xsbti.api.MethodParameter.of(name, processType(in, t), hasDefault(paramSym), special)
    }
    val t = viewer(in).memberInfo(s)
    build(t, Array(), Nil)
  }
  private def hasDefault(s: Symbol) = s != NoSymbol && s.hasFlag(Flags.DEFAULTPARAM)
  private def fieldDef[T](
      in: Symbol,
      s: Symbol,
      keepConst: Boolean,
      create: (
          String,
          xsbti.api.Access,
          xsbti.api.Modifiers,
          Array[xsbti.api.Annotation],
          xsbti.api.Type
      ) => T
  ): T = {
    val t = dropNullary(viewer(in).memberType(s))
    val t2 = if (keepConst) t else dropConst(t)
    create(simpleName(s), getAccess(s), getModifiers(s), annotations(in, s), processType(in, t2))
  }
  private def dropConst(t: Type): Type = t match {
    case ConstantType(constant) => constant.tpe
    case _                      => t
  }
  private def dropNullary(t: Type): Type = t match {
    case NullaryMethodType(un) => un
    case _                     => t
  }

  private def typeDef(in: Symbol, s: Symbol): xsbti.api.TypeMember = {
    val (typeParams, tpe) =
      viewer(in).memberInfo(s) match {
        case PolyType(typeParams0, base) => (typeParameters(in, typeParams0), base)
        case t                           => (Array[xsbti.api.TypeParameter](), t)
      }
    val name = simpleName(s)
    val access = getAccess(s)
    val modifiers = getModifiers(s)
    val as = annotations(in, s)

    if (s.isAliasType)
      xsbti.api.TypeAlias.of(name, access, modifiers, as, typeParams, processType(in, tpe))
    else if (s.isAbstractType) {
      val bounds = tpe.bounds
      xsbti.api.TypeDeclaration.of(
        name,
        access,
        modifiers,
        as,
        typeParams,
        processType(in, bounds.lo),
        processType(in, bounds.hi)
      )
    } else
      error("Unknown type member" + s)
  }

  private def structure(info: Type, s: Symbol): xsbti.api.Structure =
    structureCache.getOrElseUpdate(s, mkStructure(info, s))
  private def structureWithInherited(info: Type, s: Symbol): xsbti.api.Structure =
    structureCache.getOrElseUpdate(s, mkStructureWithInherited(info, s))

  private def removeConstructors(ds: List[Symbol]): List[Symbol] = ds filter { !_.isConstructor }

  /**
   * Create structure as-is, without embedding ancestors
   *
   * (for refinement types, and ClassInfoTypes encountered outside of a definition???).
   */
  private def mkStructure(info: Type, s: Symbol): xsbti.api.Structure = {
    // We're not interested in the full linearization, so we can just use `parents`,
    // which side steps issues with baseType when f-bounded existential types and refined types mix
    // (and we get cyclic types which cause a stack overflow in showAPI).
    val parentTypes = info.parents
    val decls = info.decls.toList
    val declsNoModuleCtor = if (s.isModuleClass) removeConstructors(decls) else decls
    mkStructure(s, parentTypes, declsNoModuleCtor, Nil)
  }

  /**
   * Track all ancestors and inherited members for a class's API.
   *
   * A class's hash does not include hashes for its parent classes -- only the symbolic names --
   * so we must ensure changes propagate somehow.
   *
   * TODO: can we include hashes for parent classes instead? This seems a bit messy.
   */
  private def mkStructureWithInherited(info: Type, s: Symbol): xsbti.api.Structure = {
    val ancestorTypes0 = linearizedAncestorTypes(info)
    val ancestorTypes =
      if (s.isDerivedValueClass) {
        val underlying = s.derivedValueClassUnbox.tpe.finalResultType
        // The underlying type of a value class should be part of the name hash
        // of the value class (see the test `value-class-underlying`), this is accomplished
        // by adding the underlying type to the list of parent types.
        underlying :: ancestorTypes0
      } else
        ancestorTypes0
    val decls = info.decls.toList
    val declsNoModuleCtor = if (s.isModuleClass) removeConstructors(decls) else decls
    val declSet = decls.toSet
    val inherited =
      info.nonPrivateMembers.toList.filterNot(declSet) // private members are not inherited
    mkStructure(s, ancestorTypes, declsNoModuleCtor, inherited)
  }

  // Note that the ordering of classes in `baseClasses` is important.
  // It would be easier to just say `baseTypeSeq.toList.tail`,
  // but that does not take linearization into account.
  def linearizedAncestorTypes(info: Type): List[Type] = info.baseClasses.tail.map(info.baseType)

  private def mkStructure(
      s: Symbol,
      bases: List[Type],
      declared: List[Symbol],
      inherited: List[Symbol]
  ): xsbti.api.Structure = {
    xsbti.api.Structure.of(
      lzy(types(s, bases)),
      lzy(processDefinitions(s, declared)),
      lzy(processDefinitions(s, inherited))
    )
  }
  private def processDefinitions(in: Symbol, defs: List[Symbol]): Array[xsbti.api.ClassDefinition] =
    sort(defs.toArray).flatMap((d: Symbol) => definition(in, d))
  private[this] def sort(defs: Array[Symbol]): Array[Symbol] = {
    Arrays.sort(defs, sortClasses)
    defs
  }

  private def definition(in: Symbol, sym: Symbol): Option[xsbti.api.ClassDefinition] = {
    def mkVar = Some(fieldDef(in, sym, keepConst = false, xsbti.api.Var.of(_, _, _, _, _)))
    def mkVal = Some(fieldDef(in, sym, keepConst = true, xsbti.api.Val.of(_, _, _, _, _)))
    if (isClass(sym))
      if (ignoreClass(sym)) {
        allNonLocalClassSymbols.+=(sym); None
      } else Some(classLike(in, sym))
    else if (sym.isNonClassType)
      Some(typeDef(in, sym))
    else if (sym.isVariable)
      if (isSourceField(sym)) mkVar else None
    else if (sym.isStable)
      if (isSourceField(sym)) mkVal else None
    else if (sym.isSourceMethod && !sym.isSetter)
      if (sym.isGetter) mkVar else Some(defDef(in, sym))
    else
      None
  }
  private def ignoreClass(sym: Symbol): Boolean =
    sym.isLocalClass || sym.isAnonymousClass || sym.fullName.endsWith(tpnme.LOCAL_CHILD.toString)

  // This filters private[this] vals/vars that were not in the original source.
  //  The getter will be used for processing instead.
  private def isSourceField(sym: Symbol): Boolean = {
    val getter = sym.getterIn(sym.enclClass)
    // the check `getter eq sym` is a precaution against infinite recursion
    // `isParamAccessor` does not exist in all supported versions of Scala, so the flag check is done directly
    (getter == NoSymbol && !sym.hasFlag(Flags.PARAMACCESSOR)) || (getter eq sym)
  }
  private def getModifiers(s: Symbol): xsbti.api.Modifiers = {
    import Flags._
    val absOver = s.hasFlag(ABSOVERRIDE)
    val abs = s.hasFlag(ABSTRACT) || s.hasFlag(DEFERRED) || absOver
    val over = s.hasFlag(OVERRIDE) || absOver
    new xsbti.api.Modifiers(
      abs,
      over,
      s.isFinal,
      s.hasFlag(SEALED),
      isImplicit(s),
      s.hasFlag(LAZY),
      s.hasFlag(MACRO),
      s.hasFlag(SUPERACCESSOR)
    )
  }

  private def isImplicit(s: Symbol) = s.hasFlag(Flags.IMPLICIT)
  private def getAccess(c: Symbol): xsbti.api.Access = {
    if (c.isPublic) Constants.public
    else if (c.isPrivateLocal) Constants.privateLocal
    else if (c.isProtectedLocal) Constants.protectedLocal
    else {
      val within = c.privateWithin
      val qualifier =
        if (within == NoSymbol) Constants.unqualified
        else xsbti.api.IdQualifier.of(within.fullName)
      if (c.hasFlag(Flags.PROTECTED)) xsbti.api.Protected.of(qualifier)
      else xsbti.api.Private.of(qualifier)
    }
  }

  /**
   * Replace all types that directly refer to the `forbidden` symbol by `NoType`.
   * (a specialized version of substThisAndSym)
   */
  class SuppressSymbolRef(forbidden: Symbol) extends TypeMap {
    def apply(tp: Type) =
      if (tp.typeSymbolDirect == forbidden) NoType
      else mapOver(tp)
  }

  private def processType(in: Symbol, t: Type): xsbti.api.Type =
    typeCache.getOrElseUpdate((in, t), makeType(in, t))
  private def makeType(in: Symbol, t: Type): xsbti.api.Type = {

    val dealiased = t match {
      case TypeRef(_, sym, _) if sym.isAliasType => t.dealias
      case _                                     => t
    }

    dealiased match {
      case NoPrefix             => Constants.emptyType
      case ThisType(sym)        => xsbti.api.Singleton.of(thisPath(sym))
      case SingleType(pre, sym) => projectionType(in, pre, sym)
      case ConstantType(constant) =>
        xsbti.api.Constant.of(processType(in, constant.tpe), constant.stringValue)

      /* explaining the special-casing of references to refinement classes (https://support.typesafe.com/tickets/1882)
       *
       * goal: a representation of type references to refinement classes that's stable across compilation runs
       *       (and thus insensitive to typing from source or unpickling from bytecode)
       *
       * problem: the current representation, which corresponds to the owner chain of the refinement:
       *   1. is affected by pickling, so typing from source or using unpickled symbols give different results (because the unpickler "localizes" owners -- this could be fixed in the compiler)
       *   2. can't distinguish multiple refinements in the same owner (this is a limitation of SBT's internal representation and cannot be fixed in the compiler)
       *
       * potential solutions:
       *   - simply drop the reference: won't work as collapsing all refinement types will cause recompilation to be skipped when a refinement is changed to another refinement
       *   - represent the symbol in the api: can't think of a stable way of referring to an anonymous symbol whose owner changes when pickled
       *   + expand the reference to the corresponding refinement type: doing that recursively may not terminate, but we can deal with that by approximating recursive references
       *     (all we care about is being sound for recompilation: recompile iff a dependency changes, and this will happen as long as we have one unrolling of the reference to the refinement)
       */
      case TypeRef(pre, sym, Nil) if sym.isRefinementClass =>
        // Since we only care about detecting changes reliably, we unroll a reference to a refinement class once.
        // Recursive references are simply replaced by NoType -- changes to the type will be seen in the first unrolling.
        // The API need not be type correct, so this truncation is acceptable. Most of all, the API should be compact.
        val unrolling = pre.memberInfo(sym) // this is a refinement type

        // in case there are recursive references, suppress them -- does this ever happen?
        // we don't have a test case for this, so warn and hope we'll get a contribution for it :-)
        val withoutRecursiveRefs = new SuppressSymbolRef(sym).mapOver(unrolling)
        if (unrolling ne withoutRecursiveRefs)
          reporter.warning(
            sym.pos,
            "sbt-api: approximated refinement ref" + t + " (== " + unrolling + ") to " + withoutRecursiveRefs + "\nThis is currently untested, please report the code you were compiling."
          )

        structure(withoutRecursiveRefs, sym)
      case tr @ TypeRef(pre, sym, args) =>
        val base = projectionType(in, pre, sym)
        if (args.isEmpty)
          if (isRawType(tr))
            processType(in, rawToExistential(tr))
          else
            base
        else
          xsbti.api.Parameterized.of(base, types(in, args))
      case SuperType(thistpe: Type, supertpe: Type) =>
        reporter.warning(
          NoPosition,
          "sbt-api: Super type (not implemented): this=" + thistpe + ", super=" + supertpe
        )
        Constants.emptyType
      case at: AnnotatedType =>
        at.annotations match {
          case Nil => processType(in, at.underlying)
          case annots =>
            xsbti.api.Annotated.of(processType(in, at.underlying), mkAnnotations(in, annots))
        }
      case rt: CompoundType   => structure(rt, rt.typeSymbol)
      case et: ExistentialType => makeExistentialType(in, et)
      case NoType =>
        Constants.emptyType // this can happen when there is an error that will be reported by a later phase
      case PolyType(typeParams, resultType) =>
        xsbti.api.Polymorphic.of(processType(in, resultType), typeParameters(in, typeParams))
      case NullaryMethodType(_) =>
        reporter.warning(
          NoPosition,
          "sbt-api: Unexpected nullary method type " + in + " in " + in.owner
        )
        Constants.emptyType
      case MethodType(_, _) =>
        reporter.echo(NoPosition, s"sbt-api: Unhandled method type $in in ${in.owner}")
        Constants.emptyType
      case _ =>
        reporter.warning(NoPosition, "sbt-api: Unhandled type " + t.getClass + " : " + t)
        Constants.emptyType
    }
  }
  private def makeExistentialType(in: Symbol, t: ExistentialType): xsbti.api.Existential = {
    val ExistentialType(typeVariables, qualified) = t
    existentialRenamings.enterExistentialTypeVariables(typeVariables)
    try {
      val typeVariablesConverted = typeParameters(in, typeVariables)
      val qualifiedConverted = processType(in, qualified)
      xsbti.api.Existential.of(qualifiedConverted, typeVariablesConverted)
    } finally {
      existentialRenamings.leaveExistentialTypeVariables(typeVariables)
    }
  }
  private def typeParameters(in: Symbol, s: Symbol): Array[xsbti.api.TypeParameter] =
    typeParameters(in, s.typeParams)
  private def typeParameters(in: Symbol, s: List[Symbol]): Array[xsbti.api.TypeParameter] =
    s.map(typeParameter(in, _)).toArray[xsbti.api.TypeParameter]
  private def typeParameter(in: Symbol, s: Symbol): xsbti.api.TypeParameter = {
    val varianceInt = s.variance
    import xsbti.api.Variance._
    val annots = annotations(in, s)
    val variance =
      if (varianceInt < 0) Contravariant else if (varianceInt > 0) Covariant else Invariant
    viewer(in).memberInfo(s) match {
      case TypeBounds(low, high) =>
        xsbti.api.TypeParameter.of(
          tparamID(s),
          annots,
          typeParameters(in, s),
          variance,
          processType(in, low),
          processType(in, high)
        )
      case PolyType(typeParams, base) =>
        xsbti.api.TypeParameter.of(
          tparamID(s),
          annots,
          typeParameters(in, typeParams),
          variance,
          processType(in, base.bounds.lo),
          processType(in, base.bounds.hi)
        )
      case x => error("Unknown type parameter info: " + x.getClass)
    }
  }
  private def tparamID(s: Symbol): String =
    existentialRenamings.renaming(s) match {
      case Some(rename) =>
        debuglog(s"Renaming existential type variable ${s.fullName} to $rename")
        rename
      case None =>
        s.fullName
    }

  /* Representation for the self type of a class symbol `s`, or `emptyType` for an *unascribed* self variable (or no self variable at all).
     Only the self variable's explicitly ascribed type is relevant for incremental compilation. */
  private def selfType(in: Symbol, s: Symbol): xsbti.api.Type =
    // `sym.typeOfThis` is implemented as `sym.thisSym.info`, which ensures the *self* symbol is initialized (the type completer is run).
    // We can safely avoid running the type completer for `thisSym` for *class* symbols where `thisSym == this`,
    // as that invariant is established on completing the class symbol (`mkClassLike` calls `s.initialize` before calling us).
    // Technically, we could even ignore a self type that's a supertype of the class's type,
    // as it does not contribute any information relevant outside of the class definition.
    if ((s.thisSym eq s) || (s.thisSym.tpeHK == s.tpeHK)) Constants.emptyType
    else processType(in, s.typeOfThis)

  def extractAllClassesOf(in: Symbol, c: Symbol): Unit = {
    classLike(in, c)
    ()
  }

  def allExtractedNonLocalClasses: Set[ClassLike] = {
    forceStructures()
    allNonLocalClassesInSrc.toSet
  }

  def allExtractedNonLocalSymbols: Set[Symbol] = allNonLocalClassSymbols.toSet

  def mainClasses: Set[String] = {
    forceStructures()
    _mainClasses.toSet
  }

  private def classLike(in: Symbol, c: Symbol): ClassLikeDef =
    classLikeCache.getOrElseUpdate((in, c), mkClassLike(in, c))
  private def mkClassLike(in: Symbol, c: Symbol): ClassLikeDef = {
    // Normalize to a class symbol, and initialize it.
    // (An object -- aka module -- also has a term symbol,
    //  but it's the module class that holds the info about its structure.)
    val sym = (if (c.isModule) c.moduleClass else c).initialize
    val defType =
      if (sym.isTrait) DefinitionType.Trait
      else if (sym.isModuleClass) {
        if (sym.isPackageObjectClass) DefinitionType.PackageModule
        else DefinitionType.Module
      } else DefinitionType.ClassDef
    val childrenOfSealedClass = sort(sym.sealedDescendants.toArray).map(c => processType(c, c.tpe))
    val topLevel = sym.owner.isPackageClass
    val anns = annotations(in, c)
    val modifiers = getModifiers(c)
    val acc = getAccess(c)
    val name = classNameAsSeenIn(in, c)
    val tParams = typeParameters(in, sym) // look at class symbol
    val selfType = lzy(this.selfType(in, sym))
    def constructClass(structure: xsbti.api.Lazy[Structure]): ClassLike = {
      xsbti.api.ClassLike.of(
        name,
        acc,
        modifiers,
        anns,
        defType,
        selfType,
        structure,
        emptyStringArray,
        childrenOfSealedClass,
        topLevel,
        tParams
      ) // use original symbol (which is a term symbol when `c.isModule`) for `name` and other non-classy stuff
    }
    val info = viewer(in).memberInfo(sym)
    val structure = lzy(structureWithInherited(info, sym))
    val classWithMembers = constructClass(structure)

    allNonLocalClassesInSrc += classWithMembers
    allNonLocalClassSymbols += sym

    if (sym.isStatic && defType == DefinitionType.Module && definitions.hasJavaMainMethod(sym)) {
      _mainClasses += name
    }

    val classDef = xsbti.api.ClassLikeDef.of(
      name,
      acc,
      modifiers,
      anns,
      tParams,
      defType
    ) // use original symbol (which is a term symbol when `c.isModule`) for `name` and other non-classy stuff
    classDef
  }

  // TODO: could we restrict ourselves to classes, ignoring the term symbol for modules,
  // since everything we need to track about a module is in the module's class (`moduleSym.moduleClass`)?
  private[this] def isClass(s: Symbol) = s.isClass || s.isModule
  // necessary to ensure a stable ordering of classes in the definitions list:
  //  modules and classes come first and are sorted by name
  // all other definitions come later and are not sorted
  private[this] val sortClasses = new Comparator[Symbol] {
    def compare(a: Symbol, b: Symbol) = {
      val aIsClass = isClass(a)
      val bIsClass = isClass(b)
      if (aIsClass == bIsClass)
        if (aIsClass)
          if (a.isModule == b.isModule)
            a.fullName.compareTo(b.fullName)
          else if (a.isModule)
            -1
          else
            1
        else
          0 // substantial performance hit if fullNames are compared here
      else if (aIsClass)
        -1
      else
        1
    }
  }
  private object Constants {
    val local = xsbti.api.ThisQualifier.of()
    val public = xsbti.api.Public.of()
    val privateLocal = xsbti.api.Private.of(local)
    val protectedLocal = xsbti.api.Protected.of(local)
    val unqualified = xsbti.api.Unqualified.of()
    val emptyPath = xsbti.api.Path.of(Array())
    val thisPath = xsbti.api.This.of()
    val emptyType = xsbti.api.EmptyType.of()
  }

  private def simpleName(s: Symbol): String = {
    val n = s.unexpandedName
    val n2 = if (n == nme.CONSTRUCTOR) constructorNameAsString(s.enclClass) else n.decode.toString
    n2.trim
  }

  private def staticAnnotations(annotations: List[AnnotationInfo]): List[AnnotationInfo] =
    if (annotations == Nil) Nil
    else {
      // `isStub` for scala/bug#11679: annotations of inherited members may be absent from the compile time
      // classpath so avoid calling `isNonBottomSubClass` on these stub symbols which would trigger an error.
      //
      // `initialize` for sbt/zinc#998: 2.13 identifies Java annotations by flags. Up to 2.13.6, this is done
      // without forcing the info of `ann.atp.typeSymbol`, flags are missing it's still a `ClassfileLoader`.
      annotations.filter(ann =>
        !isStub(ann.atp.typeSymbol) && { ann.atp.typeSymbol.initialize; ann.isStatic }
      )
    }

  private def isStub(sym: Symbol): Boolean = sym match {
    case _: StubSymbol => true
    case _             => false
  }
}

object ExtractAPI {
  private val emptyAnnotationArray = new Array[xsbti.api.Annotation](0)
}
