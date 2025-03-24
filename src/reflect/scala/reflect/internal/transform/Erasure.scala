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

package scala
package reflect
package internal
package transform

import scala.annotation.tailrec

trait Erasure {

  // FIXME: With `global` as a `val`, implementers must use early initializers, which
  //        are deprecated and will not be supported in 3.0. Please change the design,
  //        remove the early initializers from implementers, and then remove the
  //        `@nowarn` annotations from implementers.
  val global: SymbolTable
  import global._
  import definitions._

  /** An extractor object for generic arrays */
  object GenericArray {

    /** Is `tp` an unbounded generic type (i.e. which could be instantiated
     *  with primitive as well as class types)?.
     */
    @tailrec
    private def genericCore(tp: Type): Type = tp.dealiasWiden match {
      /* A Java Array<T> is erased to Array[Object] (T can only be a reference type), where as a Scala Array[T] is
       * erased to Object. However, there is only symbol for the Array class. So to make the distinction between
       * a Java and a Scala array, we check if the owner of T comes from a Java class.
       * This however caused issue scala/bug#5654. The additional test for EXISTENTIAL fixes it, see the ticket comments.
       * In short, members of an existential type (e.g. `T` in `forSome { type T }`) can have pretty arbitrary
       * owners (e.g. when computing lubs, <root> is used). All packageClass symbols have `isJavaDefined == true`.
       */
      case TypeRef(_, sym, _) if sym.isAbstractType && (!sym.owner.isJavaDefined || sym.hasFlag(Flags.EXISTENTIAL)) =>
        tp
      case ExistentialType(tparams, restp) =>
        genericCore(restp)
      case _ =>
        NoType
    }

    /** If `tp` is of the form Array[...Array[T]...] where `T` is an abstract type
     *  then Some((N, T)) where N is the number of Array constructors enclosing `T`,
     *  otherwise None. Existentials on any level are ignored.
     */
    def unapply(tp: Type): Option[(Int, Type)] = tp.dealiasWiden match {
      case TypeRef(_, ArrayClass, List(arg)) =>
        genericCore(arg) match {
          case NoType =>
            unapply(arg) match {
              case Some((level, core)) => Some((level + 1, core))
              case None => None
            }
          case core =>
            Some((1, core))
        }
      case ExistentialType(tparams, restp) =>
        unapply(restp)
      case _ =>
        None
    }
  }

  /** Arrays despite their finality may turn up as refined type parents,
   *  e.g. with "tagged types" like Array[Int] with T.
   */
  def unboundedGenericArrayLevel(tp: Type): Int = tp match {
    case GenericArray(level, core) if !(core <:< AnyRefTpe || core.upperBound == ObjectTpeJava) => level
    case RefinedType(ps, _) if ps.nonEmpty                  => logResult(s"Unbounded generic level for $tp is")(unboundedGenericArrayLevel(intersectionDominator(ps)))
    case _                                                  => 0
  }

  // @M #2585 when generating a java generic signature that includes
  // a selection of an inner class p.I, (p = `pre`, I = `cls`) must
  // rewrite to p'.I, where p' refers to the class that directly defines
  // the nested class I.
  //
  // See also #2585 marker in javaSig: there, type arguments must be
  // included (use pre.baseType(cls.owner)).
  //
  // This requires that cls.isClass.
  protected def rebindInnerClass(pre: Type, cls: Symbol): Type =
    if (cls.isTopLevel || cls.isLocalToBlock) pre else cls.owner.tpe_*

  /** The type of the argument of a value class reference after erasure
   *  This method needs to be called at a phase no later than erasurephase
   */
  def erasedValueClassArg(tref: TypeRef): Type = {
    assert(!phase.erasedTypes, "Types are erased")
    val clazz = tref.sym
    if (valueClassIsParametric(clazz)) {
      val erasureMap = if (clazz.isScala3Defined) boxing3Erasure else boxingErasure
      erasureMap(tref.memberType(clazz.derivedValueClassUnbox).resultType)
    } else {
      val erasureMap = if (clazz.isScala3Defined) scala3Erasure else scalaErasure
      erasureMap(underlyingOfValueClass(clazz))
    }
  }

  /** Does this value class have an underlying type that's a type parameter of
   *  the class itself?
   *  This method needs to be called at a phase no later than erasurephase
   */
  def valueClassIsParametric(clazz: Symbol): Boolean = {
    assert(!phase.erasedTypes, "valueClassIsParametric called after erasure")
    clazz.typeParams contains clazz.derivedValueClassUnbox.tpe.resultType.typeSymbol
  }

  abstract class ErasureMap extends TypeMap {
    def mergeParents(parents: List[Type]): Type
    def eraseArray(arrayRef: Type, pre: Type, args: List[Type]): Type

    def eraseNormalClassRef(tref: TypeRef): Type = {
      val TypeRef(pre, clazz, args) = tref
      val pre1 = apply(rebindInnerClass(pre, clazz))
      val args1 = Nil
      if ((pre eq pre1) && (args eq args1)) tref // OPT
      else typeRef(pre1, clazz, args1) // #2585
    }

    protected def eraseDerivedValueClassRef(tref: TypeRef): Type = erasedValueClassArg(tref)

    def apply(tp: Type): Type = tp match {
      case FoldableConstantType(ct) =>
        // erase classOf[List[_]] to classOf[List]. special case for classOf[Unit], avoid erasing to classOf[BoxedUnit].
        if (ct.tag == ClazzTag && ct.typeValue.typeSymbol != UnitClass) ConstantType(Constant(apply(ct.typeValue)))
        else tp
      case st: ThisType if st.sym.isPackageClass =>
        tp
      case st: SubType =>
        apply(st.supertype)
      case tref @ TypeRef(pre, sym, args) =>
        def isDottyEnumSingleton(sym: Symbol): Boolean =
          sym.isScala3Defined && sym.isModuleClass && sym.sourceModule.hasAttachment[DottyEnumSingleton.type]
        if (sym eq ArrayClass) eraseArray(tp, pre, args)
        else if ((sym eq AnyClass) || (sym eq AnyValClass) || (sym eq SingletonClass)) ObjectTpe
        else if (sym eq UnitClass) BoxedUnitTpe
        else if (sym.isRefinementClass) apply(mergeParents(tp.parents))
        else if (sym.isDerivedValueClass) eraseDerivedValueClassRef(tref)
        else if (isDottyEnumSingleton(sym)) apply(mergeParents(tp.parents)) // TODO [tasty]: dotty enum singletons are not modules.
        else if (sym.isClass) eraseNormalClassRef(tref)
        else apply(transparentDealias(sym, pre, sym.owner)) // alias type or abstract type (including opaque type)
      case PolyType(tparams, restpe) =>
        apply(restpe)
      case ExistentialType(tparams, restpe) =>
        apply(restpe)
      case mt @ MethodType(params, restpe) =>
        MethodType(
          cloneSymbolsAndModify(params, ErasureMap.this),
          if (restpe.typeSymbol == UnitClass) UnitTpe
          // this replaces each typeref that refers to an argument
          // by the type `p.tpe` of the actual argument p (p in params)
          else apply(mt.resultTypeOwnParamTypes))
      case RefinedType(parents, decls) =>
        apply(mergeParents(parents))
      case AnnotatedType(_, atp) =>
        apply(atp)
      case ClassInfoType(parents, decls, clazz) =>
        val newParents =
          if (parents.isEmpty || (clazz eq ObjectClass) || isPrimitiveValueClass(clazz)) Nil
          else if (clazz eq ArrayClass) ObjectTpe :: Nil
          else {
            val erasedParents = parents mapConserve this

            // drop first parent for traits -- it has been normalized to a class by now,
            // but we should drop that in bytecode
            if (clazz.hasFlag(Flags.TRAIT) && !clazz.hasFlag(Flags.JAVA))
              ObjectTpe :: erasedParents.tail.filter(_.typeSymbol ne ObjectClass)
            else erasedParents
          }
        if (newParents eq parents) tp
        else ClassInfoType(newParents, decls, clazz)

      // A BoundedWildcardType, e.g., can happen while this map is being used before erasure (e.g. when reasoning about sam types)
      // the regular mapOver will cause a class cast exception because TypeBounds don't erase to TypeBounds
      case pt: ProtoType => pt // skip

      case _ =>
        tp.mapOver(this)
    }

    /* scala/bug#10551, scala/bug#10646:
     *
     * There are a few contexts in which it's important to erase types referencing
     * derived value classes to the value class itself, not the underlying. As
     * of right now, those are:
     *   - inside of `classOf`
     *   - the element type of an `ArrayValue`
     * In those cases, the value class needs to be detected and erased using
     * `javaErasure`, which treats refs to value classes the same as any other
     * `TypeRef`. This used to be done by matching on `tr@TypeRef(_,sym,_)`, and
     * checking whether `sym.isDerivedValueClass`, but there are more types with
     * `typeSymbol.isDerivedValueClass` than just `TypeRef`s (`ExistentialType`
     * is one of the easiest to bump into, e.g. `classOf[VC[_]]`).
     *
     * tl;dr if you're trying to erase a value class ref to the value class itself
     * and not going through this method, you're inviting trouble into your life.
     */
    def applyInArray(tp: Type): Type = {
      if (tp.typeSymbol.isDerivedValueClass) javaErasure(tp)
      else apply(tp)
    }
  }

  protected def verifyJavaErasure = false

  /**   The erasure |T| of a type T. This is:
   *
   *   - For a constant type classOf[T], classOf[|T|], unless T is Unit. For any other constant type, itself.
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
   *   - For a non-empty type intersection (possibly with refinement)
   *      - in scala, the erasure of the intersection dominator
   *      - in java, the erasure of its first parent                  <--- @PP: not yet in spec.
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
  def erasure(sym: Symbol): ErasureMap = {
    if (sym == NoSymbol) return scalaErasure
    val enclosing = sym.enclClass
    if (enclosing.isJavaDefined) {
      if (verifyJavaErasure && sym.isMethod) verifiedJavaErasure
      else javaErasure
    }
    else if (enclosing.isScala3Defined) scala3Erasure
    else scalaErasure
  }

  /** This is used as the Scala erasure during the erasure phase itself
   *  It differs from normal erasure in that value classes are erased to ErasedValueTypes which
   *  are then later converted to the underlying parameter type in phase posterasure.
   */
  def specialErasure(sym: Symbol)(tp: Type): Type =
    if (sym != NoSymbol && sym.enclClass.isJavaDefined)
      erasure(sym)(tp)
    else if (sym.isClassConstructor)
      specialConstructorErasure(sym.owner, tp)
    else {
      specialScalaErasureFor(sym)(tp)
    }

  def specialConstructorErasure(clazz: Symbol, tpe: Type): Type = {
    tpe match {
      case PolyType(tparams, restpe) =>
        specialConstructorErasure(clazz, restpe)
      case ExistentialType(tparams, restpe) =>
        specialConstructorErasure(clazz, restpe)
      case mt @ MethodType(params, restpe) =>
        MethodType(
          cloneSymbolsAndModify(params, specialScalaErasureFor(clazz)),
          specialConstructorErasure(clazz, restpe))
      case TypeRef(pre, `clazz`, args) =>
        typeRef(pre, clazz, List())
      case tp =>
        assert(clazz == ArrayClass || tp.isError, s"unexpected constructor erasure $tp for $clazz")
        specialScalaErasureFor(clazz)(tp)
    }
  }

  /** Scala's more precise erasure than java's is problematic as follows:
   *
   *  - Symbols are read from classfiles and populated with types
   *  - The textual signature read from the bytecode is forgotten
   *  - Bytecode generation must know the precise signature of a method
   *  - the signature is derived from the erasure of the method type
   *  - If that derivation does not adhere to the rules by which the original
   *    signature was created, a NoSuchMethod error will result.
   *
   *  For this reason and others (such as distinguishing constructors from other methods)
   *  erasure is now (Symbol, Type) => Type rather than Type => Type.
   */
  abstract class ScalaErasureMap extends ErasureMap with Scala2JavaArrayErasure {

    /** In scala, calculate a useful parent.
     *  An intersection such as `Object with Trait` erases to Trait.
     */
    def mergeParents(parents: List[Type]): Type =
      intersectionDominator(parents)
  }

  trait Scala2JavaArrayErasure { self: ErasureMap =>

    def eraseArray(arrayRef: Type, pre: Type, args: List[Type]): Type =
      if (unboundedGenericArrayLevel(arrayRef) == 1) ObjectTpe
      else if (args.head.typeSymbol.isBottomClass) arrayType(ObjectTpe)
      else typeRef(self(pre), ArrayClass, args map applyInArray)

  }

  class Scala3ErasureMap extends ErasureMap { self =>

    def mergeParents(parents: List[Type]): Type = {
      erasedGlb(parents.map(self(_)))
    }

    def mergeParentsInArray(parents: List[Type]): Type = {
      erasedGlb(parents.map(super.applyInArray(_)))
    }

    override def applyInArray(tp: Type): Type = {
      tp match {
        case RefinedType(parents, _) =>
          super.applyInArray(mergeParentsInArray(parents))
        case _ =>
          super.applyInArray(tp)
      }
    }

    def eraseArray(arrayRef: Type, pre: Type, args: List[Type]): Type = {
      if (isGenericArrayElement(args.head)) ObjectTpe
      else typeRef(self(pre), ArrayClass, args map applyInArray)
    }

    /** Scala 3 implementation of erasure for intersection types.
     *  @param components the erased component types of the intersection.
     */
    private def erasedGlb(components: List[Type]): Type = {

      /** A comparison function that induces a total order on erased types,
       *  where `A <= B` implies that the erasure of `A & B` should be A.
       *
       *  This order respects the following properties:
       *  - ErasedValueTypes <= non-ErasedValueTypes
       *  - arrays <= non-arrays
       *  - primitives <= non-primitives
       *  - real classes <= traits
       *  - subtypes <= supertypes
       *
       *  Since this isn't enough to order to unrelated classes, we use
       *  lexicographic ordering of the class symbol full name as a tie-breaker.
       *  This ensure that `A <= B && B <= A` iff `A =:= B`.
       */
      def compareErasedGlb(tp1: Type, tp2: Type): Int = {
        // this check is purely an optimization.
        if (tp1 eq tp2) return 0

        val isEVT1 = tp1.isInstanceOf[ErasedValueType]
        val isEVT2 = tp2.isInstanceOf[ErasedValueType]
        if (isEVT1 && isEVT2) {
          return compareErasedGlb(
            tp1.asInstanceOf[ErasedValueType].valueClazz.tpe_*,
            tp2.asInstanceOf[ErasedValueType].valueClazz.tpe_*)
        }
        else if (isEVT1)
          return -1
        else if (isEVT2)
          return 1

        val sym1 = tp1.baseClasses.head
        val sym2 = tp2.baseClasses.head

        def compareClasses: Int = {
          if (sym1.isSubClass(sym2))
            -1
          else if (sym2.isSubClass(sym1))
            1
          else
            sym1.fullName.compareTo(sym2.fullName)
        }

        val isArray1 = tp1.typeArgs.nonEmpty && sym1.isSubClass(definitions.ArrayClass)
        val isArray2 = tp2.typeArgs.nonEmpty && sym2.isSubClass(definitions.ArrayClass)
        if (isArray1 && isArray2)
          return compareErasedGlb(tp1.typeArgs.head, tp2.typeArgs.head)
        else if (isArray1)
          return -1
        else if (isArray2)
          return 1

        val isPrimitive1 = sym1.isPrimitiveValueClass
        val isPrimitive2 = sym2.isPrimitiveValueClass
        if (isPrimitive1 && isPrimitive2)
          return compareClasses
        else if (isPrimitive1)
          return -1
        else if (isPrimitive2)
          return 1

        val isRealClass1 = sym1.isClass && !sym1.isTrait
        val isRealClass2 = sym2.isClass && !sym2.isTrait
        if (isRealClass1 && isRealClass2)
          return compareClasses
        else if (isRealClass1)
          return -1
        else if (isRealClass2)
          return 1

        compareClasses
      }

      components.min((t, u) => compareErasedGlb(t, u))
    }

    /** Dotty implementation of Array Erasure:
     *
     *  Is `Array[tp]` a generic Array that needs to be erased to `Object`?
     *  This is true if among the subtypes of `Array[tp]` there is either:
     *  - both a reference array type and a primitive array type
     *    (e.g. `Array[_ <: Int | String]`, `Array[_ <: Any]`)
     *  - or two different primitive array types (e.g. `Array[_ <: Int | Double]`)
     *  In both cases the erased lub of those array types on the JVM is `Object`.
     */
    private def isGenericArrayElement(tp: Type): Boolean = {

      object DottyTypeProxy {

        def unapply(tp: Type): Option[Type] = {
          val superTpe = translucentSuperType(tp)
          if (superTpe ne NoType) Some(superTpe) else None
        }

        def translucentSuperType(tp: Type): Type = tp match {
          case tp: TypeRef => transparentDealias(tp.sym, tp.pre, tp.sym.owner)
          case tp: SingleType => tp.underlying
          case tp: ThisType => tp.sym.typeOfThis
          case tp: ConstantType => tp.value.tpe
          case tp: RefinedType if tp.decls.nonEmpty => intersectionType(tp.parents)
          case tp: PolyType => tp.resultType
          case tp: ExistentialType => tp.underlying
          case tp: TypeBounds => tp.hi
          case tp: AnnotatedType => tp.underlying
          case tp: SuperType => tp.thistpe.baseType(tp.supertpe.typeSymbol)
          case tp => NoType
        }

      }

      object DottyAndType {
        def unapply(tp: RefinedType): Boolean = tp.decls.isEmpty
      }

      /** A symbol that represents the sort of JVM array that values of type `t` can be stored in:
       *  - If we can always store such values in a reference array, return Object
       *  - If we can always store them in a specific primitive array, return the
       *    corresponding primitive class
       *  - Otherwise, return `NoSymbol`.
       */
      def arrayUpperBound(tp: Type): Symbol = tp.dealias match {
        case TypeRef(_, sym, _) if sym.isClass =>
          val cls = sym
          // Only a few classes have both primitives and references as subclasses.
          if ((cls eq AnyClass) || (cls eq AnyValClass) || (cls eq SingletonClass))
            NoSymbol
          // We only need to check for primitives because derived value classes in arrays are always boxed.
          else if (cls.isPrimitiveValueClass)
            cls
          else
            ObjectClass
        case DottyTypeProxy(unwrapped) =>
          arrayUpperBound(unwrapped)
        case tp @ DottyAndType() =>
          // Find first `p` in `parents` where `arrayUpperBound(p) ne NoSymbol`
          def loop(tps: List[Type]): Symbol = tps match {
            case p :: tps =>
              val ub = arrayUpperBound(p)
              if (ub ne NoSymbol) ub
              else loop(tps)
            case nil => NoSymbol
          }
          loop(tp.parents)
        case _ =>
          NoSymbol
      }

      /** Can one of the JVM Array type store all possible values of type `t`? */
      def fitsInJVMArray(tp: Type): Boolean = arrayUpperBound(tp) ne NoSymbol

      def isOpaque(sym: Symbol) = sym.isScala3Defined && !sym.isClass && sym.hasAttachment[DottyOpaqueTypeAlias]

      tp.dealias match {
        case tp @ TypeRef(_, sym, _) if !isOpaque(sym) =>
          !sym.isClass &&
          !sym.isJavaDefined && // In Java code, Array[T] can never erase to Object
          !fitsInJVMArray(tp)
        case DottyTypeProxy(unwrapped) =>
          isGenericArrayElement(unwrapped)
        case tp @ DottyAndType() =>
          tp.parents.forall(isGenericArrayElement)
        case _ =>
          false
      }

    }

  }

  class JavaErasureMap extends ErasureMap with Scala2JavaArrayErasure {
    /** In java, always take the first parent.
     *  An intersection such as `Object with Trait` erases to Object.
     */
    def mergeParents(parents: List[Type]): Type =
      if (parents.isEmpty) ObjectTpe
      else parents.head

    override protected def eraseDerivedValueClassRef(tref: TypeRef): Type = eraseNormalClassRef(tref)
  }

  object scalaErasure extends ScalaErasureMap
  object scala3Erasure extends Scala3ErasureMap

  trait SpecialScalaErasure extends ErasureMap {
    override def eraseDerivedValueClassRef(tref: TypeRef): Type =
      ErasedValueType(tref.sym, erasedValueClassArg(tref))
  }

  /** This is used as the Scala erasure during the erasure phase itself
   *  It differs from normal erasure in that value classes are erased to ErasedValueTypes which
   *  are then later unwrapped to the underlying parameter type in phase posterasure.
   */
  object specialScalaErasure extends ScalaErasureMap with SpecialScalaErasure

  /** This is used as the Scala erasure for Scala 3 methods during the erasure phase itself.
   *  @see specialScalaErasure
   */
  object specialScala3Erasure extends Scala3ErasureMap with SpecialScalaErasure

  def specialScalaErasureFor(sym: Symbol): ErasureMap = {
    if (sym.isScala3Defined) specialScala3Erasure
    else specialScalaErasure
  }

  object javaErasure extends JavaErasureMap

  object verifiedJavaErasure extends JavaErasureMap {
    override def apply(tp: Type): Type = {
      val res = javaErasure(tp)
      val old = scalaErasure(tp)
      if (!(res =:= old))
        log("Identified divergence between java/scala erasure:\n  scala: " + old + "\n   java: " + res)
      res
    }
  }

  trait BoxingErasure extends ErasureMap {

    private[this] var boxPrimitives = true

    override def applyInArray(tp: Type): Type = {
      val saved = boxPrimitives
      boxPrimitives = false
      try super.applyInArray(tp)
      finally boxPrimitives = saved
    }

    override def eraseNormalClassRef(tref: TypeRef) =
      if (boxPrimitives && isPrimitiveValueClass(tref.sym)) boxedClass(tref.sym).tpe
      else super.eraseNormalClassRef(tref)

    override def eraseDerivedValueClassRef(tref: TypeRef) =
      super.eraseNormalClassRef(tref)

  }

  object boxingErasure extends ScalaErasureMap with BoxingErasure
  object boxing3Erasure extends Scala3ErasureMap with BoxingErasure

  /** The intersection dominator (SLS 3.7) of a list of types is computed as follows.
   *
   *  - If the list contains one or more occurrences of scala.Array with
   *    type parameters El1, El2, ... then the dominator is scala.Array with
   *    type parameter of intersectionDominator(List(El1, El2, ...)).           <--- @PP: not yet in spec.
   *  - Otherwise, the list is reduced to a subsequence containing only types
   *    which are not subtypes of other listed types (the span.)
   *  - If the span is empty, the dominator is Object.
   *  - If the span contains a class Tc which is not a trait and which is
   *    not Object, the dominator is Tc.                                        <--- @PP: "which is not Object" not in spec.
   *  - Otherwise, the dominator is the first element of the span.
   */
  def intersectionDominator(parents: List[Type]): Type = {
    if (parents.isEmpty) ObjectTpe
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
  }

  /** For a type alias, get its info as seen from
   *  the current prefix and owner.
   *  Sees through opaque type aliases.
   */
  def transparentDealias(sym: Symbol, pre: Type, owner: Symbol) = {
    @inline def visible(tp: Type) = tp.asSeenFrom(pre, owner)

    if (sym.isScala3Defined && !sym.isClass)
      sym.attachments.get[DottyOpaqueTypeAlias]
        .map(alias => visible(alias.tpe))
        .getOrElse(visible(sym.info))
    else
      visible(sym.info)
  }

  /** The symbol's erased info. This is the type's erasure, except for the following primitive symbols:
    *
    *   - $asInstanceOf    --> [T]T
    *   - $isInstanceOf    --> [T]scala#Boolean
    *   - synchronized     --> [T](x: T)T
    *   - class Array      --> [T]C where C is the erased classinfo of the Array class.
    *   - Array[T].<init>  --> {scala#Int)Array[T]
    *
    * An abstract type's info erases to a TypeBounds type consisting of the erasures of the abstract type's bounds.
    */
  def transformInfo(sym: Symbol, tp: Type): Type = {
    // Do not erase the primitive `synchronized` method's info or the info of its parameter.
    // We do erase the info of its type param so that subtyping can relate its bounds after erasure.
    def synchronizedPrimitive(sym: Symbol) =
      sym == Object_synchronized || (sym.owner == Object_synchronized && sym.isTerm)

    if (sym == Object_asInstanceOf || synchronizedPrimitive(sym))
      sym.info
    else if (sym == Object_isInstanceOf || sym == ArrayClass)
      PolyType(sym.info.typeParams, specialErasure(sym)(sym.info.resultType))
    else if (sym.isAbstractType)
      TypeBounds(WildcardType, WildcardType) // TODO why not use the erasure of the type's bounds, as stated in the doc?
    else if (sym.isTerm && sym.owner == ArrayClass) {
      if (sym.isClassConstructor) // TODO: switch on name for all branches -- this one is sym.name == nme.CONSTRUCTOR
        tp match {
          case MethodType(params, TypeRef(pre, sym1, args)) =>
            MethodType(cloneSymbolsAndModify(params, tp => specialErasure(sym)(tp)),
                       typeRef(specialErasure(sym)(pre), sym1, args))
          case x => throw new MatchError(x)
        }
      else if (sym.name == nme.apply)
        tp
      else if (sym.name == nme.update)
        (tp: @unchecked) match {
          case MethodType(List(index, tvar), restpe) =>
            MethodType(List(index.cloneSymbol.setInfo(specialErasure(sym)(index.tpe)), tvar), UnitTpe)
        }
      else specialErasure(sym)(tp)
    } else if (
      sym.owner != NoSymbol &&
      sym.owner.owner == ArrayClass &&
      sym == Array_update.paramss.head(1)) { // TODO: can we simplify the guard, perhaps cache the symbol to compare to?
      // special case for Array.update: the non-erased type remains, i.e. (Int,A)Unit
      // since the erasure type map gets applied to every symbol, we have to catch the
      // symbol here
      tp
    } else {
      // TODO OPT: altogether, there are 9 symbols that we special-case.
      // Could we get to the common case more quickly by looking them up in a set?
      specialErasure(sym)(tp)
    }
  }
}
