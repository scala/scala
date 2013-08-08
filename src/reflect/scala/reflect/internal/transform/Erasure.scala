package scala
package reflect
package internal
package transform

import Flags.{PARAMACCESSOR, METHOD}

trait Erasure extends Arrays {
  val global: SymbolTable
  import global._
  import definitions._

  // @M #2585 when generating a java generic signature that includes
  // a selection of an inner class p.I, (p = `pre`, I = `cls`) must
  // rewrite to p'.I, where p' refers to the class that directly defines
  // the nested class I.
  //
  // See also #2585 marker in javaSig: there, type arguments must be
  // included (use pre.baseType(cls.owner)).
  //
  // This requires that cls.isClass.
  protected def rebindInnerClass(pre: Type, cls: Symbol): Type = {
    if (cls.owner.isClass) cls.owner.tpe_* else pre // why not cls.isNestedClass?
  }

  def unboxDerivedValueClassMethod(clazz: Symbol): Symbol =
    (clazz.info.decl(nme.unbox)) orElse
    (clazz.info.decls.find(_ hasAllFlags PARAMACCESSOR | METHOD) getOrElse
     NoSymbol)

  def underlyingOfValueClass(clazz: Symbol): Type =
    clazz.derivedValueClassUnbox.tpe.resultType

  /** The type of the argument of a value class reference after erasure
   *  This method needs to be called at a phase no later than erasurephase
   */
  def erasedValueClassArg(tref: TypeRef): Type = {
    assert(!phase.erasedTypes)
    val clazz = tref.sym
    if (valueClassIsParametric(clazz)) {
      val underlying = tref.memberType(clazz.derivedValueClassUnbox).resultType
      boxingErasure(underlying)
    } else {
      scalaErasure(underlyingOfValueClass(clazz))
    }
  }

  /** Does this vakue class have an underlying type that's a type parameter of
   *  the class itself?
   *  This method needs to be called at a phase no later than erasurephase
   */
  def valueClassIsParametric(clazz: Symbol): Boolean = {
    assert(!phase.erasedTypes)
    clazz.typeParams contains
      clazz.derivedValueClassUnbox.tpe.resultType.typeSymbol
  }

  abstract class ErasureMap extends TypeMap with ErasureContext {
    def mergeParents(parents: List[Type]): Type
    def erase(tp: Type): Type = apply(tp)

    def eraseNormalClassRef(pre: Type, clazz: Symbol): Type =
      typeRef(apply(rebindInnerClass(pre, clazz)), clazz, Nil) // #2585

    protected def eraseDerivedValueClassRef(tref: TypeRef): Type = erasedValueClassArg(tref)

    def apply(tp: Type): Type = tp match {
      case ConstantType(_) =>
        tp
      case st: SubType =>
        apply(st.supertype)
      case TypeRef(_, ArrayClass, elem :: Nil) =>
        bestErasureForArrayElement(elem)
      case tref @ TypeRef(pre, sym, args) =>
        if (sym == AnyClass || sym == AnyValClass || sym == SingletonClass) ObjectTpe
        else if (sym == UnitClass) erasedTypeRef(BoxedUnitClass)
        else if (sym.isRefinementClass) apply(mergeParents(tp.parents))
        else if (sym.isDerivedValueClass) eraseDerivedValueClassRef(tref)
        else if (sym.isClass) eraseNormalClassRef(pre, sym)
        else apply(pre memberInfo sym) // alias type or abstract type
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
          else apply(mt.resultType(mt.paramTypes)))
      case RefinedType(parents, decls) =>
        apply(mergeParents(parents))
      case AnnotatedType(_, atp, _) =>
        apply(atp)
      case ClassInfoType(parents, decls, clazz) =>
        def parents1 = (
          if (clazz == ObjectClass || isPrimitiveValueClass(clazz)) Nil
          else if (clazz == ArrayClass) List(ObjectTpe)
          else removeLaterObjects(parents map this)
        )
        ClassInfoType(parents1, decls, clazz)
      case _ => mapOver(tp)
    }
  }

  protected def verifyJavaErasure = false

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
  def erasure(sym: Symbol): ErasureMap =
    if (isScalaDefined(sym)) scalaErasure
    else if (verifyJavaErasure && sym.isMethod) verifiedJavaErasure
    else javaErasure

  /** This is used as the Scala erasure during the erasure phase itself
   *  It differs from normal erasure in that value classes are erased to ErasedValueTypes which
   *  are then later converted to the underlying parameter type in phase posterasure.
   */
  def specialErasure(sym: Symbol)(tp: Type): Type =
    if (sym != NoSymbol && sym.enclClass.isJavaDefined)
      erasure(sym)(tp)
    else if (sym.isClassConstructor)
      specialScalaErasure.eraseClassConstructor(tp)
    else
      specialScalaErasure(tp)

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
   *  erasure is Symbol => Type => Type ; the symbol is used to discriminate between
   *  various TypeMaps, and the chosen TypeMap is then applied to the symbol's info.
   */
  class ScalaErasureMap extends ErasureMap {
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
      else if (parents exists isArrayType) {
        val elems = parents map arrayElementType
        if (elems contains NoType) ObjectTpe
        else bestErasureForArrayElements(elems)
      }
      else {
        val psyms = parents map (_.typeSymbol)
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

    def eraseClassConstructor(tp: Type): Type = tp match {
      case PolyType(_, restpe)        => eraseClassConstructor(restpe)
      case ExistentialType(_, restpe) => eraseClassConstructor(restpe)
      case MethodType(params, restpe) => MethodType(cloneSymbolsAndModify(params, erase), eraseClassConstructor(restpe))
      case TypeRef(pre, clazz, _)     => typeRef(erase(pre), clazz, Nil)
      case _                          => erase(tp)
    }

    def erasePolyTypeResult(sym: Symbol): Type = sym.info match {
      case PolyType(tparams, restpe) => PolyType(tparams, erase(restpe))
      case tp                        => erase(tp)
    }

    /** In scala, calculate a useful parent.
     *  An intersection such as `Object with Trait` erases to Trait.
     */
    def mergeParents(parents: List[Type]): Type =
      intersectionDominator(parents)
  }

  class JavaErasureMap extends ErasureMap {
    /** In java, always take the first parent.
     *  An intersection such as `Object with Trait` erases to Object.
     */
    def mergeParents(parents: List[Type]): Type = parents match {
      case Nil    => ObjectTpe
      case p :: _ => p
    }
  }

  object scalaErasure extends ScalaErasureMap

  /** This is used as the Scala erasure during the erasure phase itself
   *  It differs from normal erasure in that value classes are erased to ErasedValueTypes which
   *  are then later converted to the underlying parameter type in phase posterasure.
   */
  object specialScalaErasure extends ScalaErasureMap {
    override def eraseDerivedValueClassRef(tref: TypeRef): Type = ErasedValueType(tref)
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

  object boxingErasure extends ScalaErasureMap {
    override def eraseNormalClassRef(pre: Type, clazz: Symbol) =
      if (isPrimitiveValueClass(clazz)) boxedClass(clazz).tpe
      else super.eraseNormalClassRef(pre, clazz)
    override def eraseDerivedValueClassRef(tref: TypeRef) =
      super.eraseNormalClassRef(tref.pre, tref.sym)
  }

  def mapPolyResult(poly: Type)(f: Type => Type) = PolyType(poly.typeParams, f(poly.resultType))

  private def erasedArrayUpdate(tp: Type): Type = {
    val MethodType(index :: tvar :: Nil, _) = tp
    val clonedIndex = index.cloneSymbol modifyInfo specialScalaErasure
    MethodType(clonedIndex :: tvar :: Nil, UnitTpe)
  }
  private def erasedArrayConstructor(tp: Type): Type = {
    val MethodType(params, TypeRef(pre, sym, args)) = tp
    MethodType(cloneSymbolsAndModify(params, specialScalaErasure), typeRef(specialScalaErasure(pre), sym, args))
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
    def inArray   = sym.enclClass == ArrayClass
    def fullErase = specialErasure(sym)(tp)
    def polyErase = specialScalaErasure erasePolyTypeResult sym

    /* Special case for Array.update: the non-erased type remains, i.e. (Int,A)Unit
     * since the erasure type map gets applied to every symbol, we have to catch the symbol here.
     */
    sym match {
      case Object_asInstanceOf                    => sym.info
      case Object_isInstanceOf | ArrayClass       => polyErase
      case Array_apply | Array_update_lastParam   => tp
      case Array_update                           => erasedArrayUpdate(tp)
      case _ if inArray && sym.isClassConstructor => erasedArrayConstructor(tp)
      case _ if sym.isAbstractType                => TypeBounds(WildcardType, WildcardType)
      case _                                      => fullErase
    }
  }
}
