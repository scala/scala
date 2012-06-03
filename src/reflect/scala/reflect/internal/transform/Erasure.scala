package scala.reflect
package internal
package transform

import Flags.PARAMACCESSOR

trait Erasure {

  val global: SymbolTable
  import global._
  import definitions._

  /** An extractor object for generic arrays */
  object GenericArray {

    /** Is `tp` an unbounded generic type (i.e. which could be instantiated
     *  with primitive as well as class types)?.
     */
    private def genericCore(tp: Type): Type = tp.normalize match {
      /* A Java Array<T> is erased to Array[Object] (T can only be a reference type), where as a Scala Array[T] is
       * erased to Object. However, there is only symbol for the Array class. So to make the distinction between
       * a Java and a Scala array, we check if the owner of T comes from a Java class.
       * This however caused issue SI-5654. The additional test for EXSITENTIAL fixes it, see the ticket comments.
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
    def unapply(tp: Type): Option[(Int, Type)] = tp.normalize match {
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

  protected def unboundedGenericArrayLevel(tp: Type): Int = tp match {
    case GenericArray(level, core) if !(core <:< AnyRefClass.tpe) => level
    case _ => 0
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
  protected def rebindInnerClass(pre: Type, cls: Symbol): Type = {
    if (cls.owner.isClass) cls.owner.tpe else pre // why not cls.isNestedClass?
  }

  def underlyingOfValueClass(clazz: Symbol): Type =
    clazz.firstParamAccessor.tpe.resultType

  abstract class ErasureMap extends TypeMap {
    private lazy val ObjectArray  = arrayType(ObjectClass.tpe)
    private lazy val ErasedObject = erasedTypeRef(ObjectClass)

    def mergeParents(parents: List[Type]): Type

    def eraseNormalClassRef(pre: Type, clazz: Symbol): Type =
      typeRef(apply(rebindInnerClass(pre, clazz)), clazz, List()) // #2585

    protected def eraseDerivedValueClassRef(clazz: Symbol): Type =
      scalaErasure(underlyingOfValueClass(clazz))

    def apply(tp: Type): Type = tp match {
      case ConstantType(_) =>
        tp
      case st: SubType =>
        apply(st.supertype)
      case TypeRef(pre, sym, args) =>
        if (sym == ArrayClass)
          if (unboundedGenericArrayLevel(tp) == 1) ObjectClass.tpe
          else if (args.head.typeSymbol.isBottomClass) ObjectArray
          else typeRef(apply(pre), sym, args map applyInArray)
        else if (sym == AnyClass || sym == AnyValClass || sym == SingletonClass || sym == NotNullClass) ErasedObject
        else if (sym == UnitClass) erasedTypeRef(BoxedUnitClass)
        else if (sym.isRefinementClass) apply(mergeParents(tp.parents))
        else if (sym.isDerivedValueClass) eraseDerivedValueClassRef(sym)
        else if (sym.isClass) eraseNormalClassRef(pre, sym)
        else apply(sym.info) // alias type or abstract type
      case PolyType(tparams, restpe) =>
        apply(restpe)
      case ExistentialType(tparams, restpe) =>
        apply(restpe)
      case mt @ MethodType(params, restpe) =>
        MethodType(
          cloneSymbolsAndModify(params, ErasureMap.this),
          if (restpe.typeSymbol == UnitClass) erasedTypeRef(UnitClass)
          // this replaces each typeref that refers to an argument
          // by the type `p.tpe` of the actual argument p (p in params)
          else apply(mt.resultType(params map (_.tpe))))
      case RefinedType(parents, decls) =>
        apply(mergeParents(parents))
      case AnnotatedType(_, atp, _) =>
        apply(atp)
      case ClassInfoType(parents, decls, clazz) =>
        ClassInfoType(
          if (clazz == ObjectClass || isPrimitiveValueClass(clazz)) Nil
          else if (clazz == ArrayClass) List(ErasedObject)
          else removeLaterObjects(parents map this),
          decls, clazz)
      case _ =>
        mapOver(tp)
    }

    def applyInArray(tp: Type): Type = tp match {
      case TypeRef(pre, sym, args) if (sym.isDerivedValueClass) => eraseNormalClassRef(pre, sym)
      case _ => apply(tp)
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
    if (sym == NoSymbol || !sym.enclClass.isJavaDefined) scalaErasure
    else if (verifyJavaErasure && sym.isMethod) verifiedJavaErasure
    else javaErasure

  /** This is used as the Scala erasure during the erasure phase itself
   *  It differs from normal erasure in that value classes are erased to ErasedValueTypes which
   *  are then later converted to the underlying parameter type in phase posterasure.
   */
  def specialErasure(sym: Symbol)(tp: Type): Type =
    if (sym != NoSymbol && sym.enclClass.isJavaDefined)
      erasure(sym)(tp)
    else if (sym.isTerm && sym.owner.isDerivedValueClass)
      specialErasureAvoiding(sym.owner, tp)
    else if (sym.isValue && sym.owner.isMethodWithExtension)
      specialErasureAvoiding(sym.owner.owner, tp)
    else
      specialScalaErasure(tp)

  def specialErasureAvoiding(clazz: Symbol, tpe: Type): Type = {
    tpe match {
      case PolyType(tparams, restpe) =>
        specialErasureAvoiding(clazz, restpe)
      case ExistentialType(tparams, restpe) =>
        specialErasureAvoiding(clazz, restpe)
      case mt @ MethodType(params, restpe) =>
        MethodType(
          cloneSymbolsAndModify(params, specialErasureAvoiding(clazz, _)),
          if (restpe.typeSymbol == UnitClass) erasedTypeRef(UnitClass)
          else specialErasureAvoiding(clazz, (mt.resultType(params map (_.tpe)))))
      case TypeRef(pre, `clazz`, args) =>
        typeRef(pre, clazz, List())
      case _ =>
        specialScalaErasure(tpe)
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
  class ScalaErasureMap extends ErasureMap {
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
    def mergeParents(parents: List[Type]): Type =
      if (parents.isEmpty) ObjectClass.tpe
      else parents.head
  }

  object scalaErasure extends ScalaErasureMap

  /** This is used as the Scala erasure during the erasure phase itself
   *  It differs from normal erasure in that value classes are erased to ErasedValueTypes which
   *  are then later converted to the underlying parameter type in phase posterasure.
   */
  object specialScalaErasure extends ScalaErasureMap {
    override def eraseDerivedValueClassRef(clazz: Symbol): Type = ErasedValueType(clazz)
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
  }

  /** Type reference after erasure */
  def erasedTypeRef(sym: Symbol): Type =
    typeRef(erasure(sym)(sym.owner.tpe), sym, Nil)

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
      PolyType(sym.info.typeParams, specialErasure(sym)(sym.info.resultType))
    else if (sym.isAbstractType)
      TypeBounds(WildcardType, WildcardType)
    else if (sym.isTerm && sym.owner == ArrayClass) {
      if (sym.isClassConstructor)
        tp match {
          case MethodType(params, TypeRef(pre, sym1, args)) =>
            MethodType(cloneSymbolsAndModify(params, specialErasure(sym)),
                       typeRef(specialErasure(sym)(pre), sym1, args))
        }
      else if (sym.name == nme.apply)
        tp
      else if (sym.name == nme.update)
        (tp: @unchecked) match {
          case MethodType(List(index, tvar), restpe) =>
            MethodType(List(index.cloneSymbol.setInfo(specialErasure(sym)(index.tpe)), tvar),
                       erasedTypeRef(UnitClass))
        }
      else specialErasure(sym)(tp)
    } else if (
      sym.owner != NoSymbol &&
      sym.owner.owner == ArrayClass &&
      sym == Array_update.paramss.head(1)) {
      // special case for Array.update: the non-erased type remains, i.e. (Int,A)Unit
      // since the erasure type map gets applied to every symbol, we have to catch the
      // symbol here
      tp
    } else {
      specialErasure(sym)(tp)
    }
  }
}
