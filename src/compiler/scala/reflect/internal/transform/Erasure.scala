package scala.reflect
package internal
package transform

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
      case TypeRef(_, sym, _) if sym.isAbstractType && !sym.owner.isJavaDefined =>
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

  abstract class ErasureMap extends TypeMap {
    def mergeParents(parents: List[Type]): Type

    def apply(tp: Type): Type = {
      tp match {
        case ConstantType(_) =>
          tp
        case st: SubType =>
          apply(st.supertype)
        case TypeRef(pre, sym, args) =>
          if (sym == ArrayClass)
            if (unboundedGenericArrayLevel(tp) == 1) ObjectClass.tpe
            else if (args.head.typeSymbol.isBottomClass) ObjectArray
            else typeRef(apply(pre), sym, args map this)
          else if (sym == AnyClass || sym == AnyValClass || sym == SingletonClass || sym == NotNullClass) erasedTypeRef(ObjectClass)
          else if (sym == UnitClass) erasedTypeRef(BoxedUnitClass)
          else if (sym.isRefinementClass) apply(mergeParents(tp.parents))
          else if (sym.isClass) typeRef(apply(rebindInnerClass(pre, sym)), sym, List())  // #2585
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
            if (clazz == ObjectClass || isValueClass(clazz)) Nil
            else if (clazz == ArrayClass) List(erasedTypeRef(ObjectClass))
            else removeDoubleObject(parents map this),
            decls, clazz)
        case _ =>
          mapOver(tp)
      }
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
  def erasure(sym: Symbol, tp: Type): Type = {
    if (sym != NoSymbol && sym.enclClass.isJavaDefined) {
      val res = javaErasure(tp)
      if (verifyJavaErasure && sym.isMethod) {
        val old = scalaErasure(tp)
        if (!(res =:= old))
          log("Identified divergence between java/scala erasure:\n  scala: " + old + "\n   java: " + res)
      }
      res
    }
    else scalaErasure(tp)
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
  object scalaErasure extends ErasureMap {
    /** In scala, calculate a useful parent.
     *  An intersection such as `Object with Trait` erases to Trait.
     */
    def mergeParents(parents: List[Type]): Type =
      intersectionDominator(parents)
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

  object javaErasure extends ErasureMap {
    /** In java, always take the first parent.
     *  An intersection such as `Object with Trait` erases to Object.
     */
    def mergeParents(parents: List[Type]): Type =
      if (parents.isEmpty) ObjectClass.tpe
      else parents.head
  }

  /** Type reference after erasure */
  def erasedTypeRef(sym: Symbol): Type =
    typeRef(erasure(sym, sym.owner.tpe), sym, List())

  /** Remove duplicate references to class Object in a list of parent classes */
  private def removeDoubleObject(tps: List[Type]): List[Type] = tps match {
    case List() => List()
    case tp :: tps1 =>
      if (tp.typeSymbol == ObjectClass) tp :: tps1.filter(_.typeSymbol != ObjectClass)
      else tp :: removeDoubleObject(tps1)
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
    if (sym == Object_asInstanceOf)
      sym.info
    else if (sym == Object_isInstanceOf || sym == ArrayClass)
      PolyType(sym.info.typeParams, erasure(sym, sym.info.resultType))
    else if (sym.isAbstractType)
      TypeBounds(WildcardType, WildcardType)
    else if (sym.isTerm && sym.owner == ArrayClass) {
      if (sym.isClassConstructor)
        tp match {
          case MethodType(params, TypeRef(pre, sym1, args)) =>
            MethodType(cloneSymbolsAndModify(params, erasure(sym, _)),
                       typeRef(erasure(sym, pre), sym1, args))
        }
      else if (sym.name == nme.apply)
        tp
      else if (sym.name == nme.update)
        (tp: @unchecked) match {
          case MethodType(List(index, tvar), restpe) =>
            MethodType(List(index.cloneSymbol.setInfo(erasure(sym, index.tpe)), tvar),
                       erasedTypeRef(UnitClass))
        }
      else erasure(sym, tp)
    } else if (
      sym.owner != NoSymbol &&
      sym.owner.owner == ArrayClass &&
      sym == Array_update.paramss.head(1)) {
      // special case for Array.update: the non-erased type remains, i.e. (Int,A)Unit
      // since the erasure type map gets applied to every symbol, we have to catch the
      // symbol here
      tp
    } else {
      erasure(sym, tp)
    }
  }
}
