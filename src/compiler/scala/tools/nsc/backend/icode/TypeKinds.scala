/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package icode

/* A type case

    case UNIT            =>
    case BOOL            =>
    case BYTE            =>
    case SHORT           =>
    case CHAR            =>
    case INT             =>
    case LONG            =>
    case FLOAT           =>
    case DOUBLE          =>
    case REFERENCE(cls)  =>
    case ARRAY(elem)     =>

*/

trait TypeKinds { self: ICodes =>
  import global._
  import definitions.{ ArrayClass, AnyRefClass, ObjectClass, NullClass, NothingClass, arrayType }

  /** A map from scala primitive Types to ICode TypeKinds */
  lazy val primitiveTypeMap: Map[Symbol, TypeKind] = {
    import definitions._
    Map(
      UnitClass     -> UNIT,
      BooleanClass  -> BOOL,
      CharClass     -> CHAR,
      ByteClass     -> BYTE,
      ShortClass    -> SHORT,
      IntClass      -> INT,
      LongClass     -> LONG,
      FloatClass    -> FLOAT,
      DoubleClass   -> DOUBLE
    )
  }
  /** Reverse map for toType */
  private lazy val reversePrimitiveMap: Map[TypeKind, Symbol] =
    (primitiveTypeMap map (_.swap)).toMap

  /** This class represents a type kind. Type kinds
   * represent the types that the VM know (or the ICode
   * view of what VMs know).
   */
  sealed abstract class TypeKind {
    def maxType(other: TypeKind): TypeKind

    def toType: Type = reversePrimitiveMap get this map (_.tpe) getOrElse {
      this match {
        case REFERENCE(cls) => cls.tpe
        case ARRAY(elem)    => arrayType(elem.toType)
        case _              => abort("Unknown type kind.")
      }
    }

    def isReferenceType           = false
    def isArrayType               = false
    def isValueType               = false
    def isBoxedType               = false
    final def isRefOrArrayType    = isReferenceType || isArrayType
    final def isRefArrayOrBoxType = isRefOrArrayType || isBoxedType
    final def isNothingType       = this == NothingReference
    final def isNullType          = this == NullReference
    final def isInterfaceType     = this match {
      case REFERENCE(cls) if cls.isInterface || cls.isTrait => true
      case _                                                => false
    }

    /** On the JVM,
     *    BOOL, BYTE, CHAR, SHORT, and INT
     *  are like Ints for the purposes of calculating the lub.
     */
    def isIntSizedType: Boolean = false

    /** On the JVM, similar to isIntSizedType except that BOOL isn't integral while LONG is. */
    def isIntegralType: Boolean = false

    /** On the JVM, FLOAT and DOUBLE. */
    def isRealType: Boolean = false

    final def isNumericType: Boolean = isIntegralType | isRealType

    /** Simple subtyping check */
    def <:<(other: TypeKind): Boolean = (this eq other) || (this match {
      case BOOL | BYTE | SHORT | CHAR => other == INT || other == LONG
      case _                          => this eq other
    })

    /** Is this type a category 2 type in JVM terms? (ie, is it LONG or DOUBLE?) */
    def isWideType: Boolean = false

    /** The number of dimensions for array types. */
    def dimensions: Int = 0

    protected def uncomparable(thisKind: String, other: TypeKind): Nothing =
      abort("Uncomparable type kinds: " + thisKind + " with " + other)

    protected def uncomparable(other: TypeKind): Nothing =
      uncomparable(this.toString, other)
  }

  sealed abstract class ValueTypeKind extends TypeKind {
    override def isValueType = true
    override def toString = {
      this.getClass.getName stripSuffix "$" dropWhile (_ != '$') drop 1
    }
  }

  var lubs0 = 0

  /**
   * The least upper bound of two typekinds. They have to be either
   * REFERENCE or ARRAY kinds.
   *
   * The lub is based on the lub of scala types.
   */
  def lub(a: TypeKind, b: TypeKind): TypeKind = {
    /** The compiler's lub calculation does not order classes before traits.
     *  This is apparently not wrong but it is inconvenient, and causes the
     *  icode checker to choke when things don't match up.  My attempts to
     *  alter the calculation at the compiler level were failures, so in the
     *  interests of a working icode checker I'm making the adjustment here.
     *
     *  Example where we'd like a different answer:
     *
     *    abstract class Tom
     *    case object Bob extends Tom
     *    case object Harry extends Tom
     *    List(Bob, Harry)  // compiler calculates "Product with Tom" rather than "Tom with Product"
     *
     *  Here we make the adjustment by rewinding to a pre-erasure state and
     *  sifting through the parents for a class type.
     */
    def lub0(tk1: TypeKind, tk2: TypeKind): Type = beforeUncurry {
      import definitions._
      val tp = global.lub(List(tk1.toType, tk2.toType))
      val (front, rest) = tp.parents span (_.typeSymbol.isTrait)

      if (front.isEmpty || rest.isEmpty || rest.head.typeSymbol == ObjectClass) tp
      else rest.head
    }

    def isIntLub = (
      (a == INT && b.isIntSizedType) ||
      (b == INT && a.isIntSizedType)
    )

    if (a == b) a
    else if (a.isNothingType) b
    else if (b.isNothingType) a
    else if (a.isBoxedType || b.isBoxedType) AnyRefReference  // we should do better
    else if (isIntLub) INT
    else if (a.isRefOrArrayType && b.isRefOrArrayType) {
      if (a.isNullType) b
      else if (b.isNullType) a
      else toTypeKind(lub0(a, b))
    }
    else throw new CheckerException("Incompatible types: " + a + " with " + b)
  }

  /** The unit value */
  case object UNIT extends ValueTypeKind {
    def maxType(other: TypeKind) = other match {
      case UNIT | REFERENCE(NothingClass)   => UNIT
      case _                                => uncomparable(other)
    }
  }

  /** A boolean value */
  case object BOOL extends ValueTypeKind {
    override def isIntSizedType = true
    def maxType(other: TypeKind) = other match {
      case BOOL | REFERENCE(NothingClass)   => BOOL
      case _                                => uncomparable(other)
    }
  }

  /** Note that the max of Char/Byte and Char/Short is Int, because
   *  neither strictly encloses the other due to unsignedness.
   *  See ticket #2087 for a consequence.
   */

  /** A 1-byte signed integer */
  case object BYTE extends ValueTypeKind {
    override def isIntSizedType = true
    override def isIntegralType = true
    def maxType(other: TypeKind) = {
      if (other == BYTE || other.isNothingType) BYTE
      else if (other == CHAR) INT
      else if (other.isNumericType) other
      else uncomparable(other)
    }
  }

  /** A 2-byte signed integer */
  case object SHORT extends ValueTypeKind {
    override def isIntSizedType = true
    override def isIntegralType = true
    override def maxType(other: TypeKind) = other match {
      case BYTE | SHORT | REFERENCE(NothingClass) => SHORT
      case CHAR                                   => INT
      case INT | LONG | FLOAT | DOUBLE            => other
      case _                                      => uncomparable(other)
    }
  }

  /** A 2-byte UNSIGNED integer */
  case object CHAR extends ValueTypeKind {
    override def isIntSizedType = true
    override def isIntegralType = true
    override def maxType(other: TypeKind) = other match {
      case CHAR | REFERENCE(NothingClass) => CHAR
      case BYTE | SHORT                   => INT
      case INT | LONG | FLOAT | DOUBLE    => other
      case _                              => uncomparable(other)
    }
  }

  /** A 4-byte signed integer */
  case object INT extends ValueTypeKind {
    override def isIntSizedType = true
    override def isIntegralType = true
    override def maxType(other: TypeKind) = other match {
      case BYTE | SHORT | CHAR | INT | REFERENCE(NothingClass)  => INT
      case LONG | FLOAT | DOUBLE                                => other
      case _                                                    => uncomparable(other)
    }
  }

  /** An 8-byte signed integer */
  case object LONG extends ValueTypeKind {
    override def isIntegralType = true
    override def isWideType = true
    override def maxType(other: TypeKind): TypeKind =
      if (other.isIntegralType || other.isNothingType) LONG
      else if (other.isRealType) DOUBLE
      else uncomparable(other)
  }

  /** A 4-byte floating point number */
  case object FLOAT extends ValueTypeKind {
    override def isRealType = true
    override def maxType(other: TypeKind): TypeKind =
      if (other == DOUBLE) DOUBLE
      else if (other.isNumericType || other.isNothingType) FLOAT
      else uncomparable(other)
  }

  /** An 8-byte floating point number */
  case object DOUBLE extends ValueTypeKind {
    override def isRealType = true
    override def isWideType = true
    override def maxType(other: TypeKind): TypeKind =
      if (other.isNumericType || other.isNothingType) DOUBLE
      else uncomparable(other)
  }

  /** A class type. */
  final case class REFERENCE(cls: Symbol) extends TypeKind {
    override def toString = "REF(" + cls + ")"
    assert(cls ne null,
           "REFERENCE to null class symbol.")
    assert(cls != ArrayClass,
           "REFERENCE to Array is not allowed, should be ARRAY[..] instead")
    assert(cls != NoSymbol,
           "REFERENCE to NoSymbol not allowed!")

    /**
     * Approximate `lub`. The common type of two references is
     * always AnyRef. For 'real' least upper bound wrt to subclassing
     * use method 'lub'.
     */
    override def maxType(other: TypeKind) = other match {
      case REFERENCE(_) | ARRAY(_)  => AnyRefReference
      case _                        => uncomparable("REFERENCE", other)
    }

    /** Checks subtyping relationship. */
    override def <:<(other: TypeKind) = isNothingType || (other match {
      case REFERENCE(cls2)  => cls.tpe <:< cls2.tpe
      case ARRAY(_)         => cls == NullClass
      case _                => false
    })
    override def isReferenceType = true
  }

  def ArrayN(elem: TypeKind, dims: Int): ARRAY = {
    assert(dims > 0)
    if (dims == 1) ARRAY(elem)
    else ARRAY(ArrayN(elem, dims - 1))
  }

  final case class ARRAY(val elem: TypeKind) extends TypeKind {
    override def toString    = "ARRAY[" + elem + "]"
    override def isArrayType = true
    override def dimensions  = 1 + elem.dimensions

    /** The ultimate element type of this array. */
    def elementKind: TypeKind = elem match {
      case a @ ARRAY(_) => a.elementKind
      case k            => k
    }

    /**
     * Approximate `lub`. The common type of two references is
     * always AnyRef. For 'real' least upper bound wrt to subclassing
     * use method 'lub'.
     */
    override def maxType(other: TypeKind) = other match {
      case ARRAY(elem2) if elem == elem2  => ARRAY(elem)
      case ARRAY(_) | REFERENCE(_)        => AnyRefReference
      case _                              => uncomparable("ARRAY", other)
    }

    /** Array subtyping is covariant, as in Java. Necessary for checking
     *  code that interacts with Java. */
    override def <:<(other: TypeKind) = other match {
      case ARRAY(elem2)                         => elem <:< elem2
      case REFERENCE(AnyRefClass | ObjectClass) => true // TODO: platform dependent!
      case _                                    => false
    }
  }

  /** A boxed value. */
  case class BOXED(kind: TypeKind) extends TypeKind {
    override def isBoxedType = true

    override def maxType(other: TypeKind) = other match {
      case BOXED(`kind`)                      => this
      case REFERENCE(_) | ARRAY(_) | BOXED(_) => AnyRefReference
      case _                                  => uncomparable("BOXED", other)
    }

    /** Checks subtyping relationship. */
    override def <:<(other: TypeKind) = other match {
      case BOXED(`kind`)                        => true
      case REFERENCE(AnyRefClass | ObjectClass) => true // TODO: platform dependent!
      case _                                    => false
    }
  }

 /**
  * Dummy TypeKind to represent the ConcatClass in a platform-independent
  * way. For JVM it would have been a REFERENCE to 'StringBuffer'.
  */
  case object ConcatClass extends TypeKind {
    override def toString = "ConcatClass"

    /**
     * Approximate `lub`. The common type of two references is
     * always AnyRef. For 'real' least upper bound wrt to subclassing
     * use method 'lub'.
     */
    override def maxType(other: TypeKind) = other match {
      case REFERENCE(_) => AnyRefReference
      case _            => uncomparable(other)
    }

    /** Checks subtyping relationship. */
    override def <:<(other: TypeKind) = this eq other
  }

  ////////////////// Conversions //////////////////////////////

  /** Return the TypeKind of the given type
   *
   *  Call to .normalize fixes #3003 (follow type aliases). Otherwise,
   *  arrayOrClassType below would return ObjectReference.
   */
  def toTypeKind(t: Type): TypeKind = t.normalize match {
    case ThisType(ArrayClass)            => ObjectReference
    case ThisType(sym)                   => REFERENCE(sym)
    case SingleType(_, sym)              => primitiveOrRefType(sym)
    case ConstantType(_)                 => toTypeKind(t.underlying)
    case TypeRef(_, sym, args)           => primitiveOrClassType(sym, args)
    case ClassInfoType(_, _, ArrayClass) => abort("ClassInfoType to ArrayClass!")
    case ClassInfoType(_, _, sym)        => primitiveOrRefType(sym)

    // !!! Iulian says types which make no sense after erasure should not reach here,
    // which includes the ExistentialType, AnnotatedType, RefinedType.  I don't know
    // if the first two cases exist because they do or as a defensive measure, but
    // at the time I added it, RefinedTypes were indeed reaching here.
    case ExistentialType(_, t)           => toTypeKind(t)
    case AnnotatedType(_, t, _)          => toTypeKind(t)
    case RefinedType(parents, _)         => parents map toTypeKind reduceLeft lub
    // For sure WildcardTypes shouldn't reach here either, but when
    // debugging such situations this may come in handy.
    // case WildcardType                    => REFERENCE(ObjectClass)
    case norm => abort(
      "Unknown type: %s, %s [%s, %s] TypeRef? %s".format(
        t, norm, t.getClass, norm.getClass, t.isInstanceOf[TypeRef]
      )
    )
  }

  /** Return the type kind of a class, possibly an array type.
   */
  private def arrayOrClassType(sym: Symbol, targs: List[Type]) = sym match {
    case ArrayClass       => ARRAY(toTypeKind(targs.head))
    case _ if sym.isClass => newReference(sym)
    case _                =>
      assert(sym.isType, sym) // it must be compiling Array[a]
      ObjectReference
  }
  /** Interfaces have to be handled delicately to avoid introducing
   *  spurious errors, but if we treat them all as AnyRef we lose too
   *  much information.
   */
  private def newReference(sym: Symbol): TypeKind = {
    // Can't call .toInterface (at this phase) or we trip an assertion.
    // See PackratParser#grow for a method which fails with an apparent mismatch
    // between "object PackratParsers$class" and "trait PackratParsers"
    if (sym.isImplClass) {
      // pos/spec-List.scala is the sole failure if we don't check for NoSymbol
      val traitSym = sym.owner.info.decl(tpnme.interfaceName(sym.name))
      if (traitSym != NoSymbol)
        return REFERENCE(traitSym)
    }
    REFERENCE(sym)
  }

  private def primitiveOrRefType(sym: Symbol) =
    primitiveTypeMap.getOrElse(sym, newReference(sym))
  private def primitiveOrClassType(sym: Symbol, targs: List[Type]) =
    primitiveTypeMap.getOrElse(sym, arrayOrClassType(sym, targs))

  def msil_mgdptr(tk: TypeKind): TypeKind = (tk: @unchecked) match {
    case REFERENCE(cls)  => REFERENCE(loaders.clrTypes.mdgptrcls4clssym(cls))
    // TODO have ready class-symbols for the by-ref versions of built-in valuetypes
    case _ => abort("cannot obtain a managed pointer for " + tk)
  }

}
