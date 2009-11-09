/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

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

import scala.collection.mutable.{ Map, HashMap }

trait TypeKinds { self: ICodes =>
  import global._
  import definitions.{ ArrayClass, AnyRefClass, ObjectClass, NullClass, NothingClass }

  /** A map from scala primitive Types to ICode TypeKinds */
  lazy val primitiveTypeMap: collection.Map[Symbol, TypeKind] = {
    import definitions._
    collection.Map(
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
  private lazy val reversePrimitiveMap: collection.Map[TypeKind, Symbol] =
    collection.Map(primitiveTypeMap.toList map (_.swap) : _*)

  /** This class represents a type kind. Type kinds
   * represent the types that the VM know (or the ICode
   * view of what VMs know).
   */
  sealed abstract class TypeKind {

    def toType: Type = (reversePrimitiveMap get this) match {
      case Some(sym)  => sym.tpe
      case None       => this match {
        case REFERENCE(cls)  => cls.tpe // typeRef(cls.typeConstructor.prefix, cls, Nil)
        // case VALUE(cls)      => typeRef(cls.typeConstructor.prefix, cls, Nil)
        case ARRAY(elem)     => typeRef(ArrayClass.typeConstructor.prefix, ArrayClass, List(elem.toType))
        case _ => abort("Unknown type kind.")
      }
    }

    def isReferenceType: Boolean = false
    def isArrayType: Boolean = false
    def isValueType: Boolean = !isReferenceType && !isArrayType


    def isIntType: Boolean = this match {
      case BYTE | SHORT | INT | LONG | CHAR => true
      case _ => false
    }

    def isRealType: Boolean = this match {
      case FLOAT | DOUBLE => true
      case _ => false
    }

    def isNumericType: Boolean = isIntType | isRealType

    def maxType(other: TypeKind): TypeKind

    /** Simple subtyping check */
    def <:<(other: TypeKind): Boolean = (this eq other) || (this match {
      case BOOL | BYTE | SHORT | CHAR =>
        other match {
          case INT | LONG => true
          case _ => false
        }
      case _ => this eq other
    })

    override def equals(other: Any): Boolean =
      this eq other.asInstanceOf[AnyRef]

    /** Is this type a category 2 type in JVM terms? */
    def isWideType: Boolean = this match {
      case DOUBLE | LONG => true
      case _ => false
    }

    /** The number of dimensions for array types. */
    def dimensions: Int = 0
  }

  /**
   * The least upper bound of two typekinds. They have to be either
   * REFERENCE or ARRAY kinds.
   *
   * The lub is based on the lub of scala types.
   */
  def lub(a: TypeKind, b: TypeKind): TypeKind = {
    def lub0(t1: Type, t2: Type): Type = {
      val lubTpe = global.lub(t1 :: t2 :: Nil)
      assert(lubTpe.typeSymbol.isClass,
             "Least upper bound of " + t1 + " and " + t2 + " is not a class: " + lubTpe)
      lubTpe
    }

    if ((a.isReferenceType || a.isArrayType) &&
        (b.isReferenceType || b.isArrayType))
      toTypeKind(lub0(a.toType, b.toType))
    else if (a == b) a
    else if (a == REFERENCE(NothingClass)) b
    else if (b == REFERENCE(NothingClass)) a
    else (a, b) match {
      case (BOXED(a1), BOXED(b1)) => if (a1 == b1) a else REFERENCE(AnyRefClass)
      case (BOXED(_), REFERENCE(_)) | (REFERENCE(_), BOXED(_)) => REFERENCE(AnyRefClass)
      case (BOXED(_), ARRAY(_)) | (ARRAY(_), BOXED(_)) => REFERENCE(AnyRefClass)
      case (BYTE, INT) | (INT, BYTE) => INT
      case (SHORT, INT) | (INT, SHORT) => INT
      case (CHAR, INT) | (INT, CHAR) => INT
      case (BOOL, INT) | (INT, BOOL) => INT
      case _ => throw new CheckerError("Incompatible types: " + a + " with " + b)
    }
  }

  /** The unit value */
  case object UNIT extends TypeKind {
    def maxType(other: TypeKind): TypeKind = other match {
      case UNIT => UNIT
      case REFERENCE(NothingClass)  => UNIT
      case _ => abort("Uncomparable type kinds: UNIT with " + other)
    }
  }

  /** A boolean value */
  case object BOOL extends TypeKind {
    override def maxType(other: TypeKind): TypeKind = other match {
      case BOOL => BOOL
      case REFERENCE(NothingClass) => BOOL
      case _ => abort("Uncomparable type kinds: BOOL with " + other)
    }
  }

  /** A 1-byte signed integer */
  case object BYTE extends TypeKind {
    override def maxType(other: TypeKind): TypeKind =
      other match {
        case BYTE | SHORT | CHAR | INT | LONG | FLOAT | DOUBLE => other
        case REFERENCE(NothingClass) => BYTE
        case _ => abort("Uncomparable type kinds: BYTE with " + other)
      }
  }

  /** A 2-byte signed integer */
  case object SHORT extends TypeKind {
    override def maxType(other: TypeKind): TypeKind =
      other match {
        case BYTE | SHORT | CHAR => SHORT
        case REFERENCE(NothingClass) => SHORT
        case INT | LONG | FLOAT | DOUBLE => other
        case _ => abort("Uncomparable type kinds: SHORT with " + other)
      }
  }

  /** A 2-byte signed integer */
  case object CHAR extends TypeKind {
    override def maxType(other: TypeKind): TypeKind =
      other match {
        case BYTE | SHORT | CHAR => CHAR
        case REFERENCE(NothingClass) => CHAR
        case INT | LONG | FLOAT | DOUBLE => other
        case _ => abort("Uncomparable type kinds: CHAR with " + other)
      }
  }


  /** A 4-byte signed integer */
  case object INT extends TypeKind {
    override def maxType(other: TypeKind): TypeKind =
      other match {
        case BYTE | SHORT | CHAR | INT => INT
        case REFERENCE(NothingClass) => INT
        case LONG | FLOAT | DOUBLE => other
        case _ => abort("Uncomparable type kinds: INT with " + other)
      }
  }

  /** An 8-byte signed integer */
  case object LONG extends TypeKind {
    override def maxType(other: TypeKind): TypeKind =
      other match {
        case BYTE | SHORT | CHAR | INT | LONG => LONG
        case REFERENCE(NothingClass) => LONG
        case FLOAT | DOUBLE => DOUBLE
        case _ => abort("Uncomparable type kinds: LONG with " + other)
      }
  }

  /** A 4-byte floating point number */
  case object FLOAT extends TypeKind {
    override def maxType(other: TypeKind): TypeKind = other match {
      case BYTE | SHORT | CHAR | INT | LONG | FLOAT => FLOAT
      case REFERENCE(NothingClass)                  => FLOAT
      case DOUBLE                                   => DOUBLE
      case _ => abort("Uncomparable type kinds: FLOAT with " + other)
    }
  }

  /** An 8-byte floating point number */
  case object DOUBLE extends TypeKind {
    override def maxType(other: TypeKind): TypeKind =
      if (other.isNumericType)
        DOUBLE
      else if (other == REFERENCE(NothingClass)) DOUBLE
      else abort("Uncomparable type kinds: DOUBLE with " + other)
  }

  /** A string reference */
  // case object STRING extends TypeKind {
  //   override def maxType(other: TypeKind): TypeKind = other match {
  //     case STRING => STRING;
  //     case _   =>
  //       abort("Uncomparbale type kinds: STRING with " + other);
  //   }
  // }

  /** A class type. */
  final case class REFERENCE(cls: Symbol) extends TypeKind {
    assert(cls ne null,
           "REFERENCE to null class symbol.")
    assert(cls != ArrayClass,
           "REFERENCE to Array is not allowed, should be ARRAY[..] instead")
    assert(cls != NoSymbol,
           "REFERENCE to NoSymbol not allowed!")

    override def toString(): String =
      "REFERENCE(" + cls.fullNameString + ")"

    /**
     * Approximate `lub'. The common type of two references is
     * always AnyRef. For 'real' least upper bound wrt to subclassing
     * use method 'lub'.
     */
    override def maxType(other: TypeKind): TypeKind =
      other match {
        case REFERENCE(_) | ARRAY(_) =>
          REFERENCE(AnyRefClass)
        case _ =>
          abort("Uncomparbale type kinds: REFERENCE with " + other)
      }

    /** Checks subtyping relationship. */
    override def <:<(other: TypeKind): Boolean =
      if (cls == NothingClass)
        true
      else other match {
        case REFERENCE(cls2) =>
          cls.tpe <:< cls2.tpe
        case ARRAY(_) =>
          cls == NullClass
        case _ => false
      }

    override def isReferenceType: Boolean = true;

    override def equals(other: Any): Boolean = other match {
      case REFERENCE(cls2) => cls == cls2
      case _               => false
    }
  }

//   final case class VALUE(cls: Symbol) extends TypeKind {
//     override def equals(other: Any): Boolean = other match {
//       case VALUE(cls2) => cls == cls2;
//       case _ => false;
//     }

//     def maxType(other: TypeKind): TypeKind =
//       abort(toString() + " maxType " + other.toString());

//     override def toString(): String =
//       "VALUE(" + cls.fullNameString + ")";
//   }

  def ArrayN(elem: TypeKind, dims: Int): ARRAY = {
    assert(dims > 0)
    if (dims == 1)
      ARRAY(elem)
    else
      ARRAY(ArrayN(elem, dims - 1))
  }

  final case class ARRAY(val elem: TypeKind) extends TypeKind {
    override def toString(): String =
      "ARRAY[" + elem + "]"

    override def isArrayType = true

    override def dimensions: Int = 1 + elem.dimensions

    /** The ultimate element type of this array. */
    def elementKind: TypeKind = elem match {
      case a @ ARRAY(e1) => a.elementKind
      case k => k
    }

    /**
     * Approximate `lub'. The common type of two references is
     * always AnyRef. For 'real' least upper bound wrt to subclassing
     * use method 'lub'.
     */
    override def maxType(other: TypeKind): TypeKind =
      other match {
        case REFERENCE(_) =>
          REFERENCE(AnyRefClass)
        case ARRAY(elem2) =>
          if (elem == elem2) ARRAY(elem)
          else REFERENCE(AnyRefClass)
        case _ =>
          abort("Uncomparbale type kinds: ARRAY with " + other)
      }

    /** Array subtyping is covariant, as in Java. Necessary for checking
     *  code that interacts with Java. */
    override def <:<(other: TypeKind): Boolean =
      other match {
        case ARRAY(elem2) =>
          elem <:< elem2
        case REFERENCE(AnyRefClass | ObjectClass) =>
          true  //  TODO: platform dependent!
        case _ => false
      }

    override def equals(other: Any): Boolean = other match {
      case ARRAY(elem2) => elem == elem2
      case _               => false
    }

  }

  /** A boxed value. */
  case class BOXED(kind: TypeKind) extends TypeKind {
    override def toString(): String =
      "BOXED(" + kind + ")"

    /**
     * Approximate `lub'. The common type of two references is
     * always AnyRef. For 'real' least upper bound wrt to subclassing
     * use method 'lub'.
     */
    override def maxType(other: TypeKind): TypeKind =
      other match {
        case REFERENCE(_) | ARRAY(_) | BOXED(_) =>
          REFERENCE(AnyRefClass)
        case _ =>
          abort("Uncomparbale type kinds: ARRAY with " + other)
      }

    /** Checks subtyping relationship. */
    override def <:<(other: TypeKind): Boolean =
      other match {
        case REFERENCE(AnyRefClass | ObjectClass) =>
          true // TODO: platform dependent!

        case BOXED(other) =>
          kind == other

        case _ => false
      }

    override def equals(other: Any): Boolean = other match {
      case BOXED(kind2) => kind == kind2
      case _            => false
    }

  }

 /**
  * Dummy TypeKind to represent the ConcatClass in a platform-independent
  * way. For JVM it would have been a REFERENCE to 'StringBuffer'.
  */
  case object ConcatClass extends TypeKind {
    override def toString() = "ConcatClass"

    /**
     * Approximate `lub'. The common type of two references is
     * always AnyRef. For 'real' least upper bound wrt to subclassing
     * use method 'lub'.
     */
    override def maxType(other: TypeKind): TypeKind =
      other match {
        case REFERENCE(_) =>
          REFERENCE(AnyRefClass)
        case _ =>
          abort("Uncomparbale type kinds: ConcatClass with " + other)
      }

    /** Checks subtyping relationship. */
    override def <:<(other: TypeKind): Boolean = (this eq other)

    override def isReferenceType: Boolean = false
  }

  ////////////////// Conversions //////////////////////////////


  /** Return the TypeKind of the given type */
  def toTypeKind(t: Type): TypeKind = t match {
    case ThisType(sym) =>
      if (sym == ArrayClass)
        AnyRefReference
      else
        REFERENCE(sym)

    case SingleType(pre, sym) =>
      (primitiveTypeMap get sym) getOrElse REFERENCE(sym)

    case ConstantType(value) =>
      toTypeKind(t.underlying)

    case TypeRef(_, sym, args) =>
      (primitiveTypeMap get sym) getOrElse arrayOrClassType(sym, args)

    case ClassInfoType(_, _, sym) =>
      primitiveTypeMap get sym match {
        case Some(k) => k
        case None    =>
          if (sym == ArrayClass)
            abort("ClassInfoType to ArrayClass!")
          else
            REFERENCE(sym)
      }

    case ExistentialType(tparams, t) =>
      toTypeKind(t)

    case AnnotatedType(_, t, _) =>
      toTypeKind(t)

    //case WildcardType => // bq: useful hack when wildcard types come here
    //  REFERENCE(definitions.ObjectClass)

    case _ =>
      abort("Unknown type: " + t + ", " + t.normalize + "[" + t.getClass + ", " + t.normalize.getClass + "]" +
	    " TypeRef? " + t.isInstanceOf[TypeRef] + ", " + t.normalize.isInstanceOf[TypeRef])
  }

  /** Return the type kind of a class, possibly an array type.
   */
  private def arrayOrClassType(sym: Symbol, targs: List[Type]): TypeKind = {
    if (sym == ArrayClass)
      ARRAY(toTypeKind(targs.head))
    else if (sym.isClass)
        REFERENCE(sym)
    else {
      assert(sym.isType, sym) // it must be compiling Array[a]
      AnyRefReference
    }
  }

}
