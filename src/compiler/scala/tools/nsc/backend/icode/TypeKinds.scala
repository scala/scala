/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode;

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

import scala.collection.mutable.{Map, HashMap};

trait TypeKinds requires ICodes {
  import global._;

  /** This class represents a type kind. Type kinds
   * represent the types that the VM know (or the ICode
   * view of what VMs know).
   */
  abstract class TypeKind {

    /** Returns a string representation of this type kind. */
    override def toString(): String = this match {
      case UNIT    => "UNIT";
      case BOOL    => "BOOL";
      case BYTE    => "BYTE"
      case SHORT   => "SHORT";
      case CHAR    => "CHAR";
      case INT     => "INT";
      case LONG    => "LONG";
      case FLOAT   => "FLOAT";
      case DOUBLE  => "DOUBLE";
      case REFERENCE(cls) => "REFERENCE(" + cls.fullNameString + ")" ;
      case ARRAY(elem) => "ARRAY[" + elem + "]";
      case _       => abort("Unkown type kind.");
    }

    def toType: Type = this match {
      case UNIT            => definitions.UnitClass.info;
      case BOOL            => definitions.BooleanClass.info;
      case BYTE            => definitions.ByteClass.info;
      case SHORT           => definitions.ShortClass.info;
      case CHAR            => definitions.CharClass.info;
      case INT             => definitions.IntClass.info;
      case LONG            => definitions.LongClass.info;
      case FLOAT           => definitions.FloatClass.info;
      case DOUBLE          => definitions.DoubleClass.info;
      case REFERENCE(cls)  => typeRef(cls.typeConstructor.prefix, cls, Nil);
      case ARRAY(elem)     => typeRef(definitions.ArrayClass.typeConstructor.prefix,
                                      definitions.ArrayClass,
                                      elem.toType :: Nil);
      case _ => abort("Unknown type kind.");
    }

    def isReferenceType: Boolean = false;
    def isArrayType: Boolean = false;
    def isValueType: Boolean = !isReferenceType && !isArrayType;


    def isIntType: Boolean = this match {
      case BYTE | SHORT | INT | LONG | CHAR => true;
      case _ => false;
    }

    def isRealType: Boolean = this match {
      case FLOAT | DOUBLE => true;
      case _ => false;
    }

    def isNumericType: Boolean = isIntType | isRealType;

    def maxType(other: TypeKind): TypeKind;

    /** Simple subtyping check */
    def <:<(other: TypeKind): Boolean = (this == other);

    override def equals(other: Any): Boolean =
      this eq other.asInstanceOf[AnyRef];
  }

  /**
   * The least upper bound of two typekinds. They have to be either
   * REFERENCE or ARRAY kinds.
   *
   * The lub is based on the lub of scala types.
   */
  def lub(a: TypeKind, b: TypeKind): TypeKind = {
    def lub0(t1: Type, t2: Type): Type = {
      val lubTpe = global.lub(t1 :: t2 :: Nil);
      assert(lubTpe.symbol.isClass,
             "Least upper bound of " + t1 + " and " + t2 + " is not a class: " + lubTpe);
      lubTpe;
    }

    if ((a.isReferenceType || a.isArrayType) &&
        (b.isReferenceType || b.isArrayType))
      toTypeKind(lub0(a.toType, b.toType))
    else if (a == b) a
    else if (a == REFERENCE(definitions.AllClass)) b
    else if (b == REFERENCE(definitions.AllClass)) a
    else throw new CheckerError("Incompatible types: " + a + " with " + b);
  }

  /** The unit value */
  case object UNIT extends TypeKind {
    def maxType(other: TypeKind): TypeKind = other match {
      case UNIT => UNIT;
      case _ => abort("Uncomparbale type kinds: UNIT with " + other);
    }
  }

  /** A boolean value */
  case object BOOL extends TypeKind {
    override def maxType(other: TypeKind): TypeKind = other match {
      case BOOL => BOOL;
      case _ => abort("Uncomparbale type kinds: BOOL with " + other);
    }
  }

  /** A 1-byte signed integer */
  case object BYTE extends TypeKind {
    override def maxType(other: TypeKind): TypeKind =
      other match {
        case INT => INT;
        case BYTE | SHORT | INT | LONG | FLOAT | DOUBLE => other;
        case _ => abort("Uncomparbale type kinds: BYTE with " + other);
      }
  }

  /** A 2-byte signed integer */
  case object SHORT extends TypeKind {
    override def maxType(other: TypeKind): TypeKind =
      other match {
        case BYTE | SHORT | CHAR => SHORT;
        case INT | LONG | FLOAT | DOUBLE => other;
        case _ => abort("Uncomparbale type kinds: SHORT with " + other);
      }
  }

  /** A 2-byte signed integer */
  case object CHAR extends TypeKind {
    override def maxType(other: TypeKind): TypeKind =
      other match {
        case BYTE | SHORT | CHAR => CHAR;
        case INT | LONG | FLOAT | DOUBLE => other;
        case _ => abort("Uncomparbale type kinds: CHAR with " + other);
      }
  }


  /** A 4-byte signed integer */
  case object INT extends TypeKind {
    override def maxType(other: TypeKind): TypeKind =
      other match {
        case BYTE | SHORT | CHAR | INT => INT;
        case LONG | FLOAT | DOUBLE => other;
        case _ => abort("Uncomparbale type kinds: INT with " + other);
      }
  }

  /** An 8-byte signed integer */
  case object LONG extends TypeKind {
    override def maxType(other: TypeKind): TypeKind =
      other match {
        case BYTE | SHORT | CHAR | INT | LONG => LONG;
        case FLOAT | DOUBLE => DOUBLE;
        case _ => abort("Uncomparbale type kinds: LONG with " + other);
      }
  }

  /** A 4-byte floating point number */
  case object FLOAT extends TypeKind {
    override def maxType(other: TypeKind): TypeKind = other match {
      case BYTE | SHORT | CHAR | INT | FLOAT => FLOAT;
      case DOUBLE => DOUBLE;
      case _ => abort("Uncomparbale type kinds: FLOAT with " + other);
    }
  }

  /** An 8-byte floating point number */
  case object DOUBLE extends TypeKind {
    override def maxType(other: TypeKind): TypeKind =
      if (other.isNumericType)
        DOUBLE;
      else
        abort("Uncomparbale type kinds: DOUBLE with " + other);
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
  case class REFERENCE(cls: Symbol) extends TypeKind {
    assert(cls != null,
           "REFERENCE to null class symbol.");
    assert(cls != definitions.ArrayClass,
           "REFERENCE to Array is not allowed, should be ARRAY[..] instead");

    override def toString(): String =
      "REFERENCE(" + cls.fullNameString + ")";

    /**
     * Approximate `lub'. The common type of two references is
     * always AnyRef. For 'real' least upper bound wrt to subclassing
     * use method 'lub'.
     */
    override def maxType(other: TypeKind): TypeKind =
      other match {
        case REFERENCE(_) => REFERENCE(definitions.AnyRefClass);
        case _ =>
          abort("Uncomparbale type kinds: REFERENCE with " + other);
      }

    /** Checks subtyping relationship. */
    override def <:<(other: TypeKind): Boolean =
      if (cls == definitions.AllClass)
        true
      else other match {
        case REFERENCE(cls2) =>
          cls.tpe <:< cls2.tpe;
        case ARRAY(_) =>
          cls == definitions.AllRefClass;
        case _ => false;
      }

    override def isReferenceType: Boolean = true;

    override def equals(other: Any): Boolean = other match {
      case REFERENCE(cls2) => cls == cls2;
      case _               => false;
    }
  }


  case class ARRAY(val elem: TypeKind) extends TypeKind {
    override def toString(): String =
      "ARRAY[" + elem + "]";

    override def isArrayType = true;

    /**
     * Approximate `lub'. The common type of two references is
     * always AnyRef. For 'real' least upper bound wrt to subclassing
     * use method 'lub'.
     */
    override def maxType(other: TypeKind): TypeKind =
      other match {
        case REFERENCE(_) => REFERENCE(definitions.AnyRefClass);
        case ARRAY(elem2) => ARRAY(elem maxType elem2);
        case _ =>
          abort("Uncomparbale type kinds: ARRAY with " + other);
      }

    /** Checks subtyping relationship. */
    override def <:<(other: TypeKind): Boolean =
      other match {
        case ARRAY(elem2) =>
          elem <:< elem2;
        case REFERENCE(sym) =>
          (sym == definitions.AnyRefClass ||
           sym == definitions.ObjectClass) // TODO: platform dependent!

        case _ => false;
      }

    override def equals(other: Any): Boolean = other match {
      case ARRAY(elem2) => elem == elem2;
      case _               => false;
    }

  }

  ////////////////// Conversions //////////////////////////////


  /** Return the TypeKind of the given type */
  def toTypeKind(t: Type): TypeKind = t match {
    case ThisType(sym) => REFERENCE(sym);

    case SingleType(pre, sym) =>
      primitiveTypeMap get sym match {
        case Some(k) => k;
        case None    => REFERENCE(sym);
      }

    case ConstantType(value) =>
      toTypeKind(value.tpe);

    case TypeRef(_, sym, args) =>
      primitiveTypeMap get sym match {
        case Some(k) => k;
        case None    =>
          if (sym == definitions.ArrayClass)
            ARRAY(toTypeKind(args.head))
          else
            REFERENCE(sym);
      }

    case ClassInfoType(_, _, sym) =>
      primitiveTypeMap get sym match {
        case Some(k) => k;
        case None    =>
          if (sym == definitions.ArrayClass)
            abort("ClassInfoType to ArrayClass!");
          else
            REFERENCE(sym);
      }

    case _ => abort("Unknown type: " + t);
  }

  /** A map from scala primitive Types to ICode TypeKinds */
  private var primitiveTypeMap: Map[Symbol, TypeKind] = null;

  /** Initialize the map from scala primitive types to ICode types */
  def initPrimitiveTypeMap = {
    log("Initializing primitive map");
    primitiveTypeMap = new HashMap();
    primitiveTypeMap += definitions.UnitClass -> UNIT;
    primitiveTypeMap += definitions.BooleanClass -> BOOL;
    primitiveTypeMap += definitions.ByteClass -> BYTE;
    primitiveTypeMap += definitions.ShortClass -> SHORT;
    primitiveTypeMap += definitions.CharClass -> CHAR;
    primitiveTypeMap += definitions.IntClass -> INT;
    primitiveTypeMap += definitions.LongClass -> LONG;
    primitiveTypeMap += definitions.FloatClass -> FLOAT;
    primitiveTypeMap += definitions.DoubleClass -> DOUBLE;
//    primitiveTypeMap += definitions.StringClass -> STRING;
  }

}
