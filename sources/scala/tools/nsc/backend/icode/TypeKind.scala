
// $Id$

package scala.tools.nsc.backend.icode;

/** This class represents a type kind. Type kinds
  * represent the types that the VM know (or the ICode
  * view of what VMs know.
  */
class TypeKind {

  /** Returns a string representation of this type kind. */
  override def toString(): String = this match {
    case UNIT    => "UNIT";
    case BOOL    => "BOOL";
    case U1      => "U1";
    case U2      => "U2";
    case U4      => "U4";
    case U8      => "U8";
    case I1      => "I1";
    case I2      => "I2";
    case I4      => "I4";
    case I8      => "I8";
    case R4      => "R4";
    case R8      => "R8";
    case REF     => "REF";
    case STRING  => "STR";
    case NULL    => "NULL";
    case ZERO    => "ZERO";
  }
}

/** The unit value */
case object UNIT extends TypeKind;

/** A boolean value */
case object BOOL extends TypeKind;

/** A 1-byte unsigned integer */
case object U1 extends TypeKind;

/** A 2-byte unsigned integer */
case object U2 extends TypeKind;

/** A 4-byte unsigned integer */
case object U4 extends TypeKind;

/** An 8-byte unsigned integer */
case object U8 extends TypeKind;

/** A 1-byte signed integer */
case object I1 extends TypeKind;

/** A 2-byte signed integer */
case object I2 extends TypeKind;

/** A 4-byte signed integer */
case object I4 extends TypeKind;

/** An 8-byte signed integer */
case object I8 extends TypeKind;

/** A 4-byte floating point number */
case object R4 extends TypeKind;

/** An 8-byte floating point number */
case object R8 extends TypeKind;

/** An object reference */
case object REF extends TypeKind;

/** A string reference */
case object STRING extends TypeKind;

/** The null reference */
case object NULL extends TypeKind;

/** The zero value */
case object ZERO extends TypeKind;
