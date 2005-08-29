/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.symtab;

object Flags {

  // modifiers
  final val IMPLICIT      = 0x00000001;
  final val FINAL         = 0x00000002;
  final val PRIVATE       = 0x00000004;
  final val PROTECTED     = 0x00000008;

  final val SEALED        = 0x00000010;
  final val OVERRIDE      = 0x00000020;
  final val CASE          = 0x00000040;
  final val ABSTRACT      = 0x00000080;   // abstract class, or used in conjunction
                                    // with abstract override.
                                    // Note difference to DEFERRED!

  final val DEFERRED      = 0x00000100;   // was `abstract' for members
  final val METHOD        = 0x00000200;   // a def parameter
  final val TRAIT         = 0x00000400;   // a trait
  final val INTERFACE     = 0x00000800;   // symbol is an interface

  final val MUTABLE       = 0x00001000;   // symbol is a mutable variable.
  final val PARAM         = 0x00002000;   // symbol is a (value or type) parameter to a method
  final val PACKAGE       = 0x00004000;   // symbol is a java package
  final val DEPRECATED    = 0x00008000;   // symbol is deprecated.

  final val COVARIANT     = 0x00010000;   // symbol is a covariant type variable
  final val CONTRAVARIANT = 0x00020000;   // symbol is a contravariant type variable
  final val ABSOVERRIDE   = 0x00040000;   // combination of abstract & override
  final val LOCAL         = 0x00080000;   // symbol is local to current class.
	                            // pre: PRIVATE is also set

  final val JAVA          = 0x00100000;   // symbol was defined by a Java class
  final val SYNTHETIC     = 0x00200000;   // symbol is compiler-generated
  final val STABLE        = 0x00400000;   // functions that are assumed to be stable
				    // (typically, access methods for valdefs)
  final val STATIC        = 0x00800000;   // static field, method or class

  final val CASEACCESSOR  = 0x01000000;   // symbol is a case parameter (or its accessor)
  final val MODULE        = 0x02000000;   // symbol is module or class implementing a module
  final val BRIDGE        = 0x04000000;  // function is a bridge method. Set by Erasure
  final val ACCESSOR      = 0x08000000;   // a value or variable accessor

  final val ACCESS_METHOD = 0x10000000;   // function is an access function for a method in some
                                    // outer class; set by ExplicitOuter
  final val PARAMACCESSOR = 0x20000000;   // for value definitions: is an access method for a final val parameter
                                          // for parameters: is a val parameter

  final val LABEL         = 0x40000000;   // symbol is a label. Set by TailCall
  final val CAPTURED      = 0x80000000l;   // variable is accessed from nested function. Set by LambdaLift

  final val IS_ERROR      = 0x100000000l; // symbol is an error symbol
  final val OVERLOADED    = 0x200000000l; // symbol is overloaded

  final val TRANS_FLAG    = 0x400000000l; // transient flag guaranteed to be reset after each phase.
  final val INCONSTRUCTOR = TRANS_FLAG;   // transient flag for analyzer
  final val FLATTENED     = 0x800000000l; // class has been lifted out to package level

  final val INITIALIZED   = 0x1000000000l; // symbol's definition is complete
  final val LOCKED        = 0x2000000000l; // temporary flag to catch cyclic dependencies

  final val InitialFlags  = 0x000000FFFFFFFFFFl;       // flags that are enabled from phase 1.
  final val LateFlags     = 0x000FFF0000000000l;    // flags that override flags in 0xFFF.
  final val AntiFlags     = 0x7FF0000000000000l; // flags that cancel flags in 0x7FF
  final val LateShift     = 40l;
  final val AntiShift     = 52l;

  // late flags (set by a transformer phase)
  final val lateDEFERRED = (DEFERRED: long) << LateShift;
  final val lateINTERFACE = (INTERFACE: long) << LateShift;
  final val notPRIVATE   = (PRIVATE: long) << AntiShift;
  final val notPROTECTED = (PROTECTED: long) << AntiShift;

  // masks
  final val SourceFlags   = 0x002FFFFF;    // these modifiers can be set in source programs.
  final val ExplicitFlags =                // these modifiers can be set explicitly in source programs.
    PRIVATE | PROTECTED | ABSTRACT | FINAL | SEALED | OVERRIDE | CASE | IMPLICIT | ABSOVERRIDE;
  final val PrintableFlags =               // these modifiers appear in TreePrinter output.
    ExplicitFlags | LOCAL | SYNTHETIC | STABLE | CASEACCESSOR | ACCESSOR |
    ACCESS_METHOD | PARAMACCESSOR | LABEL | BRIDGE | STATIC;
  final val GenFlags      =                // these modifiers can be in generated trees
    SourceFlags | PrintableFlags;
  final val FieldFlags = MUTABLE | CASEACCESSOR | PARAMACCESSOR | STATIC | FINAL;

  final val AccessFlags   = PRIVATE | PROTECTED;
  final val VARIANCES     = COVARIANT | CONTRAVARIANT;
  final val ConstrFlags   = JAVA;
  final val PickledFlags  = 0xFFFFFFFF;

  /** Module flags inherited by their module-class */
  final val ModuleToClassFlags = AccessFlags | PACKAGE;

  def flags2mods(flags: long): int = flags.asInstanceOf[int] & GenFlags;

  def flagsToString(flags: long): String =
    List.range(0, 63)
      .map(i => flagToString(flags & (1L << i)))
      .filter("" !=).mkString("", " ", "");

  private def flagToString(flag: long): String = {
    if (flag == CAPTURED) "<captured>"
    else if (flag == INTERFACE) "<interface>"
    else if (flag == IS_ERROR) "<is-error>"
    else if (flag == OVERLOADED) "<overloaded>"
    else if (flag == TRANS_FLAG) "<trans-flag>"
    else if (flag == INITIALIZED) "<initialized>"
    else if (flag == LOCKED) "<locked>"
    else flag.asInstanceOf[int] match {
      case IMPLICIT      => "implicit"
      case FINAL         => "final"
      case PRIVATE       => "private"
      case PROTECTED     => "protected"

      case SEALED        => "sealed"
      case OVERRIDE      => "override"
      case CASE          => "case"
      case ABSTRACT      => "abstract"

      case DEFERRED      => "<deferred>"
      case METHOD        => "<method>"
      case TRAIT         => "<trait>"
      case MODULE        => "<module>"

      case MUTABLE       => "<mutable>"
      case PARAM         => "<param>"
      case PACKAGE       => "<package>"
      case DEPRECATED    => "<deprecated>"

      case COVARIANT     => "<covariant>"
      case CONTRAVARIANT => "<contravariant>"
      case ABSOVERRIDE   => "abstract override"
      case LOCAL         => "<local>"

      case JAVA          => "<java>"
      case SYNTHETIC     => "<synthetic>"
      case STABLE        => "<stable>"
      case STATIC        => "<static>"

      case CASEACCESSOR  => "<caseaccessor>"
      case ACCESSOR      => "<accessor>"

      case ACCESS_METHOD => "<access>"
      case PARAMACCESSOR => "<paramaccessor>"
      case LABEL         => "<label>"
      case BRIDGE        => "<bridge>"

      case _ => ""
    }
  }
}
