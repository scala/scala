package scala.tools.nsc.symtab;

object Flags {

  // modifiers
  val DEFERRED      = 0x00000001;   // was `abstract' for members
  val FINAL         = 0x00000002;
  val PRIVATE       = 0x00000004;
  val PROTECTED     = 0x00000008;

  val SEALED        = 0x00000010;
  val OVERRIDE      = 0x00000020;
  val CASE          = 0x00000040;
  val ABSTRACT      = 0x00000080;   // abstract class, or used in conjunction
                                    // with abstract override.
                                    // Note difference to DEFERRED!

  val METHOD        = 0x00000100;   // a def parameter
  val TRAIT         = 0x00000200;   // a trait
  val JAVA          = 0x00000400;   // symbol was defined by a Java class
  val MODULE        = 0x00000800;   // symbol is module or class implementing a module

  val MUTABLE       = 0x00001000;   // symbol is a mutable variable.
  val PARAM         = 0x00002000;   // symbol is a (value or type) parameter to a method
  val PACKAGE       = 0x00004000;   // symbol is a java package
  val DEPRECATED    = 0x00008000;   // symbol is deprecated.

  val COVARIANT     = 0x00010000;   // symbol is a covariant type variable
  val CONTRAVARIANT = 0x00020000;   // symbol is a contravariant type variable
  val SYNTHETIC     = 0x00040000;   // symbol is compiler-generated
  val STABLE        = 0x00080000;   // functions that are assumed to be stable
				    // (typically, access methods for valdefs)
  val INITIALIZED   = 0x00100000;   // symbol's definition is complete
  val LOCKED        = 0x00200000;   // temporary flag to catch cyclic dependencies
  val ACCESSED      = 0x00400000;   // symbol was accessed at least once
  val SELECTOR      = 0x00800000;   // symbol was used as selector in Select

  val CAPTURED      = 0x01000000;   // variables is accessed from nested function. Set by LambdaLift
  val ACCESSOR      = 0x02000000;   // function is an access function for a value or variable
  val ACCESS_METHOD = 0x04000000;   // function is an access function for a method in some
                                    // outer class; set by ExplicitOuter
  val PARAMACCESSOR = 0x08000000;   // for methods: is an access method for a val parameter
                                    // for parameters: is a val parameter

  val LABEL         = 0x10000000;   // symbol is a label. Set by TailCall
  val BRIDGE        = 0x20000000;   // function is a bridge method. Set by Erasure
  val IS_ERROR      = 0x40000000;   // symbol is an error symbol

  val TRANS_FLAG    = 0x80000000;   // transient flag guaranteed to be reset after each phase.
  val LIFTED        = TRANS_FLAG;   // transient flag for lambdalift
  val INCONSTRUCTOR = TRANS_FLAG;   // transient flag for analyzer

  // masks
  val SOURCEFLAGS   = 0x00037777;    // these modifiers can be set in source programs.
  val ACCESSFLAGS   = PRIVATE | PROTECTED;
  val VARIANCES     = COVARIANT | CONTRAVARIANT;
  val CONSTRFLAGS   = CASE | JAVA;

  /** Flags already set by object creation and never set afterwards */
  val CREATIONFLAGS = ACCESSFLAGS | METHOD | MODULE | MUTABLE | PARAM | PACKAGE |
    COVARIANT | CONTRAVARIANT | SYNTHETIC | STABLE | ACCESSOR | PARAMACCESSOR | IS_ERROR;

  /** Module flags inherited by their module-class */
  val MODULE2CLASSFLAGS = ACCESSFLAGS | PACKAGE;

  def flagsToString(flags: long): String =
    List.range(0, 63)
      .map(i => flagToString(flags & (1L << i)))
      .filter("" !=).mkString("", " ", "");

  private def flagToString(flag: long): String = flag.asInstanceOf[int] match {
    case PRIVATE   => "private"
    case PROTECTED => "protected"
    case ABSTRACT  => "abstract"
    case FINAL     => "final"
    case SEALED    => "sealed"
    case TRAIT     => "trait"
    case OVERRIDE  => "override"
    case CASE      => "case"
    case _ => ""
  }
}
