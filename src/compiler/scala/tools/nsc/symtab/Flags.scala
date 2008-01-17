/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab

object Flags extends Enumeration {

  // modifiers
  final val IMPLICIT      = 0x00000001
  final val FINAL         = 0x00000002
  final val PRIVATE       = 0x00000004
  final val PROTECTED     = 0x00000008

  final val SEALED        = 0x00000010
  final val OVERRIDE      = 0x00000020
  final val CASE          = 0x00000040
  final val ABSTRACT      = 0x00000080    // abstract class, or used in conjunction
                                          // with abstract override.
                                          // Note difference to DEFERRED!

  final val DEFERRED      = 0x00000100    // was `abstract' for members
  final val METHOD        = 0x00000200    // a method
  final val MODULE        = 0x00000400    // symbol is module or class implementing a module
  final val INTERFACE     = 0x00000800    // symbol is an interface

  final val MUTABLE       = 0x00001000    // symbol is a mutable variable.
  final val PARAM         = 0x00002000    // symbol is a (value or type) parameter to a method
  final val PACKAGE       = 0x00004000    // symbol is a java package
  final val DEPRECATED    = 0x00008000    // symbol is deprecated.

  final val COVARIANT     = 0x00010000    // symbol is a covariant type variable
  final val CAPTURED      = 0x00010000    // variable is accessed from nested function.
                                          // Set by LambdaLift
  final val BYNAMEPARAM   = 0x00010000    // parameter is by name
  final val CONTRAVARIANT = 0x00020000    // symbol is a contravariant type variable
  final val LABEL         = 0x00020000    // method symbol is a label. Set by TailCall
  final val INCONSTRUCTOR = 0x00020000    // class symbol is defined in this/superclass
                                          // constructor.
  final val ABSOVERRIDE   = 0x00040000    // combination of abstract & override
  final val LOCAL         = 0x00080000    // symbol is local to current class.
                                          // pre: PRIVATE or PROTECTED are also set
  final val JAVA          = 0x00100000    // symbol was defined by a Java class
  final val SYNTHETIC     = 0x00200000    // symbol is compiler-generated
  final val STABLE        = 0x00400000    // functions that are assumed to be stable
                                          // (typically, access methods for valdefs)
                                          // or classes that do not contain abstract types.
  final val STATIC        = 0x00800000    // static field, method or class

  final val CASEACCESSOR  = 0x01000000    // symbol is a case parameter (or its accessor)
  final val TRAIT         = 0x02000000    // symbol is a trait
  final val BRIDGE        = 0x04000000    // function is a bridge method. Set by Erasure
  final val ACCESSOR      = 0x08000000    // a value or variable accessor

  final val SUPERACCESSOR = 0x10000000    // a super accessor
  final val PARAMACCESSOR = 0x20000000    // for value definitions: is an access method
                                          // for a final val parameter
                                          // for parameters: is a val parameter
  final val MODULEVAR     = 0x40000000    // for term symbols: is the variable caching a module value
  final val MONOMORPHIC   = 0x40000000    // for type symbols: does not have type parameters
  final val LAZY          = 0x80000000L   // symbol is a lazy val. can't have MUTABLE unless transformed by typer

  final val IS_ERROR      = 0x100000000L  // symbol is an error symbol
  final val OVERLOADED    = 0x200000000L  // symbol is overloaded
  final val LIFTED        = 0x400000000L  // class has been lifted out to package level
                                          // local value has been lifted out to class level
                                          // todo: make LIFTED = latePRIVATE?
  final val MIXEDIN       = 0x800000000L  // term member has been mixed in
  final val EXISTENTIAL   = 0x800000000L  // type is an existential parameter or skolem

  final val EXPANDEDNAME  = 0x1000000000L // name has been expanded with class suffix
  final val IMPLCLASS     = 0x2000000000L // symbol is an implementation class
  final val PRESUPER      = 0x2000000000L // value is evaluated before super call
  final val TRANS_FLAG    = 0x4000000000L // transient flag guaranteed to be reset
                                          // after each phase.

  final val LOCKED        = 0x8000000000L // temporary flag to catch cyclic dependencies

  final val InitialFlags  = 0x000000FFFFFFFFFFL // flags that are enabled from phase 1.
  final val LateFlags     = 0x000FFF0000000000L // flags that override flags in 0xFFF.
  final val AntiFlags     = 0x7FF0000000000000L // flags that cancel flags in 0x7FF
  final val LateShift     = 40L
  final val AntiShift     = 52L

  // late flags (set by a transformer phase)
  final val latePRIVATE   = (PRIVATE: Long) << LateShift
  final val lateDEFERRED  = (DEFERRED: Long) << LateShift
  final val lateINTERFACE = (INTERFACE: Long) << LateShift
  final val lateMODULE    = (MODULE: Long) << LateShift
  final val lateFINAL     = (FINAL: Long) << LateShift
  final val lateMETHOD    = (METHOD: Long) << LateShift
  final val notPRIVATE    = (PRIVATE: Long) << AntiShift
  final val notDEFERRED   = (DEFERRED: Long) << AntiShift
  final val notPROTECTED  = (PROTECTED: Long) << AntiShift
  final val notABSTRACT   = (ABSTRACT: Long) << AntiShift
  final val notOVERRIDE   = (OVERRIDE: Long) << AntiShift
  final val notMETHOD     = (METHOD: Long) << AntiShift

  final val STATICMODULE  = lateMODULE
  final val STATICMEMBER  = notOVERRIDE


  // masks
  /** This flags can be set when class or module symbol is first created. */
  final val TopLevelCreationFlags: Long =
    MODULE | PACKAGE | FINAL | JAVA

  /** These modifiers can be set explicitly in source programs. */
  final val ExplicitFlags: Long =
    PRIVATE | PROTECTED | ABSTRACT | FINAL | SEALED |
    OVERRIDE | CASE | IMPLICIT | ABSOVERRIDE | LAZY

  /** These modifiers appear in TreePrinter output. */
  final val PrintableFlags: Long =
    ExplicitFlags | LOCAL | SYNTHETIC | STABLE | CASEACCESSOR |
    ACCESSOR | SUPERACCESSOR | PARAMACCESSOR | BRIDGE | STATIC

  final val FieldFlags: Long =
    MUTABLE | CASEACCESSOR | PARAMACCESSOR | STATIC | FINAL | PRESUPER | LAZY

  final val AccessFlags: Long   = PRIVATE | PROTECTED
  final val VARIANCES     = COVARIANT | CONTRAVARIANT
  final val ConstrFlags: Long   = JAVA
  final val PickledFlags: Long  = 0xFFFFFFFFL

  /** Module flags inherited by their module-class */
  final val ModuleToClassFlags: Long = AccessFlags | MODULE | PACKAGE | CASE | SYNTHETIC

  private def listToString(ss: List[String]): String =
    ss.filter("" !=).mkString("", " ", "")

  def flagsToString(flags: Long): String =
    listToString(for (i <- List.range(0, 63)) yield flagToString(flags & (1L << i)))

  def flagsToString(flags: Long, privateWithin: String): String = {
    var f = flags
    val pw =
      if (privateWithin == "") {
        if ((flags & (PRIVATE | LOCAL)) == (PRIVATE | LOCAL)) {
          f = f & ~(PRIVATE | LOCAL)
          "private[this]"
        } else if ((flags & (PROTECTED | LOCAL)) == (PROTECTED | LOCAL)) {
          f = f & ~(PROTECTED | LOCAL)
          "protected[this]"
        } else {
          ""
        }
      } else if ((f & PROTECTED) != 0) {
        f = f & ~PROTECTED
        "protected[" + privateWithin + "]"
      } else {
        "private[" + privateWithin + "]"
      }
    listToString(List(flagsToString(f), pw))
  }

  private def flagToString(flag: Long): String = {
    if (flag == IS_ERROR) "<is-error>"
    else if (flag == OVERLOADED  ) "<overloaded>"
    else if (flag == LIFTED      ) "<lifted>"
    else if (flag == MIXEDIN     ) "<mixedin/existential>"
    else if (flag == EXPANDEDNAME) "<expandedname>"
    else if (flag == IMPLCLASS   ) "<presuper/implclass>"
    else if (flag == TRANS_FLAG  ) "<trans-flag>"
    else if (flag == LOCKED      ) "<locked>"
    else if (flag == LAZY        ) "lazy"
    else flag.asInstanceOf[Int] match {
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
      case MODULE        => "<module>"
      case INTERFACE     => "<interface>"

      case MUTABLE       => "<mutable>"
      case PARAM         => "<param>"
      case PACKAGE       => "<package>"
      case DEPRECATED    => "<deprecated>"

      case COVARIANT     => "<covariant/captured/byname>"
      case CONTRAVARIANT => "<contravariant/label/inconstr>"
      case ABSOVERRIDE   => "abstract override"
      case LOCAL         => "<local>"

      case JAVA          => "<java>"
      case SYNTHETIC     => "<synthetic>"
      case STABLE        => "<stable>"
      case STATIC        => "<static>"

      case CASEACCESSOR  => "<caseaccessor>"
      case TRAIT         => "<trait>"
      case BRIDGE        => "<bridge>"
      case ACCESSOR      => "<accessor>"

      case SUPERACCESSOR => "<superaccessor>"
      case PARAMACCESSOR => "<paramaccessor>"

      case _ => ""
    }
  }

  class Flag(mods: Long) {
    def isPrivate   = (mods & PRIVATE  ) != 0
    def isProtected = (mods & PROTECTED) != 0
    def isVariable  = (mods &   MUTABLE) != 0
    def isPublic    = !isPrivate && !isProtected
  }
  case class FlagEnum(mask: Long) extends Val(maskToBit(mask), flagToString(mask))

  val Implicit      = FlagEnum(IMPLICIT)
  val Final         = FlagEnum(FINAL)
  val Private       = FlagEnum(PRIVATE)
  val Protected     = FlagEnum(PROTECTED)
  val Sealed        = FlagEnum(SEALED)
  val Override      = FlagEnum(OVERRIDE)
  val Case          = FlagEnum(CASE)
  val Abstract      = FlagEnum(ABSTRACT)
  val Deferred      = FlagEnum(DEFERRED)
  val Method        = FlagEnum(METHOD)
  val Module        = FlagEnum(MODULE)
  val Interface     = FlagEnum(INTERFACE)
  val Mutable       = FlagEnum(MUTABLE)
  val Param         = FlagEnum(PARAM)
  val Package       = FlagEnum(PACKAGE)
  val Deprecated    = FlagEnum(DEPRECATED)
  val Covariant     = FlagEnum(COVARIANT)
  val Contravariant = FlagEnum(CONTRAVARIANT)
  val AbsOverride   = FlagEnum(ABSOVERRIDE)
  val Local         = FlagEnum(LOCAL)
  val Synthetic     = FlagEnum(SYNTHETIC)
  val Stable        = FlagEnum(STABLE)
  val CaseAccessor  = FlagEnum(CASEACCESSOR)
  val Trait         = FlagEnum(TRAIT)
  val Bridge        = FlagEnum(BRIDGE)
  val Accessor      = FlagEnum(ACCESSOR)
  val SuperAccessor = FlagEnum(SUPERACCESSOR)
  val ParamAccessor = FlagEnum(PARAMACCESSOR)
  val ModuleVar     = FlagEnum(MODULEVAR)

}
