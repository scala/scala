/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package symtab

object Flags extends reflect.generic.Flags {

  final val InitialFlags  = 0x0001FFFFFFFFFFFFL // flags that are enabled from phase 1.
  final val LateFlags     = 0x00FE000000000000L // flags that override flags in 0x1FC.
  final val AntiFlags     = 0x7F00000000000000L // flags that cancel flags in 0x07F
  final val LateShift     = 47L
  final val AntiShift     = 56L

  // late flags (set by a transformer phase)
  final val latePRIVATE   = (PRIVATE: Long) << LateShift
  final val lateABSTRACT  = (ABSTRACT: Long) << LateShift
  final val lateDEFERRED  = (DEFERRED: Long) << LateShift
  final val lateINTERFACE = (INTERFACE: Long) << LateShift
  final val lateMODULE    = (MODULE: Long) << LateShift
  final val lateFINAL     = (FINAL: Long) << LateShift
  final val lateMETHOD    = (METHOD: Long) << LateShift

  final val notFINAL      = (FINAL: Long) << AntiShift
  final val notPRIVATE    = (PRIVATE: Long) << AntiShift
  final val notDEFERRED   = (DEFERRED: Long) << AntiShift
  final val notPROTECTED  = (PROTECTED: Long) << AntiShift
  final val notABSTRACT   = (ABSTRACT: Long) << AntiShift
  final val notOVERRIDE   = (OVERRIDE: Long) << AntiShift
  final val notMETHOD     = (METHOD: Long) << AntiShift

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
    ACCESSOR | SUPERACCESSOR | PARAMACCESSOR | BRIDGE | STATIC | VBRIDGE

  /** The two bridge flags */
  final val BRIDGES = BRIDGE | VBRIDGE

  final val FieldFlags: Long =
    MUTABLE | CASEACCESSOR | PARAMACCESSOR | STATIC | FINAL | PRESUPER | LAZY

  final val AccessFlags: Long   = PRIVATE | PROTECTED | LOCAL
  final val VARIANCES     = COVARIANT | CONTRAVARIANT
  final val ConstrFlags: Long   = JAVA

  /** Module flags inherited by their module-class */
  final val ModuleToClassFlags: Long = AccessFlags | MODULE | PACKAGE | CASE | SYNTHETIC | JAVA

  private def listToString(ss: List[String]): String =
    ss.filter("" !=).mkString("", " ", "")

  def flagsToString(flags: Long): String =
    listToString(for (mask <- pickledListOrder) yield flagToString(flags & mask))

  def flagsToString(flags: Long, privateWithin: String): String = {
    var f = flags
    val pw =
      if (privateWithin == "") {
        if ((flags & (PRIVATE | LOCAL)) == (PRIVATE | LOCAL).toLong) {
          f = f & ~(PRIVATE | LOCAL)
          "private[this]"
        } else if ((flags & (PROTECTED | LOCAL)) == (PROTECTED | LOCAL).toLong) {
          f = f & ~(PROTECTED | LOCAL)
          "protected[this]"
        } else {
          ""
        }
      } else if ((f & PROTECTED) != 0L) {
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
    else flag.toInt match {
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

      case COVARIANT     => "<covariant/captured/byname>"
      case CONTRAVARIANT => "<contravariant/label/inconstr/defaultinit>"
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
      case VBRIDGE        => "<...bridge>"

      case _ => ""
    }
  }

  class Flag(mods: Long) {
    def isPrivate   = (mods & PRIVATE  ) != 0L
    def isProtected = (mods & PROTECTED) != 0L
    def isVariable  = (mods &   MUTABLE) != 0L
    def isPublic    = !isPrivate && !isProtected
  }
}
