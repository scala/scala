package scala.reflect
package generic

/** Flags set on Modifiers instances in the parsing stage.
 */
@deprecated("scala.reflect.generic will be removed", "2.9.1") class ModifierFlags {
  final val IMPLICIT      = 0x00000200
  final val FINAL         = 0x00000020
  final val PRIVATE       = 0x00000004
  final val PROTECTED     = 0x00000001

  final val SEALED        = 0x00000400
  final val OVERRIDE      = 0x00000002
  final val CASE          = 0x00000800
  final val ABSTRACT      = 0x00000008    // abstract class, or used in conjunction with abstract override.
                                          // Note difference to DEFERRED!
  final val DEFERRED      = 0x00000010    // was `abstract' for members | trait is virtual
  final val INTERFACE     = 0x00000080    // symbol is an interface (i.e. a trait which defines only abstract methods)
  final val MUTABLE       = 0x00001000    // symbol is a mutable variable.
  final val PARAM         = 0x00002000    // symbol is a (value or type) parameter to a method

  final val COVARIANT     = 0x00010000    // symbol is a covariant type variable
  final val BYNAMEPARAM   = 0x00010000    // parameter is by name
  final val CONTRAVARIANT = 0x00020000    // symbol is a contravariant type variable
  final val ABSOVERRIDE   = 0x00040000    // combination of abstract & override
  final val LOCAL         = 0x00080000    // symbol is local to current class (i.e. private[this] or protected[this]
                                          // pre: PRIVATE or PROTECTED are also set
  final val JAVA          = 0x00100000    // symbol was defined by a Java class
  final val STATIC        = 0x00800000    // static field, method or class
  final val CASEACCESSOR  = 0x01000000    // symbol is a case parameter (or its accessor)
  final val TRAIT         = 0x02000000    // symbol is a trait
  final val DEFAULTPARAM  = 0x02000000    // the parameter has a default value
  final val PARAMACCESSOR = 0x20000000    // for field definitions generated for primary constructor
                                          //   parameters (no matter if it's a 'val' parameter or not)
                                          // for parameters of a primary constructor ('val' or not)
                                          // for the accessor methods generated for 'val' or 'var' parameters
  final val LAZY          = 0x80000000L   // symbol is a lazy val. can't have MUTABLE unless transformed by typer
  final val PRESUPER      = 0x2000000000L // value is evaluated before super call
  final val DEFAULTINIT   = 0x20000000000L// symbol is initialized to the default value: used by -Xcheckinit

  // Overridden.
  def flagToString(flag: Long): String = ""
}
object ModifierFlags extends ModifierFlags

@deprecated("scala.reflect.generic will be removed", "2.9.1") class Flags extends ModifierFlags {
  final val METHOD        = 0x00000040    // a method
  final val MODULE        = 0x00000100    // symbol is module or class implementing a module
  final val PACKAGE       = 0x00004000    // symbol is a java package

  final val CAPTURED      = 0x00010000    // variable is accessed from nested function.  Set by LambdaLift.
  final val LABEL         = 0x00020000    // method symbol is a label. Set by TailCall
  final val INCONSTRUCTOR = 0x00020000    // class symbol is defined in this/superclass constructor.
  final val SYNTHETIC     = 0x00200000    // symbol is compiler-generated
  final val STABLE        = 0x00400000    // functions that are assumed to be stable
                                          // (typically, access methods for valdefs)
                                          // or classes that do not contain abstract types.
  final val BRIDGE        = 0x04000000    // function is a bridge method. Set by Erasure
  final val ACCESSOR      = 0x08000000    // a value or variable accessor (getter or setter)

  final val SUPERACCESSOR = 0x10000000    // a super accessor
  final val MODULEVAR     = 0x40000000    // for variables: is the variable caching a module value

  final val IS_ERROR      = 0x100000000L  // symbol is an error symbol
  final val OVERLOADED    = 0x200000000L  // symbol is overloaded
  final val LIFTED        = 0x400000000L  // class has been lifted out to package level
                                          // local value has been lifted out to class level
                                          // todo: make LIFTED = latePRIVATE?
  final val MIXEDIN       = 0x800000000L  // term member has been mixed in
  final val EXISTENTIAL   = 0x800000000L  // type is an existential parameter or skolem
  final val EXPANDEDNAME  = 0x1000000000L // name has been expanded with class suffix
  final val IMPLCLASS     = 0x2000000000L // symbol is an implementation class
  final val TRANS_FLAG    = 0x4000000000L // transient flag guaranteed to be reset after each phase.

  final val LOCKED        = 0x8000000000L // temporary flag to catch cyclic dependencies
  final val SPECIALIZED   = 0x10000000000L// symbol is a generated specialized member
  final val VBRIDGE       = 0x40000000000L// symbol is a varargs bridge

  final val VARARGS       = 0x80000000000L// symbol is a Java-style varargs method
  final val TRIEDCOOKING  = 0x100000000000L // ``Cooking'' has been tried on this symbol
                                            // A Java method's type is ``cooked'' by transforming raw types to existentials

  // pickling and unpickling of flags

  // The flags from 0x001 to 0x800 are different in the raw flags
  // and in the pickled format.

  private final val IMPLICIT_PKL   = (1 << 0)
  private final val FINAL_PKL      = (1 << 1)
  private final val PRIVATE_PKL    = (1 << 2)
  private final val PROTECTED_PKL  = (1 << 3)
  private final val SEALED_PKL     = (1 << 4)
  private final val OVERRIDE_PKL   = (1 << 5)
  private final val CASE_PKL       = (1 << 6)
  private final val ABSTRACT_PKL   = (1 << 7)
  private final val DEFERRED_PKL   = (1 << 8)
  private final val METHOD_PKL     = (1 << 9)
  private final val MODULE_PKL     = (1 << 10)
  private final val INTERFACE_PKL  = (1 << 11)

  private final val PKL_MASK       = 0x00000FFF

  // must pickle EXISTENTIAL for SI-6692
  final val PickledFlags: Long     = 0x8FFFFFFFFL

  private def rawPickledCorrespondence = Array(
    (IMPLICIT, IMPLICIT_PKL),
    (FINAL, FINAL_PKL),
    (PRIVATE, PRIVATE_PKL),
    (PROTECTED, PROTECTED_PKL),
    (SEALED, SEALED_PKL),
    (OVERRIDE, OVERRIDE_PKL),
    (CASE, CASE_PKL),
    (ABSTRACT, ABSTRACT_PKL),
    (DEFERRED, DEFERRED_PKL),
    (METHOD, METHOD_PKL),
    (MODULE, MODULE_PKL),
    (INTERFACE, INTERFACE_PKL)
  )
  private val rawFlags: Array[Int]     = rawPickledCorrespondence map (_._1)
  private val pickledFlags: Array[Int] = rawPickledCorrespondence map (_._2)

  // unused in 2.9.1: left to satisfy mima complaint about missing f$1
  private def mkCorrespondenceArray(correspondence: List[(Int, Int)]) = {
    def f(flags: Int): Int = {
      correspondence.foldLeft(0) {
        case (result, (oldFlag, newFlag)) =>
          if ((flags & oldFlag) != 0) result | newFlag
          else result
      }
    }
    0 to PKL_MASK map f toArray
  }

  private def r2p(flags: Int): Int = {
    var result = 0
    var i      = 0
    while (i < rawFlags.length) {
      if ((flags & rawFlags(i)) != 0)
        result |= pickledFlags(i)

      i += 1
    }
    result
  }
  private def p2r(flags: Int): Int = {
    var result = 0
    var i      = 0
    while (i < rawFlags.length) {
      if ((flags & pickledFlags(i)) != 0)
        result |= rawFlags(i)

      i += 1
    }
    result
  }

  // Generated by mkFlagToStringMethod() at Mon Oct 11 10:07:29 PDT 2010
  @annotation.switch override def flagToString(flag: Long): String = flag match {
    case           PROTECTED => "protected"                           // (1L << 0)
    case            OVERRIDE => "override"                            // (1L << 1)
    case             PRIVATE => "private"                             // (1L << 2)
    case            ABSTRACT => "abstract"                            // (1L << 3)
    case            DEFERRED => "<deferred>"                          // (1L << 4)
    case               FINAL => "final"                               // (1L << 5)
    case              METHOD => "<method>"                            // (1L << 6)
    case           INTERFACE => "<interface>"                         // (1L << 7)
    case              MODULE => "<module>"                            // (1L << 8)
    case            IMPLICIT => "implicit"                            // (1L << 9)
    case              SEALED => "sealed"                              // (1L << 10)
    case                CASE => "case"                                // (1L << 11)
    case             MUTABLE => "<mutable>"                           // (1L << 12)
    case               PARAM => "<param>"                             // (1L << 13)
    case             PACKAGE => "<package>"                           // (1L << 14)
    case             0x8000L => ""                                    // (1L << 15)
    case         BYNAMEPARAM => "<bynameparam/captured/covariant>"    // (1L << 16)
    case       CONTRAVARIANT => "<contravariant/inconstructor/label>" // (1L << 17)
    case         ABSOVERRIDE => "absoverride"                         // (1L << 18)
    case               LOCAL => "<local>"                             // (1L << 19)
    case                JAVA => "<java>"                              // (1L << 20)
    case           SYNTHETIC => "<synthetic>"                         // (1L << 21)
    case              STABLE => "<stable>"                            // (1L << 22)
    case              STATIC => "<static>"                            // (1L << 23)
    case        CASEACCESSOR => "<caseaccessor>"                      // (1L << 24)
    case        DEFAULTPARAM => "<defaultparam/trait>"                // (1L << 25)
    case              BRIDGE => "<bridge>"                            // (1L << 26)
    case            ACCESSOR => "<accessor>"                          // (1L << 27)
    case       SUPERACCESSOR => "<superaccessor>"                     // (1L << 28)
    case       PARAMACCESSOR => "<paramaccessor>"                     // (1L << 29)
    case           MODULEVAR => "<modulevar>"                         // (1L << 30)
    case                LAZY => "lazy"                                // (1L << 31)
    case            IS_ERROR => "<is_error>"                          // (1L << 32)
    case          OVERLOADED => "<overloaded>"                        // (1L << 33)
    case              LIFTED => "<lifted>"                            // (1L << 34)
    case         EXISTENTIAL => "<existential/mixedin>"               // (1L << 35)
    case        EXPANDEDNAME => "<expandedname>"                      // (1L << 36)
    case           IMPLCLASS => "<implclass/presuper>"                // (1L << 37)
    case          TRANS_FLAG => "<trans_flag>"                        // (1L << 38)
    case              LOCKED => "<locked>"                            // (1L << 39)
    case         SPECIALIZED => "<specialized>"                       // (1L << 40)
    case         DEFAULTINIT => "<defaultinit>"                       // (1L << 41)
    case             VBRIDGE => "<vbridge>"                           // (1L << 42)
    case      0x80000000000L => ""                                    // (1L << 43)
    case     0x100000000000L => ""                                    // (1L << 44)
    case     0x200000000000L => ""                                    // (1L << 45)
    case     0x400000000000L => ""                                    // (1L << 46)
    case     0x800000000000L => ""                                    // (1L << 47)
    case    0x1000000000000L => ""                                    // (1L << 48)
    case    0x2000000000000L => ""                                    // (1L << 49)
    case    0x4000000000000L => ""                                    // (1L << 50)
    case    0x8000000000000L => ""                                    // (1L << 51)
    case   0x10000000000000L => ""                                    // (1L << 52)
    case   0x20000000000000L => ""                                    // (1L << 53)
    case   0x40000000000000L => ""                                    // (1L << 54)
    case   0x80000000000000L => ""                                    // (1L << 55)
    case  0x100000000000000L => ""                                    // (1L << 56)
    case  0x200000000000000L => ""                                    // (1L << 57)
    case  0x400000000000000L => ""                                    // (1L << 58)
    case  0x800000000000000L => ""                                    // (1L << 59)
    case 0x1000000000000000L => ""                                    // (1L << 60)
    case 0x2000000000000000L => ""                                    // (1L << 61)
    case 0x4000000000000000L => ""                                    // (1L << 62)
    case 0x8000000000000000L => ""                                    // (1L << 63)
    case _ => ""
  }
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
    List(flagsToString(f), pw) filterNot (_ == "") mkString " "
  }
  def flagsToString(flags: Long): String =
    pickledListOrder map (mask => flagToString(flags & mask)) filterNot (_ == "") mkString " "

  def rawFlagsToPickled(flags: Long): Long =
    (flags & ~PKL_MASK) | r2p(flags.toInt & PKL_MASK)

  def pickledToRawFlags(pflags: Long): Long =
    (pflags & ~PKL_MASK) | p2r(pflags.toInt & PKL_MASK)

  // List of the raw flags, in pickled order
  protected val pickledListOrder: List[Long] = {
    val all   = 0 to 62 map (1L << _)
    val front = rawFlags map (_.toLong)

    front.toList ++ (all filterNot (front contains _))
  }
}

object Flags extends Flags
