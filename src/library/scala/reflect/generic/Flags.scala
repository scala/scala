package scala.reflect
package generic

object Flags extends Flags

class Flags {

  // modifiers
  final val IMPLICIT      = 0x00000200
  final val FINAL         = 0x00000020
  final val PRIVATE       = 0x00000004
  final val PROTECTED     = 0x00000001

  final val SEALED        = 0x00000400
  final val OVERRIDE      = 0x00000002
  final val CASE          = 0x00000800
  final val ABSTRACT      = 0x00000008    // abstract class, or used in conjunction
                                          // with abstract override.
                                          // Note difference to DEFERRED!

  final val DEFERRED      = 0x00000010    // was `abstract' for members | trait is virtual
  final val METHOD        = 0x00000040    // a method
  final val MODULE        = 0x00000100    // symbol is module or class implementing a module
  final val INTERFACE     = 0x00000080    // symbol is an interface (i.e. a trait which defines only abstract methods)

  final val MUTABLE       = 0x00001000    // symbol is a mutable variable.
  final val PARAM         = 0x00002000    // symbol is a (value or type) parameter to a method
  final val PACKAGE       = 0x00004000    // symbol is a java package
  // available: 0x00008000

  final val COVARIANT     = 0x00010000    // symbol is a covariant type variable
  final val CAPTURED      = 0x00010000    // variable is accessed from nested function.
                                          // Set by LambdaLift
  final val BYNAMEPARAM   = 0x00010000    // parameter is by name
  final val CONTRAVARIANT = 0x00020000    // symbol is a contravariant type variable
  final val LABEL         = 0x00020000    // method symbol is a label. Set by TailCall
  final val INCONSTRUCTOR = 0x00020000    // class symbol is defined in this/superclass
                                          // constructor.
  final val ABSOVERRIDE   = 0x00040000    // combination of abstract & override
  final val LOCAL         = 0x00080000    // symbol is local to current class (i.e. private[this] or protected[this]
                                          // pre: PRIVATE or PROTECTED are also set
  final val JAVA          = 0x00100000    // symbol was defined by a Java class
  final val SYNTHETIC     = 0x00200000    // symbol is compiler-generated
  final val STABLE        = 0x00400000    // functions that are assumed to be stable
                                          // (typically, access methods for valdefs)
                                          // or classes that do not contain abstract types.
  final val STATIC        = 0x00800000    // static field, method or class

  final val CASEACCESSOR  = 0x01000000    // symbol is a case parameter (or its accessor)
  final val TRAIT         = 0x02000000    // symbol is a trait
  final val DEFAULTPARAM  = 0x02000000    // the parameter has a default value
  final val BRIDGE        = 0x04000000    // function is a bridge method. Set by Erasure
  final val ACCESSOR      = 0x08000000    // a value or variable accessor (getter or setter)

  final val SUPERACCESSOR = 0x10000000    // a super accessor
  final val PARAMACCESSOR = 0x20000000    // for field definitions generated for primary constructor
                                          //   parameters (no matter if it's a 'val' parameter or not)
                                          // for parameters of a primary constructor ('val' or not)
                                          // for the accessor methods generated for 'val' or 'var' parameters
  final val MODULEVAR     = 0x40000000    // for variables: is the variable caching a module value
  final val SYNTHETICMETH = 0x40000000    // for methods: synthetic method, but without SYNTHETIC flag
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
  final val SPECIALIZED   = 0x10000000000L// symbol is a generated specialized member
  final val DEFAULTINIT   = 0x20000000000L// symbol is a generated specialized member
  final val VBRIDGE       = 0x40000000000L// symbol is a varargs bridge

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

  final val PickledFlags: Long  = 0xFFFFFFFFL

  private val rawPickledCorrespondence = List(
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
  /** A map from the raw to pickled flags, and vice versa.
   */
  private val r2p = mkCorrespondenceArray(rawPickledCorrespondence)
  private val p2r = mkCorrespondenceArray(rawPickledCorrespondence map (_.swap))

  def rawFlagsToPickled(flags: Long): Long =
    (flags & ~PKL_MASK) | r2p(flags.toInt & PKL_MASK)

  def pickledToRawFlags(pflags: Long): Long =
    (pflags & ~PKL_MASK) | p2r(pflags.toInt & PKL_MASK)

  // List of the raw flags, in pickled order
  protected val pickledListOrder: List[Long] = {
    val all   = 0 to 62 map (1L << _)
    val front = rawPickledCorrespondence map (_._1.toLong)

    front.toList ++ (all filterNot (front contains _))
  }
}
