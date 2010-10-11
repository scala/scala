package scala.reflect
package generic

import Flags._

/** Common code utilized by Modifiers (which carry the flags associated
 *  with Trees) and Symbol.
 */
trait HasFlags {
  type FlagsType
  type AccessBoundaryType
  type AnnotationType

  /** Though both Symbol and Modifiers widen this method to public, it's
   *  defined protected here to give us the option in the future to route
   *  flag methods through accessors and disallow raw flag manipulation.
   *  And after that, perhaps, on some magical day: a typesafe enumeration.
   */
  protected def flags: FlagsType

  /** The printable representation of this entity's flags and access boundary,
   *  restricted to flags in the given mask.
   */
  def hasFlagsToString(mask: FlagsType): String

  /** Access level encoding: there are three scala flags (PRIVATE, PROTECTED,
   *  and LOCAL) which combine with value privateWithin (the "foo" in private[foo])
   *  to define from where an entity can be accessed.  The meanings are as follows:
   *
   *  PRIVATE     access restricted to class only.
   *  PROTECTED   access restricted to class and subclasses only.
   *  LOCAL       can only be set in conjunction with PRIVATE or PROTECTED.
   *              Further restricts access to the same object instance.
   *
   *  In addition, privateWithin can be used to set a visibility barrier.
   *  When set, everything contained in the named enclosing package or class
   *  has access.  It is incompatible with PRIVATE or LOCAL, but is additive
   *  with PROTECTED (i.e. if either the flags or privateWithin allow access,
   *  then it is allowed.)
   *
   *  The java access levels translate as follows:
   *
   *  java private:     hasFlag(PRIVATE)                && !hasAccessBoundary
   *  java package:     !hasFlag(PRIVATE | PROTECTED)   && (privateWithin == enclosing package)
   *  java protected:   hasFlag(PROTECTED)              && (privateWithin == enclosing package)
   *  java public:      !hasFlag(PRIVATE | PROTECTED)   && !hasAccessBoundary
   */
  def privateWithin: AccessBoundaryType

  /** A list of annotations attached to this entity.
   */
  def annotations: List[AnnotationType]

  /** Whether this entity has a "privateWithin" visibility barrier attached.
   */
  def hasAccessBoundary: Boolean

  /** Whether this entity has ANY of the flags in the given mask.
   */
  def hasFlag(flag: Long): Boolean

  /** Whether this entity has ALL of the flags in the given mask.
   */
  def hasAllFlags(mask: Long): Boolean

  // Tests which come through cleanly: both Symbol and Modifiers use these
  // identically, testing for a single flag.
  def isCase          = hasFlag(CASE     )
  def isFinal         = hasFlag(FINAL    )
  def isImplicit      = hasFlag(IMPLICIT )
  def isLazy          = hasFlag(LAZY     )
  def isMutable       = hasFlag(MUTABLE  )  // in Modifiers, formerly isVariable
  def isOverride      = hasFlag(OVERRIDE )
  def isPrivate       = hasFlag(PRIVATE  )
  def isProtected     = hasFlag(PROTECTED)
  def isSynthetic     = hasFlag(SYNTHETIC)

  // Newly introduced based on having a reasonably obvious clean translation.
  def isPrivateLocal   = isPrivate && hasFlag(LOCAL)
  def isProtectedLocal = isProtected && hasFlag(LOCAL)
  def isParamAccessor  = hasFlag(PARAMACCESSOR)
  def isCaseAccessor   = hasFlag(CASEACCESSOR)

  // Formerly the Modifiers impl did not include the access boundary check,
  // which must have been a bug.
  def isPublic = !hasFlag(PRIVATE | PROTECTED) && !hasAccessBoundary

  // Renamed the Modifiers impl from isArgument.
  def isParameter = hasFlag(PARAM)

  // Removed isClass qualification since the flag isn't overloaded and
  // sym.isClass is enforced in Namers#validate.
  def isSealed = hasFlag(SEALED)

  // Removed !isClass qualification since the flag isn't overloaded.
  def isDeferred = hasFlag(DEFERRED )

  // Problematic:
  // DEFAULTPARAM overloaded with TRAIT
  def hasDefault       = isParameter && hasFlag(DEFAULTPARAM)
  def hasDefaultFlag   = hasFlag(DEFAULTPARAM)
  // def isTrait          = hasFlag(TRAIT    )
  // def isTrait: Boolean = isClass && hasFlag(TRAIT) // refined later for virtual classes.

  // Problematic:
  // ABSTRACT and DEFERRED too easy to confuse, and
  // ABSTRACT + OVERRIDE ==> ABSOVERRIDE adds to it.
  //
  // def isAbstract      = hasFlag(ABSTRACT )
  // final def isAbstractClass = isClass && hasFlag(ABSTRACT)
  // def isAbstractType = false  // to be overridden

  // Question:
  // Which name? All other flags are isFlag so it's probably a mistake to
  // vary from that, but isAccessor does sound like it includes the other
  // *ACCESSOR flags. Perhaps something like isSimpleAccessor.
  //
  // def isAccessor      = hasFlag(ACCESSOR )
  // final def isGetterOrSetter = hasFlag(ACCESSOR)
}

