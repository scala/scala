/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package generic

/** Comment deleted, see internal.HasFlags. */
import Flags._

/** Common code utilized by `Modifiers` (which carry the flags associated
 *  with `Trees`) and `Symbol`.
 */
@deprecated("scala.reflect.generic will be removed", "2.9.1")
trait HasFlags {
  type FlagsType
  type AccessBoundaryType
  type AnnotationType

  /** Though both `Symbol` and `Modifiers` widen this method to public, it's
   *  defined protected here to give us the option in the future to route
   *  flag methods through accessors and disallow raw flag manipulation.
   *  And after that, perhaps, on some magical day: a typesafe enumeration.
   */
  protected def flags: FlagsType

  /** The printable representation of this entity's flags and access boundary,
   *  restricted to flags in the given mask.
   */
  def hasFlagsToString(mask: FlagsType): String

  /** Access level encoding: there are three scala flags (`PRIVATE`, `PROTECTED`,
   *  and `LOCAL`) which combine with value privateWithin (the `foo` in `private[foo]`)
   *  to define from where an entity can be accessed.  The meanings are as follows:
   *
   *  PRIVATE     access restricted to class only.
   *  PROTECTED   access restricted to class and subclasses only.
   *  LOCAL       can only be set in conjunction with PRIVATE or PROTECTED.
   *              Further restricts access to the same object instance.
   *
   *  In addition, `privateWithin` can be used to set a visibility barrier.
   *  When set, everything contained in the named enclosing package or class
   *  has access.  It is incompatible with `PRIVATE` or `LOCAL`, but is additive
   *  with `PROTECTED` (i.e. if either the flags or privateWithin allow access,
   *  then it is allowed.)
   *
   *  The Java access levels translate as follows:
   *  {{{
   *  java private:     hasFlag(PRIVATE)                && !hasAccessBoundary
   *  java package:     !hasFlag(PRIVATE | PROTECTED)   && (privateWithin == enclosing package)
   *  java protected:   hasFlag(PROTECTED)              && (privateWithin == enclosing package)
   *  java public:      !hasFlag(PRIVATE | PROTECTED)   && !hasAccessBoundary
   *  }}}
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

  /** Whether this entity has NONE of the flags in the given mask.
   */
  def hasNoFlags(mask: Long): Boolean = !hasFlag(mask)

  // Tests which come through cleanly: both Symbol and Modifiers use these
  // identically, testing for a single flag.
  def isCase      = hasFlag(CASE     )
  def isFinal     = hasFlag(FINAL    )
  def isImplicit  = hasFlag(IMPLICIT )
  def isLazy      = hasFlag(LAZY     )
  def isMutable   = hasFlag(MUTABLE  )  // in Modifiers, formerly isVariable
  def isOverride  = hasFlag(OVERRIDE )
  def isPrivate   = hasFlag(PRIVATE  )
  def isProtected = hasFlag(PROTECTED)
  def isSynthetic = hasFlag(SYNTHETIC)
  def isInterface = hasFlag(INTERFACE)

  // Newly introduced based on having a reasonably obvious clean translation.
  def isPrivateLocal   = hasAllFlags(PRIVATE | LOCAL)
  def isProtectedLocal = hasAllFlags(PROTECTED | LOCAL)
  def isParamAccessor  = hasFlag(PARAMACCESSOR)
  def isCaseAccessor   = hasFlag(CASEACCESSOR)
  def isSuperAccessor  = hasFlag(SUPERACCESSOR)
  def isLifted         = hasFlag(LIFTED)

  // Formerly the `Modifiers` impl did not include the access boundary check,
  // which must have been a bug.
  def isPublic = hasNoFlags(PRIVATE | PROTECTED) && !hasAccessBoundary

  // Renamed the Modifiers impl from isArgument.
  def isParameter = hasFlag(PARAM)

  // Removed isClass qualification since the flag isn't overloaded and
  // sym.isClass is enforced in Namers#validate.
  def isSealed = hasFlag(SEALED)

  // Removed !isClass qualification since the flag isn't overloaded.
  def isDeferred = hasFlag(DEFERRED )

  // Dropped isTerm condition because flag isn't overloaded.
  def isAbstractOverride = hasFlag(ABSOVERRIDE)

  def isDefaultInit = hasFlag(DEFAULTINIT)

  // Disambiguating: DEFAULTPARAM, TRAIT
  def hasDefault     = hasAllFlags(DEFAULTPARAM | PARAM)
  def isTrait        = hasFlag(TRAIT) && !hasFlag(PARAM)
  def hasTraitFlag   = hasFlag(TRAIT)
  def hasDefaultFlag = hasFlag(DEFAULTPARAM)

  // Straightforwardly named accessors already being used differently.
  // These names are most likely temporary.
  def hasAbstractFlag      = hasFlag(ABSTRACT)
  def hasAccessorFlag      = hasFlag(ACCESSOR)
  def hasLocalFlag         = hasFlag(LOCAL)
  def hasModuleFlag        = hasFlag(MODULE)
  def hasPackageFlag       = hasFlag(PACKAGE)
  def hasPreSuperFlag      = hasFlag(PRESUPER)
  def hasStableFlag        = hasFlag(STABLE)
  def hasStaticFlag        = hasFlag(STATIC)

   // Disambiguating: BYNAMEPARAM, CAPTURED, COVARIANT.
  def isByNameParam      = hasAllFlags(BYNAMEPARAM | PARAM)
  // Nope, these aren't going to fly:
  // def isCapturedVariable = hasAllFlags(CAPTURED | MUTABLE)
  // def isCovariant        = hasFlag(COVARIANT) && hasNoFlags(PARAM | MUTABLE)

  // Disambiguating: LABEL, CONTRAVARIANT, INCONSTRUCTOR
  def isLabel = hasAllFlags(LABEL | METHOD) && !hasAccessorFlag
  // Cannot effectively disambiguate the others at this level.
  def hasContravariantFlag = hasFlag(CONTRAVARIANT)
  def hasInConstructorFlag = hasFlag(INCONSTRUCTOR)

  // Name
  def isJavaDefined = hasFlag(JAVA)

  // Keeping some potentially ambiguous names around so as not to break
  // the rest of the world
  @deprecated("", "2.9.0")
  def isAbstract = hasFlag(ABSTRACT)
  // Problematic:
  // ABSTRACT and DEFERRED too easy to confuse, and
  // ABSTRACT + OVERRIDE ==> ABSOVERRIDE adds to it.
  //
  // final def isAbstractClass = isClass && hasFlag(ABSTRACT)
  // def isAbstractType = false  // to be overridden

  // Question:
  // Which name? All other flags are isFlag so it's probably a mistake to
  // vary from that, but isAccessor does sound like it includes the other
  // *ACCESSOR flags. Perhaps something like isSimpleAccessor.
  //
  // def isAccessor      = hasFlag(ACCESSOR )
  // final def isGetterOrSetter = hasAccessorFlag
}
