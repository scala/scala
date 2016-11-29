package scala
package reflect
package internal

import Flags._

/** Common code utilized by Modifiers (which carry the flags associated
 *  with Trees) and Symbol.
 */
trait HasFlags {
  type AccessBoundaryType
  type AnnotationType

  /** Though both Symbol and Modifiers widen this method to public, it's
   *  defined protected here to give us the option in the future to route
   *  flag methods through accessors and disallow raw flag manipulation.
   *  And after that, perhaps, on some magical day: a typesafe enumeration.
   */
  protected def flags: Long

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

  /** Whether this entity has NONE of the flags in the given mask.
   */
  def hasNoFlags(mask: Long): Boolean = !hasFlag(mask)

  /** The printable representation of this entity's flags and access boundary,
   *  restricted to flags in the given mask.
   */
  def flagString: String = flagString(flagMask)
  def flagString(mask: Long): String = calculateFlagString(flags & mask)

  /** The default mask determining which flags to display.
   */
  def flagMask: Long = AllFlags

  /** The string representation of a single bit, seen from this
   *  flag carrying entity.
   */
  def resolveOverloadedFlag(flag: Long): String = Flags.flagToString(flag)

  // Tests which come through cleanly: both Symbol and Modifiers use these
  // identically, testing for a single flag.
  def hasAbstractFlag       = hasFlag(ABSTRACT)
  def hasAccessorFlag       = hasFlag(ACCESSOR)
  def hasDefault            = hasFlag(DEFAULTPARAM) && hasFlag(METHOD | PARAM) // Second condition disambiguates with TRAIT
  def hasJavaEnumFlag       = hasFlag(JAVA_ENUM)
  def hasJavaAnnotationFlag = hasFlag(JAVA_ANNOTATION)
  @deprecated("use isLocalToThis instead", "2.11.0")
  def hasLocalFlag          = hasFlag(LOCAL)
  def isLocalToThis         = hasFlag(LOCAL)
  def hasModuleFlag         = hasFlag(MODULE)
  def hasPackageFlag        = hasFlag(PACKAGE)
  def hasStableFlag         = hasFlag(STABLE)
  def hasStaticFlag         = hasFlag(STATIC)
  def isAbstractOverride    = hasFlag(ABSOVERRIDE)
  def isAnyOverride         = hasFlag(OVERRIDE | ABSOVERRIDE)
  def isCase                = hasFlag(CASE)
  def isCaseAccessor        = hasFlag(CASEACCESSOR)
  def isDeferred            = hasFlag(DEFERRED)
  def isFinal               = hasFlag(FINAL)
  def isArtifact            = hasFlag(ARTIFACT)
  def isImplicit            = hasFlag(IMPLICIT)
  def isInterface           = hasFlag(INTERFACE)
  def isJavaDefined         = hasFlag(JAVA)
  def isLabel               = hasAllFlags(LABEL | METHOD) && !hasAccessorFlag
  def isLazy                = hasFlag(LAZY)
  def isLifted              = hasFlag(LIFTED)
  def isMacro               = hasFlag(MACRO)
  def isMutable             = hasFlag(MUTABLE)
  def isOverride            = hasFlag(OVERRIDE)
  def isParamAccessor       = hasFlag(PARAMACCESSOR)
  def isPrivate             = hasFlag(PRIVATE)
  @deprecated ("use `hasPackageFlag` instead", "2.11.0")
  def isPackage             = hasFlag(PACKAGE)
  def isPrivateLocal        = hasAllFlags(PrivateLocal)
  def isProtected           = hasFlag(PROTECTED)
  def isProtectedLocal      = hasAllFlags(ProtectedLocal)
  def isPublic              = hasNoFlags(PRIVATE | PROTECTED) && !hasAccessBoundary
  def isSealed              = hasFlag(SEALED)
  def isSpecialized         = hasFlag(SPECIALIZED)
  def isSuperAccessor       = hasFlag(SUPERACCESSOR)
  def isSynthetic           = hasFlag(SYNTHETIC)
  def isTrait               = hasFlag(TRAIT) && !hasFlag(PARAM)
  def isTraitOrInterface    = isTrait || isInterface

  def flagBitsToString(bits: Long): String = {
    // Fast path for common case
    if (bits == 0L) "" else {
      var sb: StringBuilder = null
      var i = 0
      while (i <= MaxBitPosition) {
        val flag = Flags.rawFlagPickledOrder(i)
        if ((bits & flag) != 0L) {
          val s = resolveOverloadedFlag(flag)
          if (s.length > 0) {
            if (sb eq null) sb = new StringBuilder append s
            else if (sb.length == 0) sb append s
            else sb append " " append s
          }
        }
        i += 1
      }
      if (sb eq null) "" else sb.toString
    }
  }

  def accessString: String = {
    val pw = if (hasAccessBoundary) privateWithin.toString else ""

    if (pw == "") {
      if (hasAllFlags(PrivateLocal)) "private[this]"
      else if (hasAllFlags(ProtectedLocal)) "protected[this]"
      else if (hasFlag(PRIVATE)) "private"
      else if (hasFlag(PROTECTED)) "protected"
      else ""
    }
    else if (hasFlag(PROTECTED)) "protected[" + pw + "]"
    else "private[" + pw + "]"
  }
  protected def calculateFlagString(basis: Long): String = {
    val access    = accessString
    val nonAccess = flagBitsToString(basis & ~AccessFlags)

    if (access == "") nonAccess
    else if (nonAccess == "") access
    else nonAccess + " " + access
  }

  // Guess this can't be deprecated seeing as it's in the reflect API.
  def isParameter = hasFlag(PARAM)
}
