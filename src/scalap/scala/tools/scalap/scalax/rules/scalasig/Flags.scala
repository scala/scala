package scala.tools.scalap
package scalax
package rules
package scalasig

trait Flags {
  def hasFlag(flag: Long): Boolean

  def isImplicit = hasFlag(0x00000001)
  def isFinal = hasFlag(0x00000002)
  def isPrivate = hasFlag(0x00000004)
  def isProtected = hasFlag(0x00000008)

  def isSealed = hasFlag(0x00000010)
  def isOverride = hasFlag(0x00000020)
  def isCase = hasFlag(0x00000040)
  def isAbstract = hasFlag(0x00000080)

  def isDeferred = hasFlag(0x00000100)
  def isMethod = hasFlag(0x00000200)
  def isModule = hasFlag(0x00000400)
  def isInterface = hasFlag(0x00000800)

  def isMutable = hasFlag(0x00001000)
  def isParam = hasFlag(0x00002000)
  def isPackage = hasFlag(0x00004000)
  def isDeprecated = hasFlag(0x00008000)

  def isCovariant = hasFlag(0x00010000)
  def isCaptured = hasFlag(0x00010000)

  def isByNameParam = hasFlag(0x00010000)
  def isContravariant = hasFlag(0x00020000)
  def isLabel = hasFlag(0x00020000) // method symbol is a label. Set by TailCall
  def isInConstructor = hasFlag(0x00020000) // class symbol is defined in this/superclass constructor

  def isAbstractOverride = hasFlag(0x00040000)
  def isLocal = hasFlag(0x00080000)

  def isJava = hasFlag(0x00100000)
  def isSynthetic = hasFlag(0x00200000)
  def isStable = hasFlag(0x00400000)
  def isStatic = hasFlag(0x00800000)

  def isCaseAccessor = hasFlag(0x01000000)
  def isTrait = hasFlag(0x02000000)
  def isBridge = hasFlag(0x04000000)
  def isAccessor = hasFlag(0x08000000)

  def isSuperAccessor = hasFlag(0x10000000)
  def isParamAccessor = hasFlag(0x20000000)

  def isModuleVar = hasFlag(0x40000000) // for variables: is the variable caching a module value
  def isMonomorphic = hasFlag(0x40000000) // for type symbols: does not have type parameters
  def isLazy = hasFlag(0x80000000L) // symbol is a lazy val. can't have MUTABLE unless transformed by typer

  def isError = hasFlag(0x100000000L)
  def isOverloaded = hasFlag(0x200000000L)
  def isLifted = hasFlag(0x400000000L)

  def isMixedIn = hasFlag(0x800000000L)
  def isExistential = hasFlag(0x800000000L)

  def isExpandedName = hasFlag(0x1000000000L)
  def isImplementationClass = hasFlag(0x2000000000L)
  def isPreSuper = hasFlag(0x2000000000L)

}
