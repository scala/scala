package scala.reflect
package generic

import scala.reflect.NameTransformer

@deprecated("scala.reflect.generic will be removed", "2.9.1") trait StdNames {
  self: Universe =>

  val nme: LibraryTermNames
  val tpnme: LibraryTypeNames

  def encode(str: String): TermName = newTermName(NameTransformer.encode(str))

  implicit def stringToTermName(s: String): TermName = newTermName(s)

  trait LibraryCommonNames {
    type NameType <: Name
    implicit def createNameType(name: String): NameType

    val EMPTY: NameType              = ""
    val ANON_FUN_NAME: NameType      = "$anonfun"
    val EMPTY_PACKAGE_NAME: NameType = "<empty>"
    val IMPORT: NameType             = "<import>"
    val MODULE_SUFFIX: NameType      = "$module"
    val ROOT: NameType               = "<root>"
  }

  trait LibraryTermNames extends LibraryCommonNames {
    val EXPAND_SEPARATOR_STRING = "$$"
    val LOCAL_SUFFIX_STRING     = " "
    val ROOTPKG: NameType       = "_root_"

    /** The expanded name of `name' relative to this class `base` with given `separator`
     */
    def expandedName(name: TermName, base: Symbol, separator: String = EXPAND_SEPARATOR_STRING): TermName =
      newTermName(base.fullName('$') + separator + name)

    def moduleVarName(name: TermName): TermName = newTermName("" + name + MODULE_SUFFIX)
  }
  trait LibraryTypeNames extends LibraryCommonNames {
    val REFINE_CLASS_NAME: NameType  = "<refinement>"
    val ANON_CLASS_NAME: NameType    = "$anon"
  }
}
