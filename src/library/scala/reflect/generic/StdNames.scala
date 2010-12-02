package scala.reflect
package generic

import scala.reflect.NameTransformer

trait StdNames { self: Universe =>

  val nme: StandardNames

  def encode(str: String): Name = newTermName(NameTransformer.encode(str))

  class StandardNames {
    val EXPAND_SEPARATOR_STRING = "$$"
    val LOCAL_SUFFIX_STRING = " "

    val EMPTY              = newTermName("")
    val EMPTY_PACKAGE_NAME = newTermName("<empty>")
    val IMPORT             = newTermName("<import>")
    val REFINE_CLASS_NAME  = newTermName("<refinement>")
    val ROOT               = newTermName("<root>")

    val ANON_CLASS_NAME    = newTermName("$anon")
    val ANON_FUN_NAME      = newTermName("$anonfun")
    val MODULE_SUFFIX      = newTermName("$module")
    val ROOTPKG            = newTermName("_root_")

    /** The expanded name of `name' relative to this class `base` with given `separator`
     */
    def expandedName(name: Name, base: Symbol, separator: String = EXPAND_SEPARATOR_STRING): Name =
      newTermName(base.fullName('$') + separator + name)

    def moduleVarName(name: Name): Name =
      newTermName(name.toString + MODULE_SUFFIX)
  }
}
