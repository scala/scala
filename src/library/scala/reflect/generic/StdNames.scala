package scala.reflect
package generic

trait StdNames { self: Universe =>

  val nme: StandardNames

  class StandardNames {
    val EXPAND_SEPARATOR_STRING = "$$"
    val LOCAL_SUFFIX_STRING = " "

    val ANON_CLASS_NAME    = newTermName("$anon")
    val ANON_FUN_NAME      = newTermName("$anonfun")
    val EMPTY_PACKAGE_NAME = newTermName("<empty>")
    val IMPORT             = newTermName("<import>")
    val REFINE_CLASS_NAME  = newTermName("<refinement>")
    val ROOT               = newTermName("<root>")
    val ROOTPKG            = newTermName("_root_")
    val EMPTY              = newTermName("")

    /** The expanded name of `name' relative to this class `base` with given `separator`
     */
    def expandedName(name: Name, base: Symbol, separator: String = EXPAND_SEPARATOR_STRING): Name =
      newTermName(base.fullName('$') + separator + name)
  }
}
