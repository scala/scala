package scala
package reflect
package internal

package object util {
  import StringOps.longestCommonPrefix

  // Shorten a name like Symbols$FooSymbol to FooSymbol.
  private def shortenName(name: String): String = {
    if (name == "") return ""
    val segments = (name split '$').toList
    val last     = segments.last

    if (last.length == 0)
      segments takeRight 2 mkString "$"
    else
      last
  }

  def shortClassOfInstance(x: AnyRef): String = shortClass(x.getClass)
  def shortClass(clazz: Class[_]): String = {
    val name: String = (clazz.getName split '.').last
    def isModule     = name endsWith "$"                        // object
    def isAnon       = (name split '$').last forall (_.isDigit) // anonymous class

    if (isModule)
      (name split '$' filterNot (_ == "")).last + "$"
    else if (isAnon) {
      val parents = clazz.getSuperclass :: clazz.getInterfaces.toList
      parents map (c => shortClass(c)) mkString " with "
    }
    else shortenName(name)
  }
}
