package scala.tools.nsc

import util.ScalaClassLoader

trait ClassLoaders { self: Global =>

  def staticClass(fullname: String) = {
    if (self.forMSIL)
      throw new UnsupportedOperationException("Scala reflection not available on this platform")

    getClass(newTypeName(fullname))
  }

  def staticModule(fullname: String) = {
    if (self.forMSIL)
      throw new UnsupportedOperationException("Scala reflection not available on this platform")

    getModule(newTermName(fullname))
  }

  private def getClass(fullname: Name): Symbol = {
    var result = getModuleOrClass(fullname.toTypeName)
    while (result.isAliasType) result = result.info.typeSymbol
    result
  }

  private def getModule(fullname: Name): Symbol =
    getModuleOrClass(fullname.toTermName)

  private def getModuleOrClass(path: Name): Symbol =
    getModuleOrClass(path, path.length)

  private def getModuleOrClass(path: Name, len: Int): Symbol = {
    val point = path lastPos('.', len - 1)
    val owner =
      if (point > 0) getModuleOrClass(path.toTermName, point)
      else definitions.RootClass
    val name = path subName (point + 1, len)
    val sym = owner.info member name
    val result = if (path.isTermName) sym.suchThat(_ hasFlag symtab.Flags.MODULE) else sym
    if (result != NoSymbol) result
    else {
      if (settings.debug.value) { log(sym.info); log(sym.info.members) }//debug
      if (owner.isRoot && isJavaClass(name.toString))
        definitions.EmptyPackageClass.info decl name
      else {
        def info(msg: => String) = if (settings.verbose.value) println(msg)
        info("*** missing: "+name+"/"+name.isTermName+"/"+owner+"/"+owner.hasPackageFlag+"/"+owner.info.decls.getClass)
        MissingRequirementError.notFound((if (path.isTermName) "object " else "class ")+path)
      }
    }
  }

  private def isJavaClass(path: String): Boolean =
    try {
      val classpath = platform.classPath.asURLs
      var classLoader = ScalaClassLoader.fromURLs(classpath)
      Class.forName(path, true, classLoader)
      true
    } catch {
      case (_: ClassNotFoundException) | (_: NoClassDefFoundError) | (_: IncompatibleClassChangeError) =>
      false
    }
}
