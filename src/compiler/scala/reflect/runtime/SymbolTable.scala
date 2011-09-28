package scala.reflect
package runtime

/**
 *  This symbol table trait fills in the definitions so that class information is obtained by refection.
 *  It can be used either from the reflexive mirror itself (class Universe), or else from
 *  a runtime compiler that uses reflection to get a class information (class scala.tools.nsc.ReflectGlobal)
 */
trait SymbolTable extends internal.SymbolTable with JavaToScala with ScalaToJava with Loaders {

  /** If `owner` is a package class (but not the empty package) and `name` is a term name, make a new package
   *  <owner>.<name>, otherwise return NoSymbol.
   *  Exception: If owner is root and a java class with given name exists, create symbol in empty package instead.
   */
  override def missingHook(owner: Symbol, name: Name): Symbol =
    if (owner.isRoot && isJavaClass(name.toString))
      definitions.EmptyPackageClass.info decl name
    else if (name.isTermName && owner.hasPackageFlag && !owner.isEmptyPackageClass)
      makeScalaPackage(if (owner.isRoot) name.toString else owner.fullName+"."+name).sourceModule
    else {
      info("*** missing: "+name+"/"+name.isTermName+"/"+owner+"/"+owner.hasPackageFlag+"/"+owner.info.decls.getClass)
      super.missingHook(owner, name)
    }

  /** Assert that packages have package scopes */
  override def validateClassInfo(tp: ClassInfoType) {
    assert(!tp.typeSymbol.isPackageClass || tp.decls.isInstanceOf[PackageScope])
  }

  def info(msg: => String) =
    if (settings.verbose.value) println("[reflect-compiler] "+msg)

  def debugInfo(msg: => String) =
    if (settings.debug.value) info(msg)
}
