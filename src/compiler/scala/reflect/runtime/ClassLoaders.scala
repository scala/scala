package scala.reflect
package runtime

trait ClassLoaders extends internal.SymbolTable { self: SymbolTable =>

  def staticClass(fullname: String) =
    definitions.getRequiredClass(fullname)

  def staticModule(fullname: String) =
    definitions.getRequiredModule(fullname)

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
}
