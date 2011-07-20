package scala.reflect
package runtime

import java.lang.{Class => jClass, Package => jPackage}

trait Loaders { self: Universe =>

  /** The lazy type for root.
   */
  override val rootLoader = new LazyType {
    override def complete(sym: Symbol) = sym setInfo new PackageType(definitions.RootClass)
  }

  /** The standard completer for top-level classes
   *  @param clazz   The top-level class
   *  @param module  The companion object of `clazz`
   *  Calling `complete` on this type will assign the infos of `clazz` and `module`
   *  by unpickling information from the corresponding Java class. If no Java class
   *  is found, a package is created instead.
   */
  class TopClassCompleter(clazz: Symbol, module: Symbol) extends LazyType {
    def makePackage() {
      val ptpe = new PackageType(module.moduleClass)
      clazz setInfo ptpe
      module setInfo ptpe
      module.moduleClass setInfo ptpe
    }
    override def complete(sym: Symbol) = {
      println("completing "+sym+"/"+clazz.fullName)
      assert(sym == clazz || sym == module || sym == module.moduleClass)
      try {
        unpickleClass(clazz, module, jClass.forName(clazz.fullName))
      } catch {
        case ex: ClassNotFoundException => makePackage()
        case ex: NoClassDefFoundError => makePackage()
          // Note: We catch NoClassDefFoundError because there are situations
          // where a package and a class have the same name except for capitalization.
          // It seems in this case the class is loaded even if capitalization differs
          // but then a NoClassDefFound error is issued with a ("wrong name: ...")
          // reason. (I guess this is a concession to Windows).
          // The present behavior is a bit too forgiving, in that it masks
          // all class loade errors, not just wrong name errors. We should try
          // to be more discriminating. To get on the right track simply delete
          // the clause above and load a collection class such as collection.Iterable.
          // You'll see an error that class `parallel` has the wrong name.
      }
    }
  }

  /** Create a class and a companion object, enter in enclosing scope,
   *  and initialize with a lazy type completer.
   *  @param owner   The owner of the newly created class and object
   *  @param name    The simple name of the newly created class
   *  @param completer  The completer to be used to set the info of the class and the module
   */
  protected def createClassModule(owner: Symbol, name: TypeName, completer: (Symbol, Symbol) => LazyType) = {
    val clazz = owner.newClass(NoPosition, name)
    val module = owner.newModule(NoPosition, name.toTermName)
    owner.info.decls enter clazz
    owner.info.decls enter module
    initClassModule(clazz, module, completer(clazz, module))
    (clazz, module)
  }

  protected def initClassModule(clazz: Symbol, module: Symbol, completer: LazyType) = {
    clazz.setInfo(completer)
    module.setInfo(completer)
    module.moduleClass.setInfo(completer)
  }

  /** The type for packages.
   *  Since we cannot search the file system for classes in directories to populate
   *  a package, we do the following instead: For every member that is looked up in
   *  the package, create a class and companion object optimistically, giving it
   *  a TopClassCompleter type. When any of the two symbols is forced via info,
   *  the TopClassCompleter will sort things out.
   */
  class PackageType(pkg: Symbol) extends ClassInfoType(List(), newScope, pkg) {
    override def decl(name: Name): Symbol =
      (decls lookup name) orElse {
        assert(this eq pkg.info, this+" "+pkg.info)
        assert(decls eq pkg.info.decls)
        println("creating "+name+" in "+pkg)
        val (clazz, module) = createClassModule(pkg, name.toTypeName, new TopClassCompleter(_, _))
        if (name.isTypeName) clazz else module
      }
    override def member(name: Name): Symbol = decl(name)
    override def findMember(name: Name, excludedFlags: Long, requiredFlags: Long, stableOnly: Boolean) =
      member(name).filter (m => m.hasAllFlags(requiredFlags) && !m.hasFlag(excludedFlags))
  }
}