package scala.reflect
package runtime

import internal.Flags
import java.lang.{Class => jClass, Package => jPackage}
import scala.collection.mutable

private[reflect] trait SymbolLoaders { self: SymbolTable =>

  /** The standard completer for top-level classes
   *  @param clazz   The top-level class
   *  @param module  The companion object of `clazz`
   *  Calling `complete` on this type will assign the infos of `clazz` and `module`
   *  by unpickling information from the corresponding Java class. If no Java class
   *  is found, a package is created instead.
   */
  class TopClassCompleter(clazz: Symbol, module: Symbol) extends SymLoader with FlagAssigningCompleter {
//    def makePackage() {
//      println("wrong guess; making package "+clazz)
//      val ptpe = newPackageType(module.moduleClass)
//      for (sym <- List(clazz, module, module.moduleClass)) {
//        sym setFlag Flags.PACKAGE
//        sym setInfo ptpe
//      }
//    }

    override def complete(sym: Symbol) = {
      debugInfo("completing "+sym+"/"+clazz.fullName)
      assert(sym == clazz || sym == module || sym == module.moduleClass)
//      try {
      atPhaseNotLaterThan(picklerPhase) {
        val loadingMirror = mirrorThatLoaded(sym)
        val javaClass = loadingMirror.javaClass(clazz.javaClassName)
        loadingMirror.unpickleClass(clazz, module, javaClass)
//      } catch {
//        case ex: ClassNotFoundException => makePackage()
//        case ex: NoClassDefFoundError => makePackage()
          // Note: We catch NoClassDefFoundError because there are situations
          // where a package and a class have the same name except for capitalization.
          // It seems in this case the class is loaded even if capitalization differs
          // but then a NoClassDefFound error is issued with a ("wrong name: ...")
          // reason. (I guess this is a concession to Windows).
          // The present behavior is a bit too forgiving, in that it masks
          // all class load errors, not just wrong name errors. We should try
          // to be more discriminating. To get on the right track simply delete
          // the clause above and load a collection class such as collection.Iterable.
          // You'll see an error that class `parallel` has the wrong name.
//      }
      }
    }
    override def load(sym: Symbol) = complete(sym)
  }

  /** Create a class and a companion object, enter in enclosing scope,
   *  and initialize with a lazy type completer.
   *  @param owner   The owner of the newly created class and object
   *  @param name    The simple name of the newly created class
   *  @param completer  The completer to be used to set the info of the class and the module
   */
  protected def initAndEnterClassAndModule(owner: Symbol, name: TypeName, completer: (Symbol, Symbol) => LazyType) = {
    assert(!(name.toString endsWith "[]"), name)
    val clazz = owner.newClass(name)
    val module = owner.newModule(name.toTermName)
    // without this check test/files/run/t5256g and test/files/run/t5256h will crash
    // todo. reflection meeting verdict: need to enter the symbols into the first symbol in the owner chain that has a non-empty scope
    if (owner.info.decls != EmptyScope) {
      owner.info.decls enter clazz
      owner.info.decls enter module
    }
    initClassAndModule(clazz, module, completer(clazz, module))
    (clazz, module)
  }

  protected def setAllInfos(clazz: Symbol, module: Symbol, info: Type) = {
    List(clazz, module, module.moduleClass) foreach (_ setInfo info)
  }

  protected def initClassAndModule(clazz: Symbol, module: Symbol, completer: LazyType) =
    setAllInfos(clazz, module, completer)

  /** The type completer for packages.
   */
  class LazyPackageType extends LazyType with FlagAgnosticCompleter {
    override def complete(sym: Symbol) {
      assert(sym.isPackageClass)
      sym setInfo new ClassInfoType(List(), new PackageScope(sym), sym)
        // override def safeToString = pkgClass.toString
      openPackageModule(sym)
    }
  }

  /** Is the given name valid for a top-level class? We exclude names with embedded $-signs, because
   *  these are nested classes or anonymous classes,
   */
  def isInvalidClassName(name: Name) = {
    val dp = name pos '$'
    0 < dp && dp < (name.length - 1)
  }

  class PackageScope(pkgClass: Symbol) extends Scope(initFingerPrints = -1L) // disable fingerprinting as we do not know entries beforehand
      with SynchronizedScope {
    assert(pkgClass.isType)
    // disable fingerprinting as we do not know entries beforehand
    private val negatives = mutable.Set[Name]() // Syncnote: Performance only, so need not be protected.
    override def lookupEntry(name: Name): ScopeEntry = {
      val e = super.lookupEntry(name)
      if (e != null)
        e
      else if (isInvalidClassName(name) || (negatives contains name))
        null
      else {
        val path =
          if (pkgClass.isEmptyPackageClass) name.toString
          else pkgClass.fullName + "." + name
        val currentMirror = mirrorThatLoaded(pkgClass)
        currentMirror.tryJavaClass(path) match {
          case Some(cls) =>
            val loadingMirror = currentMirror.mirrorDefining(cls)
            val (clazz, module) =
              if (loadingMirror eq currentMirror) {
                initAndEnterClassAndModule(pkgClass, name.toTypeName, new TopClassCompleter(_, _))
              } else {
                val origOwner = loadingMirror.packageNameToScala(pkgClass.fullName)
                val clazz = origOwner.info decl name.toTypeName
                val module = origOwner.info decl name.toTermName
                assert(clazz != NoSymbol)
                assert(module != NoSymbol)
                pkgClass.info.decls enter clazz
                pkgClass.info.decls enter module
                (clazz, module)
              }
            debugInfo(s"created $module/${module.moduleClass} in $pkgClass")
            lookupEntry(name)
          case none =>
            debugInfo("*** not found : "+path)
            negatives += name
            null
        }
      }
    }
  }

  /** Assert that packages have package scopes */
  override def validateClassInfo(tp: ClassInfoType) {
    assert(!tp.typeSymbol.isPackageClass || tp.decls.isInstanceOf[PackageScope])
  }

  override def newPackageScope(pkgClass: Symbol) = new PackageScope(pkgClass)

  override def scopeTransform(owner: Symbol)(op: => Scope): Scope =
    if (owner.isPackageClass) owner.info.decls else op
}
