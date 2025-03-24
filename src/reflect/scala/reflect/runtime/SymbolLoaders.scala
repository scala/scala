/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package runtime

import scala.collection.mutable
import scala.reflect.internal.Flags._

private[reflect] trait SymbolLoaders { self: SymbolTable =>

  /** The standard completer for top-level classes
   *  @param clazz   The top-level class
   *  @param module  The companion object of `clazz`
   *  Calling `complete` on this type will assign the infos of `clazz` and `module`
   *  by unpickling information from the corresponding Java class. If no Java class
   *  is found, a package is created instead.
   */
  class TopClassCompleter(clazz: ClassSymbol, module: ModuleSymbol) extends SymLoader with FlagAssigningCompleter {
    markFlagsCompleted(clazz, module)(mask = ~TopLevelPickledFlags)
    override def complete(sym: Symbol) = {
      debugInfo("completing "+sym+"/"+clazz.fullName)
      assert(sym == clazz || sym == module || sym == module.moduleClass, "Must be class or module")
      slowButSafeEnteringPhaseNotLaterThan(picklerPhase) {
        val loadingMirror = mirrorThatLoaded(sym)
        val javaClass = loadingMirror.javaClass(clazz.javaClassName)
        loadingMirror.unpickleClass(clazz, module, javaClass)
        // NOTE: can't mark as thread-safe here, because unpickleClass might decide to delegate to FromJavaClassCompleter
        // if (!isCompilerUniverse) markAllCompleted(clazz, module)
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
  protected def initAndEnterClassAndModule(owner: Symbol, name: TypeName, completer: (ClassSymbol, ModuleSymbol) => LazyType) = {
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
    override def complete(sym: Symbol): Unit = {
      assert(sym.isPackageClass, "Must be package")
      // Time travel to a phase before refchecks avoids an initialization issue. `openPackageModule`
      // creates a module symbol and invokes `companionModule` while the `infos` field is
      // still null. This calls `isModuleNotMethod`, which forces the `info` if run after refchecks.
      slowButSafeEnteringPhaseNotLaterThan(picklerPhase) {
        sym setInfo new ClassInfoType(List(), new PackageScope(sym), sym)
        // override def safeToString = pkgClass.toString
        openPackageModule(sym)
        markAllCompleted(sym)
      }
    }
  }


  // Since runtime reflection doesn't have a luxury of enumerating all classes
  // on the classpath, it has to materialize symbols for top-level definitions
  // (packages, classes, objects) on demand.
  //
  // Someone asks us for a class named `foo.Bar`? Easy. Let's speculatively create
  // a package named `foo` and then look up `newTypeName("bar")` in its decls.
  // This lookup, implemented in `SymbolLoaders.PackageScope` tests the waters by
  // trying to to `Class.forName("foo.Bar")` and then creates a ClassSymbol upon
  // success (the whole story is a bit longer, but the rest is irrelevant here).
  //
  // That's all neat, but these non-deterministic mutations of the global symbol
  // table give a lot of trouble in multi-threaded setting. One of the popular
  // reflection crashes happens when multiple threads happen to trigger symbol
  // materialization multiple times for the same symbol, making subsequent
  // reflective operations stumble upon outrageous stuff like overloaded packages.
  //
  // Short of significantly changing SymbolLoaders I see no other way than just
  // to slap a global lock on materialization in runtime reflection.
  class PackageScope(pkgClass: Symbol) extends Scope
      with SynchronizedScope {
    assert(pkgClass.isType, "Must be type")

    // materializing multiple copies of the same symbol in PackageScope is a very popular bug
    // this override does its best to guard against it
    override def enter[T <: Symbol](sym: T): sym.type = {
      // workaround for scala/bug#7728
      if (isCompilerUniverse) super.enter(sym)
      else {
        val existing = super.lookupEntry(sym.name)
        def eitherIsMethod(sym1: Symbol, sym2: Symbol) = sym1.isMethod || sym2.isMethod
        assert(existing == null || eitherIsMethod(existing.sym, sym), s"pkgClass = $pkgClass, sym = $sym, existing = $existing")
        super.enter(sym)
      }
    }

    override def enterIfNew[T <: Symbol](sym: T): T = {
      val existing = super.lookupEntry(sym.name)
      if (existing == null) enter(sym)
      else existing.sym.asInstanceOf[T]
    }

    // package scopes need to synchronize on the GIL
    // because lookupEntry might cause changes to the global symbol table
    override def syncLockSynchronized[T](body: => T): T = gilSynchronized(body)
    private[this] val negatives = new mutable.HashSet[Name]
    override def lookupEntry(name: Name): ScopeEntry = syncLockSynchronized {
      val e = super.lookupEntry(name)
      if (e != null)
        e
      else if (negatives contains name)
        null
      else {
        val path =
          if (pkgClass.isEmptyPackageClass) name.toString
          else pkgClass.fullName + "." + name
        val currentMirror = mirrorThatLoaded(pkgClass)
        currentMirror.tryJavaClass(path) match {
          case Some(cls) =>
            val loadingMirror = currentMirror.mirrorDefining(cls)
            val (_, module) =
              if (loadingMirror eq currentMirror) {
                initAndEnterClassAndModule(pkgClass, name.toTypeName, new TopClassCompleter(_, _))
              } else {
                val origOwner = loadingMirror.packageNameToScala(pkgClass.fullName)
                val clazz = origOwner.info decl name.toTypeName
                val module = origOwner.info decl name.toTermName
                assert(clazz != NoSymbol, "Missing class symbol")
                assert(module != NoSymbol, "Missing module symbol")
                // currentMirror.mirrorDefining(cls) might side effect by entering symbols into pkgClass.info.decls
                // therefore, even though in the beginning of this method, super.lookupEntry(name) returned null
                // entering clazz/module now will result in a double-enter assertion in PackageScope.enter
                // here's how it might happen
                // 1) we are the rootMirror
                // 2) cls.getClassLoader is different from our classloader
                // 3) mirrorDefining(cls) looks up a mirror corresponding to that classloader and cannot find it
                // 4) mirrorDefining creates a new mirror
                // 5) that triggers Mirror.init() of the new mirror
                // 6) that triggers definitions.syntheticCoreClasses
                // 7) that might materialize symbols and enter them into our scope (because syntheticCoreClasses live in rootMirror)
                // 8) now we come back here and try to enter one of the now entered symbols => BAM!
                // therefore we use enterIfNew rather than just enter
                enterIfNew(clazz)
                enterIfNew(module)
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
  override def validateClassInfo(tp: ClassInfoType): Unit =
    assert(!tp.typeSymbol.isPackageClass || tp.decls.isInstanceOf[PackageScope], "Package must have package scope")

  override def newPackageScope(pkgClass: Symbol) = new PackageScope(pkgClass)

  override def scopeTransform(owner: Symbol)(op: => Scope): Scope =
    if (owner.isPackageClass) owner.info.decls else op
}
