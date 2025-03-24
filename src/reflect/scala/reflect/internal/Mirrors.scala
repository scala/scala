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
package internal

import Flags._

trait Mirrors extends api.Mirrors {
  thisUniverse: SymbolTable =>

  override type Mirror >: Null <: RootsBase

  // root symbols hold a strong reference to the enclosing mirror
  // this prevents the mirror from being collected
  // if there are any symbols created by that mirror
  trait RootSymbol extends Symbol { def mirror: Mirror }

  abstract class RootsBase(rootOwner: Symbol) extends scala.reflect.api.Mirror[Mirrors.this.type] { thisMirror =>
    private[this] var initialized = false
    def isMirrorInitialized = initialized

    protected[scala] def rootLoader: LazyType

    val RootClass: ClassSymbol
    val RootPackage: ModuleSymbol
    val EmptyPackageClass: ClassSymbol
    val EmptyPackage: ModuleSymbol

    def symbolOf[T: universe.WeakTypeTag]: universe.TypeSymbol = universe.weakTypeTag[T].in(this).tpe.typeSymbolDirect.asType

    def findMemberFromRoot(fullName: Name): Symbol = {
      val segs = nme.segments(fullName.toString, fullName.isTermName)
      if (segs.isEmpty) NoSymbol
      else definitions.findNamedMember(segs.tail, RootClass.info member segs.head)
    }

    /** Todo: organize similar to mkStatic in scala.reflect.Base */
    @annotation.unused
    private def getModuleOrClass(path: Name, len: Int): Symbol =
      getModuleOrClass(path.toString, len, path.newName(_))

    private def getModuleOrClass(path: String, len: Int, toName: String => Name): Symbol = {
      val point  = path.lastIndexOf('.', len - 1)
      val owner  = if (point > 0) getModuleOrClass(path, point, newTermName(_)) else RootClass
      val name   = toName(path.substring(point + 1, len))
      val sym    = owner.info.member(name)
      val result = if (name.isTermName) sym.suchThat(_.hasFlag(MODULE)) else sym
      if (result != NoSymbol) result
      else {
        if (settings.isDebug) { log(sym.info); log(sym.info.members) }//debug
        thisMirror.missingHook(owner, name) orElse {
          MissingRequirementError.notFound(s"${if (name.isTermName) "object" else "class"} $path in $thisMirror")
        }
      }
    }

    /** If you're looking for a class, pass a type name.
     *  If a module, a term name.
     *
     *  Unlike `staticModuleOrClass`, this function
     *  loads unqualified names from the root package.
     */
    private def getModuleOrClass(path: String, toName: String => Name): Symbol =
      getModuleOrClass(path, path.length, toName)

    /** If you're looking for a class, pass a type name.
     *  If a module, a term name.
     *
     *  Unlike `getModuleOrClass`, this function
     *  loads unqualified names from the empty package.
     */
    private def staticModuleOrClass(path: String, toName: String => Name): Symbol = {
      val isPackageless = !path.contains('.')
      if (isPackageless) EmptyPackageClass.info decl toName(path)
      else getModuleOrClass(path, toName)
    }

    protected def mirrorMissingHook(owner: Symbol, name: Name): Symbol = NoSymbol

    protected def universeMissingHook(owner: Symbol, name: Name): Symbol = thisUniverse.missingHook(owner, name)

    private[scala] def missingHook(owner: Symbol, name: Name): Symbol = logResult(s"missingHook($owner, $name)")(
      mirrorMissingHook(owner, name) orElse universeMissingHook(owner, name)
    )

    // todo: get rid of most the methods here and keep just staticClass/Module/Package

    /************************ loaders of class symbols ************************/

    private def ensureClassSymbol(fullname: String, sym: Symbol): ClassSymbol =
      sym match {
        case x: ClassSymbol => x
        case _              => MissingRequirementError.notFound("class " + fullname)
      }

    @deprecated("Use overload that accepts a String.", "2.13.0")
    def getClassByName(fullname: Name): ClassSymbol =
      ensureClassSymbol(fullname.toString, getModuleOrClass(fullname.toString, fullname.length, newTypeName(_)))

    def getClassByName(fullname: String): ClassSymbol =
      getRequiredClass(fullname)

    // TODO_NAMES
    def getRequiredClass(fullname: String, toName: String => Name): ClassSymbol =
      ensureClassSymbol(fullname, getModuleOrClass(fullname, fullname.length, toName))

    def getRequiredClass(fullname: String): ClassSymbol =
      ensureClassSymbol(fullname, getModuleOrClass(fullname, fullname.length, newTypeName(_)))

    def requiredClass[T: ClassTag] : ClassSymbol =
      getRequiredClass(erasureName[T], newTypeName(_))

    def getClassIfDefined(fullname: String): Symbol =
      getClassIfDefined(fullname, newTypeName(_))

    @deprecated("Use overload that accepts a String.", "2.13.0")
    def getClassIfDefined(fullname: Name): Symbol =
      wrapMissing(getClassByName(fullname.toTypeName))

    // TODO_NAMES
    def getClassIfDefined(fullname: String, toName: String => Name): Symbol =
      wrapMissing(getRequiredClass(fullname, toName))

    /** @inheritdoc
     *
     *  Unlike getClassByName/getRequiredClass this function can also load packageless symbols.
     *  Compiler might ignore them, but they should be loadable with macros.
     */
    override def staticClass(fullname: String): ClassSymbol =
      try ensureClassSymbol(fullname, staticModuleOrClass(fullname, newTypeName(_)))
      catch { case mre: MissingRequirementError => throw new ScalaReflectionException(mre.msg) }

    /************************ loaders of module symbols ************************/

    private def ensureModuleSymbol(fullname: String, sym: Symbol, allowPackages: Boolean): ModuleSymbol =
      sym match {
        case x: ModuleSymbol if allowPackages || !x.hasPackageFlag => x
        case _                                                     => MissingRequirementError.notFound(s"object $fullname")
      }

    @deprecated("Use overload that accepts a String.", "2.13.0")
    def getModuleByName(fullname: Name): ModuleSymbol =
      getModuleByName(fullname.toString)

    def getModuleByName(fullname: String): ModuleSymbol =
      ensureModuleSymbol(fullname, getModuleOrClass(fullname, fullname.length, newTermName(_)), allowPackages = true)

    def getRequiredModule(fullname: String): ModuleSymbol =
      getModuleByName(fullname)

    // TODO: What syntax do we think should work here? Say you have an object
    // like scala.Predef.  You can't say requiredModule[scala.Predef] since there's
    // no accompanying Predef class, and if you say requiredModule[scala.Predef.type]
    // the name found via the erasure is scala.Predef$.  For now I am
    // removing the trailing $, but I think that classTag should have
    // a method which returns a usable name, one which doesn't expose this
    // detail of the backend.
    def requiredModule[T: ClassTag] : ModuleSymbol =
      getRequiredModule(erasureName[T] stripSuffix "$")

    def getModuleIfDefined(fullname: String): Symbol =
      wrapMissing(getModuleByName(fullname))

    @deprecated("Use overload that accepts a String.", "2.13.0")
    def getModuleIfDefined(fullname: Name): Symbol =
      getModuleIfDefined(fullname.toString)

    /** @inheritdoc
     *
     *  Unlike getModule/getRequiredModule this function can also load packageless symbols.
     *  Compiler might ignore them, but they should be loadable with macros.
     */
    override def staticModule(fullname: String): ModuleSymbol =
      try ensureModuleSymbol(fullname, staticModuleOrClass(fullname, newTermName(_)), allowPackages = false)
      catch { case mre: MissingRequirementError => throw new ScalaReflectionException(mre.msg) }

    /************************ loaders of package symbols ************************/

    private def ensurePackageSymbol(fullname: String, sym: Symbol, allowModules: Boolean): ModuleSymbol =
      sym match {
        case x: ModuleSymbol if allowModules || x.hasPackageFlag => x
        case _                                                   => MissingRequirementError.notFound("package " + fullname)
      }

    @deprecated("use overload that accepts a String.", since = "2.13.0")
    def getPackage(fullname: TermName): ModuleSymbol =
      getPackage(fullname.toString)
    def getPackage(fullname: String): ModuleSymbol =
      ensurePackageSymbol(fullname, getModuleOrClass(fullname, newTermName(_)), allowModules = true)

    @deprecated("use overload that accepts a String.", since = "2.12.11")
    def getPackageIfDefined(fullname: TermName): Symbol =
      getPackageIfDefined(fullname.toString)
    def getPackageIfDefined(fullname: String): Symbol =
      wrapMissing(getPackage(fullname))

    @deprecated("use getPackage", "2.11.0")
    def getRequiredPackage(fullname: String): ModuleSymbol =
      getPackage(fullname)

    @deprecated("use overload that accepts a String.", since = "2.12.11")
    def getPackageObject(fullname: TermName): ModuleSymbol =
      getPackageObject(fullname.toString)
    def getPackageObject(fullname: String): ModuleSymbol =
      getPackage(fullname).packageObject match {
        case x: ModuleSymbol => x
        case _               => MissingRequirementError.notFound("package object " + fullname)
      }

    @deprecated("use overload that accepts a String.", since = "2.12.11")
    def getPackageObjectIfDefined(fullname: TermName): Symbol =
      getPackageObjectIfDefined(fullname.toString)
    def getPackageObjectIfDefined(fullname: String): Symbol =
      wrapMissing(getPackageObject(fullname))

    override def staticPackage(fullname: String): ModuleSymbol =
      try ensurePackageSymbol(fullname.toString, getModuleOrClass(fullname, fullname.length, newTermName(_)), allowModules = false)
      catch { case mre: MissingRequirementError => throw new ScalaReflectionException(mre.msg) }

    /************************ helpers ************************/

    def erasureName[T: ClassTag] : String = {
      /* We'd like the String representation to be a valid
       * scala type, so we have to decode the jvm's secret language.
       */
      def erasureString(clazz: Class[_]): String = {
        if (clazz.isArray) "Array[" + erasureString(clazz.getComponentType) + "]"
        else clazz.getName
      }
      erasureString(classTag[T].runtimeClass)
    }

   @inline final def wrapMissing(body: => Symbol): Symbol =
      try body
      catch { case _: MissingRequirementError => NoSymbol }

    def init(): Unit = if (!initialized) {
      // Still fiddling with whether it's cleaner to do some of this setup here
      // or from constructors.  The latter approach tends to invite init order issues.

      EmptyPackageClass setInfo rootLoader
      EmptyPackage setInfo EmptyPackageClass.tpe

      connectModuleToClass(EmptyPackage, EmptyPackageClass)
      connectModuleToClass(RootPackage, RootClass)

      RootClass.info.decls enter EmptyPackage
      RootClass.info.decls enter RootPackage

      if (rootOwner != NoSymbol) {
        // synthetic core classes are only present in root mirrors
        // because Definitions.scala, which initializes and enters them, only affects rootMirror
        // therefore we need to enter them manually for non-root mirrors
        definitions.syntheticCoreClasses foreach (theirSym => {
          val theirOwner = theirSym.owner
          assert(theirOwner.isPackageClass, s"theirSym = $theirSym, theirOwner = $theirOwner")
          val ourOwner = staticPackage(theirOwner.fullName).moduleClass
          val ourSym = theirSym // just copy the symbol into our branch of the symbol table
          ourOwner.info.decls enterIfNew ourSym
        })
      }

      initialized = true
    }
  }

  abstract class Roots(rootOwner: Symbol) extends RootsBase(rootOwner) { thisMirror =>

    // TODO - having these as objects means they elude the attempt to
    // add synchronization in SynchronizedSymbols.  But we should either
    // flip on object overrides or find some other accommodation, because
    // lazy vals are unnecessarily expensive relative to objects and it
    // is very beneficial for a handful of bootstrap symbols to have
    // first class identities
    sealed trait WellKnownSymbol extends Symbol {
      this initFlags (PackageFlags | STATIC)
    }
    // Features common to RootClass and RootPackage, the roots of all
    // type and term symbols respectively.
    sealed trait RootSymbol extends WellKnownSymbol with thisUniverse.RootSymbol {
      final override def isRootSymbol = true
      override def owner              = rootOwner
      override def typeOfThis         = thisSym.tpe
      def mirror                      = thisMirror.asInstanceOf[Mirror]
    }

    class RootPackage extends ModuleSymbol(rootOwner, NoPosition, nme.ROOTPKG) with RootSymbol {
      this setInfo NullaryMethodType(RootClass.tpe)

      override def isRootPackage = true
    }

    // This is the package _root_.  The actual root cannot be referenced at
    // the source level, but _root_ is essentially a function => <root>.
    lazy val RootPackage = new RootPackage

    class RootClass extends PackageClassSymbol(rootOwner, NoPosition, tpnme.ROOT) with RootSymbol {
      this setInfo rootLoader

      override def isRoot            = true
      override def isEffectiveRoot   = true
      override def isNestedClass     = false
      override def sourceModule      = RootPackage
    }

    // This is <root>, the actual root of everything except the package _root_.
    // <root> and _root_ (RootPackage and RootClass) should be the only "well known"
    // symbols owned by NoSymbol.  All owner chains should go through RootClass,
    // although it is probable that some symbols are created as direct children
    // of NoSymbol to ensure they will not be stumbled upon.  (We should designate
    // a better encapsulated place for that.)
    lazy val RootClass = new RootClass

    class EmptyPackage extends ModuleSymbol(RootClass, NoPosition, nme.EMPTY_PACKAGE_NAME) with WellKnownSymbol {
      override def isEmptyPackage = true
    }

    // The empty package, which holds all top level types without given packages.
    lazy val EmptyPackage = new EmptyPackage

    class EmptyPackageClass extends PackageClassSymbol(RootClass, NoPosition, tpnme.EMPTY_PACKAGE_NAME) with WellKnownSymbol {
      override def isEffectiveRoot     = true
      override def isEmptyPackageClass = true
      override def sourceModule        = EmptyPackage
    }

    lazy val EmptyPackageClass = new EmptyPackageClass
  }
}
