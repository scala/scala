/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import Flags._

trait Mirrors extends api.Mirrors {
  self: SymbolTable =>

  override type Mirror >: Null <: RootsBase

  abstract class RootsBase(rootOwner: Symbol) extends MirrorOf[Mirrors.this.type] { thisMirror =>

    protected[scala] def rootLoader: LazyType

    val RootClass: ClassSymbol
    val RootPackage: ModuleSymbol
    val EmptyPackageClass: ClassSymbol
    val EmptyPackage: ModuleSymbol

    def findMemberFromRoot(fullName: Name): Symbol = {
      val segs = nme.segments(fullName.toString, fullName.isTermName)
      if (segs.isEmpty) NoSymbol
      else definitions.findNamedMember(segs.tail, RootClass.info member segs.head)
    }

    /** If you're looking for a class, pass a type name.
     *  If a module, a term name.
     *
     *  `tryPackageless` tells this function to search for requested a requested symbol in EmptyPackageClass as well.
     *  Compiler might ignore them, but they should be loadable with macros if the programmer wishes.
     *  More info here: http://groups.google.com/group/scala-internals/browse_thread/thread/5146021fd7c0cec
     */
    private def getModuleOrClass(path: Name, tryPackageless: Boolean): Symbol = getModuleOrClass(path, path.length, tryPackageless)

    /** Todo: organize similar to mkStatic in reflect.Base */
    private def getModuleOrClass(path: Name, len: Int, tryPackageless: Boolean): Symbol = {
      val point = path lastPos('.', len - 1)
      val owner =
        if (point > 0) getModuleOrClass(path.toTermName, point, tryPackageless)
        else RootClass
      val name = path subName (point + 1, len)
      var sym = owner.info member name
      if (sym == NoSymbol && owner == RootClass && tryPackageless) sym = EmptyPackageClass.info decl name
      val result = if (path.isTermName) sym.suchThat(_ hasFlag MODULE) else sym
      if (result != NoSymbol) result
      else {
        if (settings.debug.value) { log(sym.info); log(sym.info.members) }//debug
        thisMirror.missingHook(owner, name) orElse {
          MissingRequirementError.notFound((if (path.isTermName) "object " else "class ")+path+" in "+thisMirror)
        }
      }
    }

    protected def mirrorMissingHook(owner: Symbol, name: Name): Symbol = NoSymbol

    protected def universeMissingHook(owner: Symbol, name: Name): Symbol = self.missingHook(owner, name)

    private[scala] def missingHook(owner: Symbol, name: Name): Symbol = mirrorMissingHook(owner, name) orElse universeMissingHook(owner, name)

    // todo: get rid of most the methods here and keep just staticClass/Module/Package

    /************************ loaders of class symbols ************************/

    private def getClassImpl(fullname: TypeName, tryPackageless: Boolean): ClassSymbol = {
      var result = getModuleOrClass(fullname.toTypeName, tryPackageless)
      while (result.isAliasType) result = result.info.typeSymbol
      result match {
        case x: ClassSymbol => x
        case _              => MissingRequirementError.notFound("class " + fullname)
      }
    }

    @deprecated("Use getClassByName", "2.10.0")
    def getClass(fullname: Name): ClassSymbol =
      getClassByName(fullname)

    def getClassByName(fullname: Name): ClassSymbol =
      getClassImpl(fullname.toTypeName, tryPackageless = false)

    def getRequiredClass(fullname: String): ClassSymbol =
      getClassByName(newTypeNameCached(fullname))

    def requiredClass[T: ClassTag] : ClassSymbol =
      getRequiredClass(erasureName[T])

    def getClassIfDefined(fullname: String): Symbol =
      getClassIfDefined(newTypeName(fullname))

    def getClassIfDefined(fullname: Name): Symbol =
      wrapMissing(getClassByName(fullname.toTypeName))

    /** Unlike getClassByName/getRequiredClass this function can also load packageless symbols.
     *  Compiler might ignore them, but they should be loadable with macros.
     */
    override def staticClass(fullname: String): ClassSymbol =
      getClassImpl(newTypeNameCached(fullname), tryPackageless = true)

    /************************ loaders of module symbols ************************/

    private def getModuleImpl(fullname: TermName, tryPackageless: Boolean): ModuleSymbol = {
      var result = getModuleOrClass(fullname, tryPackageless)
      result match {
        case x: ModuleSymbol => x
        case _               => MissingRequirementError.notFound("object " + fullname)
      }
    }

    @deprecated("Use getModuleByName", "2.10.0")
    def getModule(fullname: Name): ModuleSymbol =
      getModuleByName(fullname)

    def getModuleByName(fullname: Name): ModuleSymbol =
      getModuleImpl(fullname.toTermName, tryPackageless = false)

    def getRequiredModule(fullname: String): ModuleSymbol =
      getModule(newTermNameCached(fullname))

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
      getModuleIfDefined(newTermName(fullname))

    def getModuleIfDefined(fullname: Name): Symbol =
      wrapMissing(getModule(fullname.toTermName))

    /** Unlike getModule/getRequiredModule this function can also load packageless symbols.
     *  Compiler might ignore them, but they should be loadable with macros.
     */
    override def staticModule(fullname: String): ModuleSymbol =
      getModuleImpl(newTermNameCached(fullname), tryPackageless = true)

    /************************ loaders of package symbols ************************/

    def getPackage(fullname: Name): ModuleSymbol = getModule(fullname)

    def getRequiredPackage(fullname: String): ModuleSymbol =
      getPackage(newTermNameCached(fullname))

    def getPackageObject(fullname: String): ModuleSymbol =
      (getModule(newTermName(fullname)).info member nme.PACKAGE) match {
        case x: ModuleSymbol => x
        case _               => MissingRequirementError.notFound("package object " + fullname)
      }

    def getPackageObjectIfDefined(fullname: String): Symbol = {
      val module = getModuleIfDefined(newTermName(fullname))
      if (module == NoSymbol) NoSymbol
      else {
        val packageObject = module.info member nme.PACKAGE
        packageObject match {
          case x: ModuleSymbol => x
          case _               => NoSymbol
        }
      }
    }

    /************************ helpers ************************/

    def erasureName[T: ClassTag] : String = {
      /** We'd like the String representation to be a valid
       *  scala type, so we have to decode the jvm's secret language.
       */
      def erasureString(clazz: Class[_]): String = {
        if (clazz.isArray) "Array[" + erasureString(clazz.getComponentType) + "]"
        else clazz.getName
      }
      erasureString(classTag[T].runtimeClass)
    }

   @inline private def wrapMissing(body: => Symbol): Symbol =
      try body
      catch { case _: MissingRequirementError => NoSymbol }

    /** getModule2/getClass2 aren't needed at present but may be again,
     *  so for now they're mothballed.
     */
    // def getModule2(name1: Name, name2: Name) = {
    //   try getModuleOrClass(name1.toTermName)
    //   catch { case ex1: FatalError =>
    //     try getModuleOrClass(name2.toTermName)
    //     catch { case ex2: FatalError => throw ex1 }
    //   }
    // }
    // def getClass2(name1: Name, name2: Name) = {
    //   try {
    //     val result = getModuleOrClass(name1.toTypeName)
    //     if (result.isAliasType) getClass(name2) else result
    //   }
    //   catch { case ex1: FatalError =>
    //     try getModuleOrClass(name2.toTypeName)
    //     catch { case ex2: FatalError => throw ex1 }
    //   }
    // }

    def init() {
      // Still fiddling with whether it's cleaner to do some of this setup here
      // or from constructors.  The latter approach tends to invite init order issues.

      EmptyPackageClass setInfo rootLoader
      EmptyPackage setInfo EmptyPackageClass.tpe

      connectModuleToClass(EmptyPackage, EmptyPackageClass)
      connectModuleToClass(RootPackage, RootClass)

      RootClass.info.decls enter EmptyPackage
      RootClass.info.decls enter RootPackage
    }
  }

  abstract class Roots(rootOwner: Symbol) extends RootsBase(rootOwner) { thisMirror =>

    // TODO - having these as objects means they elude the attempt to
    // add synchronization in SynchronizedSymbols.  But we should either
    // flip on object overrides or find some other accomodation, because
    // lazy vals are unnecessarily expensive relative to objects and it
    // is very beneficial for a handful of bootstrap symbols to have
    // first class identities
    sealed trait WellKnownSymbol extends Symbol {
      this initFlags TopLevelCreationFlags
    }
    // Features common to RootClass and RootPackage, the roots of all
    // type and term symbols respectively.
    sealed trait RootSymbol extends WellKnownSymbol {
      final override def isRootSymbol = true
      override def owner              = rootOwner
      override def typeOfThis         = thisSym.tpe
    }

    // This is the package _root_.  The actual root cannot be referenced at
    // the source level, but _root_ is essentially a function => <root>.
    final object RootPackage extends ModuleSymbol(rootOwner, NoPosition, nme.ROOTPKG) with RootSymbol {
      this setInfo NullaryMethodType(RootClass.tpe)
      RootClass.sourceModule = this

      override def isRootPackage = true
    }
    // This is <root>, the actual root of everything except the package _root_.
    // <root> and _root_ (RootPackage and RootClass) should be the only "well known"
    // symbols owned by NoSymbol.  All owner chains should go through RootClass,
    // although it is probable that some symbols are created as direct children
    // of NoSymbol to ensure they will not be stumbled upon.  (We should designate
    // a better encapsulated place for that.)
    final object RootClass extends PackageClassSymbol(rootOwner, NoPosition, tpnme.ROOT) with RootSymbol {
      this setInfo rootLoader

      override def isRoot            = true
      override def isEffectiveRoot   = true
      override def isStatic          = true
      override def isNestedClass     = false
    }
    // The empty package, which holds all top level types without given packages.
    final object EmptyPackage extends ModuleSymbol(RootClass, NoPosition, nme.EMPTY_PACKAGE_NAME) with WellKnownSymbol {
      override def isEmptyPackage = true
    }
    final object EmptyPackageClass extends PackageClassSymbol(RootClass, NoPosition, tpnme.EMPTY_PACKAGE_NAME) with WellKnownSymbol {
      override def isEffectiveRoot     = true
      override def isEmptyPackageClass = true
    }
  }
}
