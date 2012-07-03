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

    /** Todo: organize similar to mkStatic in reflect.Base */
    private def getModuleOrClass(path: Name, len: Int): Symbol = {
      val point = path lastPos('.', len - 1)
      val owner =
        if (point > 0) getModuleOrClass(path.toTermName, point)
        else RootClass
      val name = path subName (point + 1, len)
      val sym = owner.info member name
      val result = if (path.isTermName) sym.suchThat(_ hasFlag MODULE) else sym
      if (result != NoSymbol) result
      else {
        if (settings.debug.value) { log(sym.info); log(sym.info.members) }//debug
        tryMissingHooks(owner, name) orElse {
          MissingRequirementError.notFound((if (path.isTermName) "object " else "class ")+path+" in "+thisMirror)
        }
      }
    }

    protected def mirrorMissingHook(owner: Symbol, name: Name): Symbol = NoSymbol

    protected def symbolTableMissingHook(owner: Symbol, name: Name): Symbol = self.missingHook(owner, name)

    private[reflect] def tryMissingHooks(owner: Symbol, name: Name): Symbol = mirrorMissingHook(owner, name) orElse symbolTableMissingHook(owner, name)

    /** If you're looking for a class, pass a type name.
     *  If a module, a term name.
     */
    private def getModuleOrClass(path: Name): Symbol = getModuleOrClass(path, path.length)

    override def staticClass(fullName: String): ClassSymbol = getRequiredClass(fullName)

    // todo: get rid of most creation methods and keep just staticClass/Module/Package

    def getClassByName(fullname: Name): ClassSymbol = {
      var result = getModuleOrClass(fullname.toTypeName)
      while (result.isAliasType) result = result.info.typeSymbol
      result match {
        case x: ClassSymbol => x
        case _              => MissingRequirementError.notFound("class " + fullname)
      }
    }

    override def staticModule(fullName: String): ModuleSymbol = getRequiredModule(fullName)

    def getModule(fullname: Name): ModuleSymbol =
      // [Eugene++] should be a ClassCastException instead?
      getModuleOrClass(fullname.toTermName) match {
        case x: ModuleSymbol => x
        case _               => MissingRequirementError.notFound("object " + fullname)
      }

    def getPackage(fullname: Name): ModuleSymbol = getModule(fullname)

    def getRequiredPackage(fullname: String): ModuleSymbol =
      getPackage(newTermNameCached(fullname))

    @deprecated("Use getClassByName", "2.10.0")
    def getClass(fullname: Name): ClassSymbol = getClassByName(fullname)

    def getRequiredClass(fullname: String): ClassSymbol =
      getClassByName(newTypeNameCached(fullname)) match {
        case x: ClassSymbol => x
        case _              => MissingRequirementError.notFound("class " + fullname)
      }

    def getRequiredModule(fullname: String): ModuleSymbol =
      getModule(newTermNameCached(fullname))

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

    def requiredClass[T: ClassTag] : ClassSymbol =
      getRequiredClass(erasureName[T])

    // TODO: What syntax do we think should work here? Say you have an object
    // like scala.Predef.  You can't say requiredModule[scala.Predef] since there's
    // no accompanying Predef class, and if you say requiredModule[scala.Predef.type]
    // the name found via the erasure is scala.Predef$.  For now I am
    // removing the trailing $, but I think that classTag should have
    // a method which returns a usable name, one which doesn't expose this
    // detail of the backend.
    def requiredModule[T: ClassTag] : ModuleSymbol =
      getRequiredModule(erasureName[T] stripSuffix "$")

    def getClassIfDefined(fullname: String): Symbol =
      getClassIfDefined(newTypeName(fullname))

    def getClassIfDefined(fullname: Name): Symbol =
      wrapMissing(getClassByName(fullname.toTypeName))

    def getModuleIfDefined(fullname: String): Symbol =
      getModuleIfDefined(newTermName(fullname))

    def getModuleIfDefined(fullname: Name): Symbol =
      wrapMissing(getModule(fullname.toTermName))

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

      EmptyPackageClass setInfo ClassInfoType(Nil, newPackageScope(EmptyPackageClass), EmptyPackageClass)
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
      override def ownerOfNewSymbols = EmptyPackageClass
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
