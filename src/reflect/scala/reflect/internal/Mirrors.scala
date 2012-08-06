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
    val RootPackage: ObjectSymbol
    val EmptyPackageClass: ClassSymbol
    val EmptyPackage: ObjectSymbol

    def findMemberFromRoot(fullName: Name): Symbol = {
      val segs = nme.segments(fullName.toString, fullName.isTermName)
      if (segs.isEmpty) NoSymbol
      else definitions.findNamedMember(segs.tail, RootClass.info member segs.head)
    }

    /** Todo: organize similar to mkStatic in reflect.Base */
    private def getObjectOrClass(path: Name, len: Int): Symbol = {
      val point = path lastPos('.', len - 1)
      val owner =
        if (point > 0) getObjectOrClass(path.toTermName, point)
        else RootClass
      val name = path subName (point + 1, len)
      var sym = owner.info member name
      val result = if (path.isTermName) sym.suchThat(_ hasFlag OBJECT) else sym
      if (result != NoSymbol) result
      else {
        if (settings.debug.value) { log(sym.info); log(sym.info.members) }//debug
        thisMirror.missingHook(owner, name) orElse {
          MissingRequirementError.notFound((if (path.isTermName) "object " else "class ")+path+" in "+thisMirror)
        }
      }
    }

    /** If you're looking for a class, pass a type name.
     *  If an object, a term name.
     *
     *  Unlike `getObjectOrClass`, this function
     *  loads unqualified names from the root package.
     */
    private def getObjectOrClass(path: Name): Symbol =
      getObjectOrClass(path, path.length)

    /** If you're looking for a class, pass a type name.
     *  If an object, a term name.
     *
     *  Unlike `getObjectOrClass`, this function
     *  loads unqualified names from the empty package.
     */
    private def staticObjectOrClass(path: Name): Symbol = {
      val isPackageless = path.pos('.') == path.length
      if (isPackageless) EmptyPackageClass.info decl path
      else getObjectOrClass(path)
    }

    protected def mirrorMissingHook(owner: Symbol, name: Name): Symbol = NoSymbol

    protected def universeMissingHook(owner: Symbol, name: Name): Symbol = self.missingHook(owner, name)

    private[scala] def missingHook(owner: Symbol, name: Name): Symbol = mirrorMissingHook(owner, name) orElse universeMissingHook(owner, name)

    // todo: get rid of most the methods here and keep just staticClass/Object/Package

    /************************ loaders of class symbols ************************/

    private def ensureClassSymbol(fullname: String, sym: Symbol): ClassSymbol = {
      var result = sym
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
      ensureClassSymbol(fullname.toString, getObjectOrClass(fullname.toTypeName))

    def getRequiredClass(fullname: String): ClassSymbol =
      getClassByName(newTypeNameCached(fullname))

    def requiredClass[T: ClassTag] : ClassSymbol =
      getRequiredClass(erasureName[T])

    def getClassIfDefined(fullname: String): Symbol =
      getClassIfDefined(newTypeNameCached(fullname))

    def getClassIfDefined(fullname: Name): Symbol =
      wrapMissing(getClassByName(fullname.toTypeName))

    /** @inheritdoc
     *
     *  Unlike getClassByName/getRequiredClass this function can also load packageless symbols.
     *  Compiler might ignore them, but they should be loadable with macros.
     */
    override def staticClass(fullname: String): ClassSymbol =
      ensureClassSymbol(fullname, staticObjectOrClass(newTypeNameCached(fullname)))

    /************************ loaders of object symbols ************************/

    private def ensureObjectSymbol(fullname: String, sym: Symbol, allowPackages: Boolean): ObjectSymbol =
      sym match {
        case x: ObjectSymbol if allowPackages || !x.isPackage => x
        case _                                                => MissingRequirementError.notFound("object " + fullname)
      }

    @deprecated("Use getObjectByName", "2.10.0")
    def getObject(fullname: Name): ObjectSymbol =
      getObjectByName(fullname)

    def getObjectByName(fullname: Name): ObjectSymbol =
      ensureObjectSymbol(fullname.toString, getObjectOrClass(fullname.toTermName), allowPackages = true)

    def getRequiredObject(fullname: String): ObjectSymbol =
      getObject(newTermNameCached(fullname))

    // TODO: What syntax do we think should work here? Say you have an object
    // like scala.Predef.  You can't say requiredObject[scala.Predef] since there's
    // no accompanying Predef class, and if you say requiredObject[scala.Predef.type]
    // the name found via the erasure is scala.Predef$.  For now I am
    // removing the trailing $, but I think that classTag should have
    // a method which returns a usable name, one which doesn't expose this
    // detail of the backend.
    def requiredObject[T: ClassTag] : ObjectSymbol =
      getRequiredObject(erasureName[T] stripSuffix "$")

    def getObjectIfDefined(fullname: String): Symbol =
      getObjectIfDefined(newTermNameCached(fullname))

    def getObjectIfDefined(fullname: Name): Symbol =
      wrapMissing(getObject(fullname.toTermName))

    /** @inheritdoc
     *
     *  Unlike getObject/getRequiredObject this function can also load packageless symbols.
     *  Compiler might ignore them, but they should be loadable with macros.
     */
    override def staticObject(fullname: String): ObjectSymbol =
      ensureObjectSymbol(fullname, staticObjectOrClass(newTermNameCached(fullname)), allowPackages = false)

    /************************ loaders of package symbols ************************/

    private def ensurePackageSymbol(fullname: String, sym: Symbol, allowObjects: Boolean): ObjectSymbol =
      sym match {
        case x: ObjectSymbol if allowObjects || x.isPackage => x
        case _                                              => MissingRequirementError.notFound("package " + fullname)
      }

    def getPackage(fullname: Name): ObjectSymbol =
      ensurePackageSymbol(fullname.toString, getObjectOrClass(fullname), allowObjects = true)

    def getRequiredPackage(fullname: String): ObjectSymbol =
      getPackage(newTermNameCached(fullname))

    def getPackageObject(fullname: String): ObjectSymbol =
      (getPackage(newTermName(fullname)).info member nme.PACKAGE) match {
        case x: ObjectSymbol => x
        case _               => MissingRequirementError.notFound("package object " + fullname)
      }

    def getPackageObjectIfDefined(fullname: String): Symbol =
      getPackageObjectIfDefined(newTermNameCached(fullname))

    def getPackageObjectIfDefined(fullname: Name): Symbol =
      wrapMissing(getPackageObject(fullname.toTermName))

    override def staticPackage(fullname: String): ObjectSymbol =
      ensurePackageSymbol(fullname.toString, getObjectOrClass(newTermNameCached(fullname)), allowObjects = false)

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

    /** getObject2/getClass2 aren't needed at present but may be again,
     *  so for now they're mothballed.
     */
    // def getObject2(name1: Name, name2: Name) = {
    //   try getObjectOrClass(name1.toTermName)
    //   catch { case ex1: FatalError =>
    //     try getObjectOrClass(name2.toTermName)
    //     catch { case ex2: FatalError => throw ex1 }
    //   }
    // }
    // def getClass2(name1: Name, name2: Name) = {
    //   try {
    //     val result = getObjectOrClass(name1.toTypeName)
    //     if (result.isAliasType) getClass(name2) else result
    //   }
    //   catch { case ex1: FatalError =>
    //     try getObjectOrClass(name2.toTypeName)
    //     catch { case ex2: FatalError => throw ex1 }
    //   }
    // }

    def init() {
      // Still fiddling with whether it's cleaner to do some of this setup here
      // or from constructors.  The latter approach tends to invite init order issues.

      EmptyPackageClass setInfo rootLoader
      EmptyPackage setInfo EmptyPackageClass.tpe

      connectObjectToClass(EmptyPackage, EmptyPackageClass)
      connectObjectToClass(RootPackage, RootClass)

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
    final object RootPackage extends ObjectSymbol(rootOwner, NoPosition, nme.ROOTPKG) with RootSymbol {
      this setInfo NullaryMethodType(RootClass.tpe)
      RootClass.sourceObject = this

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
    final object EmptyPackage extends ObjectSymbol(RootClass, NoPosition, nme.EMPTY_PACKAGE_NAME) with WellKnownSymbol {
      override def isEmptyPackage = true
    }
    final object EmptyPackageClass extends PackageClassSymbol(RootClass, NoPosition, tpnme.EMPTY_PACKAGE_NAME) with WellKnownSymbol {
      override def isEffectiveRoot     = true
      override def isEmptyPackageClass = true
    }
  }
}
