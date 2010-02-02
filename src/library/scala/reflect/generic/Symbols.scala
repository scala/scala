package scala.reflect
package generic

import Flags._

trait Symbols { self: Universe =>

  type Symbol >: Null <: AbsSymbol

  abstract class AbsSymbol { this: Symbol =>

    /** The owner of this symbol.
     */
    def owner: Symbol

    /** The flags of this symbol */
    def flags: Long

    /** The name of the symbol as a member of the `Name` type.
     */
    def name: Name

    /** The name of the symbol before decoding, e.g. `$eq$eq` instead of `==`.
     */
    def encodedName: String

    /** The decoded name of the symbol, e.g. `==` instead of `$eq$eq`.
     */
    def decodedName: String = stripLocalSuffix(NameTransformer.decode(encodedName))

    /** The encoded full path name of this symbol, where outer names and inner names
     *  are separated by `separator` characters.
     *  Never translates expansions of operators back to operator symbol.
     *  Never adds id.
     */
    final def fullName(separator: Char): String = stripLocalSuffix {
      if (isRoot || isRootPackage || this == NoSymbol) this.toString
      else if (owner.isEffectiveRoot) encodedName
      else owner.enclClass.fullName(separator) + separator + encodedName
    }

    private def stripLocalSuffix(s: String) = s stripSuffix nme.LOCAL_SUFFIX_STRING

    /** The encoded full path name of this symbol, where outer names and inner names
     *  are separated by periods.
     */
    final def fullName: String = fullName('.')

    /** Does symbol have a flag in `mask` set? */
    final def hasFlag(mask: Long): Boolean = (flags & mask) != 0L

    /** Set when symbol has a modifier of the form private[X], NoSymbol otherwise.
     */
    def privateWithin: Symbol

    /** The raw info of the type
     */
    def rawInfo: Type

    /** The type of the symbol
     */
    def tpe: Type = info

    /** The info of the symbol. This is like tpe, except for class symbols where the `info`
     *  describes the contents of the class whereas the `tpe` is a reference to the class.
     */
    def info: Type = {
      val tp = rawInfo
      tp.complete(this)
      tp
    }

    /** If this symbol is a class or trait, its self type, otherwise the type of the symbol itse;lf
     */
    def typeOfThis: Type

    def owner_=(sym: Symbol)         { throw new UnsupportedOperationException("owner_= inapplicable for " + this) }
    def flags_=(flags: Long)         { throw new UnsupportedOperationException("flags_= inapplicable for " + this) }
    def info_=(tp: Type)             { throw new UnsupportedOperationException("info_= inapplicable for " + this) }
    def typeOfThis_=(tp: Type)       { throw new UnsupportedOperationException("typeOfThis_= inapplicable for " + this) }
    def privateWithin_=(sym: Symbol) { throw new UnsupportedOperationException("privateWithin_= inapplicable for " + this) }
    def sourceModule_=(sym: Symbol)  { throw new UnsupportedOperationException("sourceModule_= inapplicable for " + this) }
    def addChild(sym: Symbol)        { throw new UnsupportedOperationException("addChild inapplicable for " + this) }
    def addAnnotation(annot: AnnotationInfo) { throw new UnsupportedOperationException("addAnnotation inapplicable for " + this) }

    /** For a module class its linked class, for a plain class
     *  the module class of its linked module.
     *  For instance
     *    object Foo
     *    class Foo
     *
     *  Then object Foo has a `moduleClass' (invisible to the user, the backend calls it Foo$
     *  linkedClassOfClass goes from class Foo$ to class Foo, and back.
     */
    def linkedClassOfClass: Symbol

    /** The module corresponding to this module class (note that this
     *  is not updated when a module is cloned), or NoSymbol if this is not a ModuleClass
     */
    def sourceModule: Symbol = NoSymbol

    /** If symbol is an object defition, it's implied associated class,
     *  otherwise NoSymbol
     */
    def moduleClass: Symbol

// flags and kind tests

    def isTerm         = false // to be overridden
    def isType         = false // to be overridden
    def isClass        = false // to be overridden
    def isAliasType    = false      //to be overridden
    def isAbstractType = false   //to be overridden
    private[scala] def isSkolem = false // to be overridden

    final def isImplicit = hasFlag(IMPLICIT)
    final def isFinal = hasFlag(FINAL)
    final def isPrivate = hasFlag(PRIVATE)
    final def isProtected = hasFlag(PROTECTED)

    /** Is this symbol a sealed class? */
          def isTrait: Boolean = isClass && hasFlag(TRAIT) // refined later for virtual classes.
    final def hasDefault = isParameter && hasFlag(DEFAULTPARAM)
    final def isAbstractClass = isClass && hasFlag(ABSTRACT)
    final def isAbstractOverride = isTerm && hasFlag(ABSTRACT) && hasFlag(OVERRIDE)
    final def isBridge = hasFlag(BRIDGE)
    final def isCase = hasFlag(CASE)
    final def isCaseAccessor = hasFlag(CASEACCESSOR)
    final def isContravariant = isType && hasFlag(CONTRAVARIANT)
    final def isCovariant = isType && hasFlag(COVARIANT)
    final def isDeferred = hasFlag(DEFERRED) && !isClass
    final def isExistentiallyBound = isType && hasFlag(EXISTENTIAL)
    final def isGetterOrSetter = hasFlag(ACCESSOR)
    final def isInterface = hasFlag(INTERFACE)
    final def isJavaDefined = hasFlag(JAVA)
    final def isLazy = hasFlag(LAZY)
    final def isMethod = isTerm && hasFlag(METHOD)
    final def isModule = isTerm && hasFlag(MODULE)
    final def isModuleClass = isClass && hasFlag(MODULE)
    final def isMutable = hasFlag(MUTABLE)
    final def isOverloaded = hasFlag(OVERLOADED)
    final def isOverride = hasFlag(OVERRIDE)
    final def isPackage = isModule && hasFlag(PACKAGE)
    final def isPackageClass = isClass && hasFlag(PACKAGE)
    final def isParamAccessor = hasFlag(PARAMACCESSOR)
    final def isParameter = hasFlag(PARAM)
    final def isRefinementClass = isClass && name == mkTypeName(nme.REFINE_CLASS_NAME)
    final def isSealed = isClass && (hasFlag(SEALED) || definitions.isValueClass(this))
    final def isSourceMethod = isTerm && (flags & (METHOD | STABLE)) == METHOD // exclude all accessors!!!
    final def isSuperAccessor = hasFlag(SUPERACCESSOR)
    final def isSynthetic = hasFlag(SYNTHETIC)
    final def isTypeParameter = isType && isParameter && !isSkolem

    /** Is this symbol an implementation class for a mixin? */
    final def isImplClass = isClass && hasFlag(IMPLCLASS)

    /** Is this symbol early initialized */
    final def isEarlyInitialized: Boolean = isTerm && hasFlag(PRESUPER)

    final def isPrivateLocal = hasFlag(PRIVATE) && hasFlag(LOCAL)
    final def isProtectedLocal = hasFlag(PROTECTED) && hasFlag(LOCAL)
    final def isPublic = !hasFlag(PRIVATE | PROTECTED) && privateWithin == NoSymbol

    final def isRoot = isPackageClass && owner == NoSymbol
    final def isRootPackage = isPackage && owner == NoSymbol
    final def isEmptyPackage = isPackage && name == nme.EMPTY_PACKAGE_NAME
    final def isEmptyPackageClass = isPackageClass && name == mkTypeName(nme.EMPTY_PACKAGE_NAME)

    /** Is this symbol an effective root for fullname string?
     */
    def isEffectiveRoot = isRoot || isEmptyPackageClass

    // creators

    def newValue(name: Name, pos: Position = NoPosition): Symbol
    def newAbstractType(name: Name, pos: Position = NoPosition): Symbol
    def newAliasType(name: Name, pos: Position = NoPosition): Symbol
    def newClass(name: Name, pos: Position = NoPosition): Symbol
    def newModuleClass(name: Name, pos: Position = NoPosition): Symbol
    def newMethod(name: Name, pos: Position = NoPosition): Symbol
    def newModule(name: Name, clazz: Symbol, pos: Position = NoPosition): Symbol

    // access to related symbols

    /** The next enclosing class */
    def enclClass: Symbol = if (isClass) this else owner.enclClass

    /** The next enclosing method */
    def enclMethod: Symbol = if (isSourceMethod) this else owner.enclMethod
  }

  val NoSymbol: Symbol
}


