/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import scala.collection.{ mutable, immutable }
import Flags._
import PartialFunction._

trait Definitions extends reflect.api.StandardDefinitions {
  self: SymbolTable =>

  // the scala value classes
  trait ValueClassDefinitions {
    self: definitions.type =>

    private[Definitions] def valueCache(name: Name) = {
      if (name.isTypeName) ScalaPackageClass.info member name
      else ScalaPackageClass.info member name suchThat (_ hasFlag MODULE)
    }
    private[Definitions] def valueModuleMethod(className: Name, methodName: Name): Symbol = {
      valueCache(className.toTermName).moduleClass.tpe member methodName
    }

    import ClassfileConstants._

    private val nameToWeight = Map[Name, Int](
      tpnme.Byte   -> 2,
      tpnme.Char   -> 3,
      tpnme.Short  -> 4,
      tpnme.Int    -> 12,
      tpnme.Long   -> 24,
      tpnme.Float  -> 48,
      tpnme.Double -> 96
    )

    private val nameToTag = Map[Name, Char](
      tpnme.Byte    -> BYTE_TAG,
      tpnme.Char    -> CHAR_TAG,
      tpnme.Short   -> SHORT_TAG,
      tpnme.Int     -> INT_TAG,
      tpnme.Long    -> LONG_TAG,
      tpnme.Float   -> FLOAT_TAG,
      tpnme.Double  -> DOUBLE_TAG,
      tpnme.Boolean -> BOOL_TAG,
      tpnme.Unit    -> VOID_TAG,
      tpnme.Object  -> TVAR_TAG
    )

    private def classesMap[T](f: Name => T) = symbolsMap(ScalaValueClassesNoUnit, f)
    private def symbolsMap[T](syms: List[Symbol], f: Name => T): Map[Symbol, T] = syms zip (syms map (x => f(x.name))) toMap
    private def symbolsMapFilt[T](syms: List[Symbol], p: Name => Boolean, f: Name => T) = symbolsMap(syms filter (x => p(x.name)), f)

    private def boxedName(name: Name) = sn.Boxed(name.toTypeName)

    lazy val abbrvTag         = symbolsMap(ObjectClass :: ScalaValueClasses, nameToTag)
    lazy val numericWeight    = symbolsMapFilt(ScalaValueClasses, nameToWeight.keySet, nameToWeight)
    lazy val boxedModule      = classesMap(x => getModule(boxedName(x)))
    lazy val boxedClass       = classesMap(x => getClass(boxedName(x)))
    lazy val refClass         = classesMap(x => getClass("scala.runtime." + x + "Ref"))
    lazy val volatileRefClass = classesMap(x => getClass("scala.runtime.Volatile" + x + "Ref"))
    lazy val boxMethod        = classesMap(x => valueModuleMethod(x, nme.box))
    lazy val unboxMethod      = classesMap(x => valueModuleMethod(x, nme.unbox))

    private def newClass(owner: Symbol, name: TypeName, parents: List[Type]): Symbol = {
      val clazz = owner.newClass(NoPosition, name)
      clazz.setInfo(ClassInfoType(parents, new Scope, clazz))
      owner.info.decls.enter(clazz)
      clazz
    }

    def isNumericSubClass(sub: Symbol, sup: Symbol) = (
         (numericWeight contains sub)
      && (numericWeight contains sup)
      && (numericWeight(sup) % numericWeight(sub) == 0)
    )

    /** Is symbol a numeric value class? */
    def isNumericValueClass(sym: Symbol): Boolean =
      numericWeight contains sym

    def isGetClass(sym: Symbol) =
      (sym.name == nme.getClass_) && (sym.paramss.isEmpty || sym.paramss.head.isEmpty)

    private[Definitions] def fullNameStrings: List[String] = nme.ScalaValueNames map ("scala." + _)
    private[Definitions] lazy val fullValueName: Set[Name] = {
      val values = nme.ScalaValueNames flatMap (x => List(newTypeName("scala." + x), newTermName("scala." + x)))
      values.toSet + newTypeName("scala.AnyVal")
    }

    lazy val AnyValClass  = valueCache(tpnme.AnyVal)
    lazy val UnitClass    = valueCache(tpnme.Unit)
    lazy val ByteClass    = valueCache(tpnme.Byte)
    lazy val ShortClass   = valueCache(tpnme.Short)
    lazy val CharClass    = valueCache(tpnme.Char)
    lazy val IntClass     = valueCache(tpnme.Int)
    lazy val LongClass    = valueCache(tpnme.Long)
    lazy val FloatClass   = valueCache(tpnme.Float)
    lazy val DoubleClass  = valueCache(tpnme.Double)
    lazy val BooleanClass = valueCache(tpnme.Boolean)
      def Boolean_and = getMember(BooleanClass, nme.ZAND)
      def Boolean_or  = getMember(BooleanClass, nme.ZOR)
      def Boolean_not = getMember(BooleanClass, nme.UNARY_!)

    def ScalaValueClassesNoUnit = ScalaValueClasses filterNot (_ eq UnitClass)
    def ScalaValueClasses: List[Symbol] = List(
      UnitClass,
      BooleanClass,
      ByteClass,
      ShortClass,
      CharClass,
      IntClass,
      LongClass,
      FloatClass,
      DoubleClass
    )
  }

  object definitions extends AbsDefinitions with ValueClassDefinitions {
    private var isInitialized = false
    def isDefinitionsInitialized = isInitialized

    // symbols related to packages
    var emptypackagescope: Scope = null //debug

    // This is the package _root_.  The actual root cannot be referenced at
    // the source level, but _root_ is essentially a function () => <root>.
    lazy val RootPackage: Symbol = {
      val rp = NoSymbol.newValue(NoPosition, nme.ROOTPKG)
        .setFlag(FINAL | MODULE | PACKAGE | JAVA)
        .setInfo(NullaryMethodType(RootClass.tpe))
      RootClass.sourceModule = rp
      rp
    }

    // This is the actual root of everything, including the package _root_.
    lazy val RootClass: ModuleClassSymbol = (
      NoSymbol.newModuleClass(NoPosition, tpnme.ROOT)
        setFlag (FINAL | MODULE | PACKAGE | JAVA)
        setInfo rootLoader
    )
    // The empty package, which holds all top level types without given packages.
    lazy val EmptyPackage       = RootClass.newPackage(NoPosition, nme.EMPTY_PACKAGE_NAME).setFlag(FINAL)
    lazy val EmptyPackageClass  = EmptyPackage.moduleClass

    lazy val JavaLangPackage      = getModule(sn.JavaLang)
    lazy val JavaLangPackageClass = JavaLangPackage.moduleClass
    lazy val ScalaPackage         = getModule(nme.scala_)
    lazy val ScalaPackageClass    = ScalaPackage.moduleClass

    lazy val RuntimePackage       = getModule("scala.runtime")
    lazy val RuntimePackageClass  = RuntimePackage.moduleClass

    // convenient one-argument parameter lists
    lazy val anyparam     = List(AnyClass.typeConstructor)
    lazy val anyvalparam  = List(AnyValClass.typeConstructor)
    lazy val anyrefparam  = List(AnyRefClass.typeConstructor)

    // private parameter conveniences
    private def booltype    = BooleanClass.typeConstructor
    private def inttype     = IntClass.typeConstructor
    private def stringtype  = StringClass.typeConstructor

    // top types
    lazy val AnyClass             = newClass(ScalaPackageClass, tpnme.Any, Nil) setFlag (ABSTRACT)
    lazy val AnyRefClass          = newAlias(ScalaPackageClass, tpnme.AnyRef, ObjectClass.typeConstructor)
    lazy val ObjectClass          = getClass(sn.Object)
    lazy val AnyCompanionClass    = getClass("scala.AnyCompanion") setFlag (SEALED | ABSTRACT | TRAIT)
    lazy val AnyValCompanionClass = getClass("scala.AnyValCompanion") setFlag (SEALED | ABSTRACT | TRAIT)

    // bottom types
    lazy val RuntimeNothingClass  = getClass(ClassfileConstants.SCALA_NOTHING)
    lazy val RuntimeNullClass     = getClass(ClassfileConstants.SCALA_NULL)

    sealed abstract class BottomClassSymbol(name: TypeName, parent: Symbol) extends ClassSymbol(ScalaPackageClass, NoPosition, name) {
      locally {
        this setFlag ABSTRACT | TRAIT | FINAL
        this setInfo ClassInfoType(List(parent.tpe), new Scope, this)
        owner.info.decls enter this
      }
      final override def isBottomClass = true
    }
    final object NothingClass extends BottomClassSymbol(tpnme.Nothing, AnyClass) {
      override def isSubClass(that: Symbol) = true
    }
    final object NullClass extends BottomClassSymbol(tpnme.Null, AnyRefClass) {
      override def isSubClass(that: Symbol) = (
           (that eq AnyClass)
        || (that ne NothingClass) && (that isSubClass ObjectClass)
      )
    }

    // exceptions and other throwables
    lazy val ClassCastExceptionClass        = getClass("java.lang.ClassCastException")
    lazy val IndexOutOfBoundsExceptionClass = getClass(sn.IOOBException)
    lazy val InvocationTargetExceptionClass = getClass(sn.InvTargetException)
    lazy val MatchErrorClass                = getClass("scala.MatchError")
    lazy val NonLocalReturnControlClass     = getClass("scala.runtime.NonLocalReturnControl")
    lazy val NullPointerExceptionClass      = getClass(sn.NPException)
    lazy val ThrowableClass                 = getClass(sn.Throwable)
    lazy val UninitializedErrorClass        = getClass("scala.UninitializedFieldError")

    // fundamental reference classes
    lazy val ScalaObjectClass           = getMember(ScalaPackageClass, tpnme.ScalaObject)
    lazy val PartialFunctionClass       = getClass("scala.PartialFunction")
    lazy val AbstractPartialFunctionClass = getClass("scala.runtime.AbstractPartialFunction")
    lazy val SymbolClass                = getClass("scala.Symbol")
    lazy val StringClass                = getClass(sn.String)
    lazy val StringModule               = StringClass.linkedClassOfClass
    lazy val ClassClass                 = getClass(sn.Class)
      def Class_getMethod               = getMember(ClassClass, nme.getMethod_)
    lazy val DynamicClass               = getClass("scala.Dynamic")

    // fundamental modules
    lazy val SysPackage = getPackageObject("scala.sys")
      def Sys_error    = getMember(SysPackage, nme.error)

    // Modules whose members are in the default namespace
    lazy val UnqualifiedModules = List(PredefModule, ScalaPackage, JavaLangPackage)
    // Those modules and their module classes
    lazy val UnqualifiedOwners  = UnqualifiedModules.toSet ++ UnqualifiedModules.map(_.moduleClass)

    lazy val PredefModule: Symbol = getModule("scala.Predef")
    lazy val PredefModuleClass = PredefModule.moduleClass
      // Note: this is not the type alias AnyRef, it's a val defined in Predef
      // used by the @specialize annotation.
      def Predef_AnyRef = getMember(PredefModule, nme.AnyRef)
      def Predef_classOf = getMember(PredefModule, nme.classOf)
      def Predef_identity = getMember(PredefModule, nme.identity)
      def Predef_conforms = getMember(PredefModule, nme.conforms)
      def Predef_wrapRefArray = getMember(PredefModule, nme.wrapRefArray)
      
    /** Is `sym` a member of Predef with the given name?
     *  Note: DON't replace this by sym == Predef_conforms/etc, as Predef_conforms is a `def`
     *  which does a member lookup (it can't be a lazy val because we might reload Predef
     *  during resident compilations).
     */
    def isPredefMemberNamed(sym: Symbol, name: Name) = (
      (sym.name == name) && (sym.owner == PredefModule.moduleClass)
    )

    lazy val ConsoleModule: Symbol = getModule("scala.Console")
    lazy val ScalaRunTimeModule: Symbol = getModule("scala.runtime.ScalaRunTime")
    lazy val SymbolModule: Symbol = getModule("scala.Symbol")
    lazy val Symbol_apply = getMember(SymbolModule, nme.apply)
      def SeqFactory = getMember(ScalaRunTimeModule, nme.Seq)
      def arrayApplyMethod = getMember(ScalaRunTimeModule, "array_apply")
      def arrayUpdateMethod = getMember(ScalaRunTimeModule, "array_update")
      def arrayLengthMethod = getMember(ScalaRunTimeModule, "array_length")
      def arrayCloneMethod = getMember(ScalaRunTimeModule, "array_clone")
      def ensureAccessibleMethod = getMember(ScalaRunTimeModule, "ensureAccessible")
      def scalaRuntimeSameElements = getMember(ScalaRunTimeModule, nme.sameElements)

    // classes with special meanings
    lazy val StringAddClass   = getClass("scala.runtime.StringAdd")
    lazy val ArrowAssocClass  = getClass("scala.Predef.ArrowAssoc")
    lazy val StringAdd_+      = getMember(StringAddClass, nme.PLUS)
    lazy val NotNullClass     = getClass("scala.NotNull")
    lazy val ScalaNumberClass           = getClass("scala.math.ScalaNumber")
    lazy val TraitSetterAnnotationClass = getClass("scala.runtime.TraitSetter")
    lazy val DelayedInitClass = getClass("scala.DelayedInit")
      def delayedInitMethod = getMember(DelayedInitClass, nme.delayedInit)
      // a dummy value that communicates that a delayedInit call is compiler-generated
      // from phase UnCurry to phase Constructors
      // !!! This is not used anywhere (it was checked in that way.)
      // def delayedInitArgVal = EmptyPackageClass.newValue(NoPosition, nme.delayedInitArg)
      //   .setInfo(UnitClass.tpe)

    lazy val TypeConstraintClass   = getClass("scala.annotation.TypeConstraint")
    lazy val SingletonClass        = newClass(ScalaPackageClass, tpnme.Singleton, anyparam) setFlag (ABSTRACT | TRAIT | FINAL)
    lazy val SerializableClass     = getClass("scala.Serializable")
    lazy val JavaSerializableClass = getClass(sn.JavaSerializable)
    lazy val ComparableClass       = getClass("java.lang.Comparable")
    lazy val JavaCloneableClass    = getClass("java.lang.Cloneable")
    lazy val RemoteInterfaceClass  = getClass("java.rmi.Remote")
    lazy val RemoteExceptionClass  = getClass("java.rmi.RemoteException")

    lazy val RepeatedParamClass = newCovariantPolyClass(
      ScalaPackageClass,
      tpnme.REPEATED_PARAM_CLASS_NAME,
      tparam => seqType(tparam.typeConstructor)
    )

    lazy val JavaRepeatedParamClass = newCovariantPolyClass(
      ScalaPackageClass,
      tpnme.JAVA_REPEATED_PARAM_CLASS_NAME,
      tparam => arrayType(tparam.typeConstructor)
    )

    def isByNameParamType(tp: Type)        = tp.typeSymbol == ByNameParamClass
    def isScalaRepeatedParamType(tp: Type) = tp.typeSymbol == RepeatedParamClass
    def isJavaRepeatedParamType(tp: Type)  = tp.typeSymbol == JavaRepeatedParamClass
    def isRepeatedParamType(tp: Type)      = isScalaRepeatedParamType(tp) || isJavaRepeatedParamType(tp)
    def isCastSymbol(sym: Symbol)          = sym == Any_asInstanceOf || sym == Object_asInstanceOf

    def isJavaVarArgsMethod(m: Symbol)       = m.isMethod && isJavaVarArgs(m.info.params)
    def isJavaVarArgs(params: List[Symbol])  = params.nonEmpty && isJavaRepeatedParamType(params.last.tpe)
    def isScalaVarArgs(params: List[Symbol]) = params.nonEmpty && isScalaRepeatedParamType(params.last.tpe)
    def isVarArgsList(params: List[Symbol])  = params.nonEmpty && isRepeatedParamType(params.last.tpe)
    def isVarArgTypes(formals: List[Type])   = formals.nonEmpty && isRepeatedParamType(formals.last)

    def hasRepeatedParam(tp: Type): Boolean = tp match {
      case MethodType(formals, restpe) => isScalaVarArgs(formals) || hasRepeatedParam(restpe)
      case PolyType(_, restpe)         => hasRepeatedParam(restpe)
      case _                           => false
    }

    def isPrimitiveArray(tp: Type) = tp match {
      case TypeRef(_, ArrayClass, arg :: Nil) => isValueClass(arg.typeSymbol)
      case _                                  => false
    }
    def isArrayOfSymbol(tp: Type, elem: Symbol) = tp match {
      case TypeRef(_, ArrayClass, arg :: Nil) => arg.typeSymbol == elem
      case _                                  => false
    }

    lazy val ByNameParamClass = newCovariantPolyClass(
      ScalaPackageClass,
      tpnme.BYNAME_PARAM_CLASS_NAME,
      tparam => AnyClass.typeConstructor
    )
    lazy val EqualsPatternClass = {
      val clazz = newClass(ScalaPackageClass, tpnme.EQUALS_PATTERN_NAME, Nil)
      clazz setInfo polyType(List(newTypeParam(clazz, 0)), ClassInfoType(anyparam, new Scope, clazz))
    }

    // collections classes
    lazy val ConsClass          = getClass("scala.collection.immutable.$colon$colon")
    lazy val IterableClass      = getClass("scala.collection.Iterable")
    lazy val IteratorClass      = getClass("scala.collection.Iterator")
    lazy val ListClass          = getClass("scala.collection.immutable.List")
    lazy val SeqClass           = getClass("scala.collection.Seq")
    lazy val StringBuilderClass = getClass("scala.collection.mutable.StringBuilder")
    lazy val TraversableClass   = getClass("scala.collection.Traversable")

    lazy val ListModule       = getModule("scala.collection.immutable.List")
      lazy val List_apply = getMember(ListModule, nme.apply)
    lazy val NilModule        = getModule("scala.collection.immutable.Nil")
    lazy val SeqModule        = getModule("scala.collection.Seq")

    // arrays and their members
    lazy val ArrayModule  = getModule("scala.Array")
      def ArrayModule_overloadedApply = getMember(ArrayModule, nme.apply)
    lazy val ArrayClass   = getClass("scala.Array")
      def Array_apply      = getMember(ArrayClass, nme.apply)
      def Array_update     = getMember(ArrayClass, nme.update)
      def Array_length     = getMember(ArrayClass, nme.length)
      lazy val Array_clone = getMember(ArrayClass, nme.clone_)

    // reflection / structural types
    lazy val SoftReferenceClass     = getClass("java.lang.ref.SoftReference")
    lazy val WeakReferenceClass     = getClass("java.lang.ref.WeakReference")
    lazy val MethodClass            = getClass(sn.MethodAsObject)
      def methodClass_setAccessible = getMember(MethodClass, nme.setAccessible)
    lazy val EmptyMethodCacheClass  = getClass("scala.runtime.EmptyMethodCache")
    lazy val MethodCacheClass       = getClass("scala.runtime.MethodCache")
      def methodCache_find  = getMember(MethodCacheClass, nme.find_)
      def methodCache_add   = getMember(MethodCacheClass, nme.add_)

    // scala.reflect
    lazy val ReflectApiUniverse = getClass("scala.reflect.api.Universe")
    lazy val ReflectRuntimeMirror = getModule("scala.reflect.runtime.Mirror")
      def freeValueMethod = getMember(ReflectRuntimeMirror, "freeValue")
    lazy val ReflectPackage = getPackageObject("scala.reflect")
      def Reflect_mirror = getMember(ReflectPackage, "mirror")


    lazy val PartialManifestClass  = getClass("scala.reflect.ClassManifest")
    lazy val PartialManifestModule = getModule("scala.reflect.ClassManifest")
    lazy val FullManifestClass     = getClass("scala.reflect.Manifest")
    lazy val FullManifestModule    = getModule("scala.reflect.Manifest")
    lazy val OptManifestClass      = getClass("scala.reflect.OptManifest")
    lazy val NoManifest            = getModule("scala.reflect.NoManifest")
    lazy val CodeClass             = getClass(sn.Code)
    lazy val CodeModule            = getModule(sn.Code)
      def Code_lift = getMember(CodeModule, nme.lift_)

    lazy val ScalaSignatureAnnotation = getClass("scala.reflect.ScalaSignature")
    lazy val ScalaLongSignatureAnnotation = getClass("scala.reflect.ScalaLongSignature")

    // Option classes
    lazy val OptionClass: Symbol = getClass("scala.Option")
    lazy val SomeClass: Symbol   = getClass("scala.Some")
    lazy val NoneModule: Symbol  = getModule("scala.None")
    lazy val SomeModule: Symbol  = getModule("scala.Some")

    /** Note: don't use this manifest/type function for anything important,
     *  as it is incomplete.  Would love to have things like existential types
     *  working, but very unfortunately the manifests just stuff the relevant
     *  information into the toString method.
     */
    def manifestToType(m: OptManifest[_]): Type = m match {
      case x: AnyValManifest[_] =>
        getClassIfDefined("scala." + x).tpe
      case m: ClassManifest[_] =>
        val name = m.erasure.getName
        if (name endsWith nme.MODULE_SUFFIX_STRING)
          getModuleIfDefined(name stripSuffix nme.MODULE_SUFFIX_STRING).tpe
        else {
          val sym  = getClassIfDefined(name)
          val args = m.typeArguments

          if (sym eq NoSymbol) NoType
          else if (args.isEmpty) sym.tpe
          else appliedType(sym.typeConstructor, args map manifestToType)
        }
      case _ =>
        NoType
    }

    // The given symbol represents either String.+ or StringAdd.+
    def isStringAddition(sym: Symbol) = sym == String_+ || sym == StringAdd_+
    def isArrowAssoc(sym: Symbol) = ArrowAssocClass.tpe.decls.toList contains sym

    // The given symbol is a method with the right signature to be a runnable java program.
    def isJavaMainMethod(sym: Symbol) = sym.tpe match {
      case MethodType(param :: Nil, restpe) if restpe.typeSymbol == UnitClass =>
        param.tpe match {
          case TypeRef(_, ArrayClass, arg :: Nil) => arg.typeSymbol == StringClass
          case _                                  => false
        }
      case _ => false
    }
    // The given class has a main method.
    def hasJavaMainMethod(sym: Symbol): Boolean =
      (sym.tpe member nme.main).alternatives exists isJavaMainMethod
    def hasJavaMainMethod(path: String): Boolean =
      hasJavaMainMethod(getModuleIfDefined(path))

    def isOptionType(tp: Type)  = cond(tp.normalize) { case TypeRef(_, OptionClass, List(_)) => true }
    def isSomeType(tp: Type)    = cond(tp.normalize) { case TypeRef(_,   SomeClass, List(_)) => true }
    def isNoneType(tp: Type)    = cond(tp.normalize) { case TypeRef(_,   NoneModule, List(_)) => true }

    def optionType(tp: Type)    = typeRef(NoPrefix, OptionClass, List(tp))
    def someType(tp: Type)      = typeRef(NoPrefix, SomeClass, List(tp))
    def symbolType              = typeRef(SymbolClass.typeConstructor.prefix, SymbolClass, List())
    def longType                = typeRef(LongClass.typeConstructor.prefix, LongClass, List())

    // Product, Tuple, Function
    private def mkArityArray(name: String, arity: Int, countFrom: Int = 1): Array[Symbol] = {
      val list = countFrom to arity map (i => getClass("scala." + name + i))
      if (countFrom == 0) list.toArray
      else (NoSymbol +: list).toArray
    }

    val MaxTupleArity, MaxProductArity, MaxFunctionArity = 22
    /** The maximal dimensions of a generic array creation.
     *  I.e. new Array[Array[Array[Array[Array[T]]]]] creates a 5 times
     *  nested array. More is not allowed.
     */
    val MaxArrayDims = 5
    lazy val TupleClass     = mkArityArray("Tuple", MaxTupleArity)
    lazy val ProductClass   = mkArityArray("Product", MaxProductArity)
    lazy val FunctionClass  = mkArityArray("Function", MaxFunctionArity, 0)
    lazy val AbstractFunctionClass = mkArityArray("runtime.AbstractFunction", MaxFunctionArity, 0)
    lazy val isProductNClass = ProductClass.toSet

      def tupleField(n: Int, j: Int) = getMember(TupleClass(n), "_" + j)
      def isTupleType(tp: Type): Boolean = isTupleType(tp, false)
      def isTupleTypeOrSubtype(tp: Type): Boolean = isTupleType(tp, true)
      private def isTupleType(tp: Type, subtypeOK: Boolean) = tp.normalize match {
        case TypeRef(_, sym, args) if args.nonEmpty =>
          val len = args.length
          len <= MaxTupleArity && {
            val tsym = TupleClass(len)
            (sym == tsym) || (subtypeOK && !tp.isHigherKinded && sym.isSubClass(tsym))
          }
        case _ => false
      }

      def tupleType(elems: List[Type]) = {
        val len = elems.length
        if (len <= MaxTupleArity) {
          val sym = TupleClass(len)
          typeRef(sym.typeConstructor.prefix, sym, elems)
        } else NoType
      }

    lazy val ProductRootClass: Symbol = getClass("scala.Product")
      def Product_productArity = getMember(ProductRootClass, nme.productArity)
      def Product_productElement = getMember(ProductRootClass, nme.productElement)
      // def Product_productElementName = getMember(ProductRootClass, nme.productElementName)
      def Product_iterator = getMember(ProductRootClass, nme.productIterator)
      def Product_productPrefix = getMember(ProductRootClass, nme.productPrefix)
      def Product_canEqual = getMember(ProductRootClass, nme.canEqual_)

      def productProj(z:Symbol, j: Int): Symbol = getMember(z, nme.productAccessorName(j))
      def productProj(n: Int,   j: Int): Symbol = productProj(ProductClass(n), j)

      /** returns true if this type is exactly ProductN[T1,...,Tn], not some subclass */
      def isExactProductType(tp: Type): Boolean = isProductNClass(tp.typeSymbol)

      def productType(elems: List[Type]) = {
        if (elems.isEmpty) UnitClass.tpe
        else {
          val len = elems.length
          if (len <= MaxProductArity) {
            val sym = ProductClass(len)
            typeRef(sym.typeConstructor.prefix, sym, elems)
          }
          else NoType
        }
      }

    /** if tpe <: ProductN[T1,...,TN], returns List(T1,...,TN) else Nil */
    def getProductArgs(tpe: Type): List[Type] = tpe.baseClasses find isProductNClass match {
      case Some(x)  => tpe.baseType(x).typeArgs
      case _        => Nil
    }

    def unapplyUnwrap(tpe:Type) = tpe.finalResultType.normalize match {
      case RefinedType(p :: _, _) => p.normalize
      case tp                     => tp
    }

    def functionApply(n: Int) = getMember(FunctionClass(n), nme.apply)
    def functionType(formals: List[Type], restpe: Type) = {
      val len = formals.length
      if (len <= MaxFunctionArity) {
        val sym = FunctionClass(len)
        typeRef(sym.typeConstructor.prefix, sym, formals :+ restpe)
      } else NoType
    }

    def abstractFunctionForFunctionType(tp: Type) = tp.normalize match {
      case tr @ TypeRef(_, _, args) if isFunctionType(tr) =>
        val sym = AbstractFunctionClass(args.length - 1)
        typeRef(sym.typeConstructor.prefix, sym, args)
      case _ =>
        NoType
    }

    def isFunctionType(tp: Type): Boolean = tp.normalize match {
      case TypeRef(_, sym, args) if args.nonEmpty =>
        val len = args.length
        len < MaxFunctionArity && sym == FunctionClass(len - 1)
      case _ =>
        false
    }

    def isSeqType(tp: Type) = elementType(SeqClass, tp.normalize) != NoType

    def elementType(container: Symbol, tp: Type): Type = tp match {
      case TypeRef(_, `container`, arg :: Nil)  => arg
      case _                                    => NoType
    }

    def seqType(arg: Type)    = appliedType(SeqClass.typeConstructor, List(arg))
    def arrayType(arg: Type)  = appliedType(ArrayClass.typeConstructor, List(arg))
    def byNameType(arg: Type) = appliedType(ByNameParamClass.typeConstructor, List(arg))

    def ClassType(arg: Type) =
      if (phase.erasedTypes || forMSIL) ClassClass.tpe
      else appliedType(ClassClass.typeConstructor, List(arg))

    //
    // .NET backend
    //

    lazy val ComparatorClass = getClass("scala.runtime.Comparator")
    // System.ValueType
    lazy val ValueTypeClass: Symbol = getClass(sn.ValueType)
    // System.MulticastDelegate
    lazy val DelegateClass: Symbol = getClass(sn.Delegate)
    var Delegate_scalaCallers: List[Symbol] = List()
    // Symbol -> (Symbol, Type): scalaCaller -> (scalaMethodSym, DelegateType)
    // var Delegate_scalaCallerInfos: HashMap[Symbol, (Symbol, Type)] = _
    lazy val Delegate_scalaCallerTargets: mutable.HashMap[Symbol, Symbol] = mutable.HashMap()

    def isCorrespondingDelegate(delegateType: Type, functionType: Type): Boolean = {
      isSubType(delegateType, DelegateClass.tpe) &&
      (delegateType.member(nme.apply).tpe match {
      	case MethodType(delegateParams, delegateReturn) =>
      	  isFunctionType(functionType) &&
      	  (functionType.normalize match {
      	    case TypeRef(_, _, args) =>
      	      (delegateParams.map(pt => {
                      if (pt.tpe == AnyClass.tpe) definitions.ObjectClass.tpe else pt})
      	       ::: List(delegateReturn)) == args
      	    case _ => false
      	  })
        case _ => false
      })
    }

    // members of class scala.Any
    var Any_==          : Symbol = _
    var Any_!=          : Symbol = _
    var Any_equals      : Symbol = _
    var Any_hashCode    : Symbol = _
    var Any_toString    : Symbol = _
    var Any_getClass    : Symbol = _
    var Any_isInstanceOf: Symbol = _
    var Any_asInstanceOf: Symbol = _
    var Any_##          : Symbol = _

    // members of class java.lang.{Object, String}
    var Object_eq          : Symbol = _
    var Object_ne          : Symbol = _
    var Object_==          : Symbol = _
    var Object_!=          : Symbol = _
    var Object_##          : Symbol = _
    var Object_synchronized: Symbol = _
    lazy val Object_isInstanceOf = newPolyMethod(
      ObjectClass, "$isInstanceOf",
      tparam => MethodType(List(), booltype)) setFlag (FINAL | SYNTHETIC)
    lazy val Object_asInstanceOf = newPolyMethod(
      ObjectClass, "$asInstanceOf",
      tparam => MethodType(List(), tparam.typeConstructor)) setFlag (FINAL | SYNTHETIC)

    def Object_getClass  = getMember(ObjectClass, nme.getClass_)
    def Object_clone     = getMember(ObjectClass, nme.clone_)
    def Object_finalize  = getMember(ObjectClass, nme.finalize_)
    def Object_notify    = getMember(ObjectClass, nme.notify_)
    def Object_notifyAll = getMember(ObjectClass, nme.notifyAll_)
    def Object_equals    = getMember(ObjectClass, nme.equals_)
    def Object_hashCode  = getMember(ObjectClass, nme.hashCode_)
    def Object_toString  = getMember(ObjectClass, nme.toString_)

    var String_+           : Symbol = _

    // boxed classes
    lazy val ObjectRefClass         = getClass("scala.runtime.ObjectRef")
    lazy val VolatileObjectRefClass = getClass("scala.runtime.VolatileObjectRef")
    lazy val BoxesRunTimeClass      = getModule("scala.runtime.BoxesRunTime")
    lazy val BoxedNumberClass       = getClass(sn.BoxedNumber)
    lazy val BoxedCharacterClass    = getClass(sn.BoxedCharacter)
    lazy val BoxedBooleanClass      = getClass(sn.BoxedBoolean)
    lazy val BoxedByteClass         = getClass("java.lang.Byte")
    lazy val BoxedShortClass        = getClass("java.lang.Short")
    lazy val BoxedIntClass          = getClass("java.lang.Integer")
    lazy val BoxedLongClass         = getClass("java.lang.Long")
    lazy val BoxedFloatClass        = getClass("java.lang.Float")
    lazy val BoxedDoubleClass       = getClass("java.lang.Double")

    lazy val BoxedUnitClass         = getClass("scala.runtime.BoxedUnit")
    lazy val BoxedUnitModule        = getModule("scala.runtime.BoxedUnit")
      def BoxedUnit_UNIT = getMember(BoxedUnitModule, "UNIT")
      def BoxedUnit_TYPE = getMember(BoxedUnitModule, "TYPE")

    // Annotation base classes
    lazy val AnnotationClass            = getClass("scala.annotation.Annotation")
    lazy val ClassfileAnnotationClass   = getClass("scala.annotation.ClassfileAnnotation")
    lazy val StaticAnnotationClass      = getClass("scala.annotation.StaticAnnotation")

    // Annotations
    lazy val BridgeClass                = getClass("scala.annotation.bridge")
    lazy val ElidableMethodClass        = getClass("scala.annotation.elidable")
    lazy val ImplicitNotFoundClass      = getClass("scala.annotation.implicitNotFound")
    lazy val MigrationAnnotationClass   = getClass("scala.annotation.migration")
    lazy val ScalaStrictFPAttr          = getClass("scala.annotation.strictfp")
    lazy val SerializableAttr           = getClass("scala.annotation.serializable") // @serializable is deprecated
    lazy val SwitchClass                = getClass("scala.annotation.switch")
    lazy val TailrecClass               = getClass("scala.annotation.tailrec")
    lazy val VarargsClass               = getClass("scala.annotation.varargs")
    lazy val uncheckedStableClass       = getClass("scala.annotation.unchecked.uncheckedStable")
    lazy val uncheckedVarianceClass     = getClass("scala.annotation.unchecked.uncheckedVariance")

    lazy val BeanPropertyAttr           = getClass("scala.beans.BeanProperty")
    lazy val BooleanBeanPropertyAttr    = getClass("scala.beans.BooleanBeanProperty")
    lazy val CloneableAttr              = getClass("scala.cloneable")
    lazy val DeprecatedAttr             = getClass("scala.deprecated")
    lazy val DeprecatedNameAttr         = getClass("scala.deprecatedName")
    lazy val NativeAttr                 = getClass("scala.native")
    lazy val RemoteAttr                 = getClass("scala.remote")
    lazy val ScalaInlineClass           = getClass("scala.inline")
    lazy val ScalaNoInlineClass         = getClass("scala.noinline")
    lazy val SerialVersionUIDAttr       = getClass("scala.SerialVersionUID")
    lazy val SpecializedClass           = getClass("scala.specialized")
    lazy val ThrowsClass                = getClass("scala.throws")
    lazy val TransientAttr              = getClass("scala.transient")
    lazy val UncheckedClass             = getClass("scala.unchecked")
    lazy val VolatileAttr               = getClass("scala.volatile")

    // Meta-annotations
    lazy val BeanGetterTargetClass      = getMetaAnnotation("beanGetter")
    lazy val BeanSetterTargetClass      = getMetaAnnotation("beanSetter")
    lazy val FieldTargetClass           = getMetaAnnotation("field")
    lazy val GetterTargetClass          = getMetaAnnotation("getter")
    lazy val ParamTargetClass           = getMetaAnnotation("param")
    lazy val SetterTargetClass          = getMetaAnnotation("setter")
    // TODO: module, moduleClass? package, packageObject?

    private def getMetaAnnotation(name: String) = getClass("scala.annotation.meta." + name)
    def isMetaAnnotation(sym: Symbol): Boolean = metaAnnotations(sym) || (
      // Trying to allow for deprecated locations
      sym.isAliasType && isMetaAnnotation(sym.info.typeSymbol)
    )
    lazy val metaAnnotations = Set(
      FieldTargetClass, ParamTargetClass,
      GetterTargetClass, SetterTargetClass,
      BeanGetterTargetClass, BeanSetterTargetClass
    )

    lazy val AnnotationDefaultAttr: Symbol = {
      val attr = newClass(RuntimePackageClass, tpnme.AnnotationDefaultATTR, List(AnnotationClass.typeConstructor))
      // This attribute needs a constructor so that modifiers in parsed Java code make sense
      attr.info.decls enter attr.newClassConstructor(NoPosition)
      attr
    }

    def getPackageObjectClass(fullname: Name): Symbol =
      getPackageObject(fullname).companionClass

    def getPackageObject(fullname: Name): Symbol =
      getModule(fullname).info member nme.PACKAGE

    def getModule(fullname: Name): Symbol =
      getModuleOrClass(fullname.toTermName)

    def getClass(fullname: Name): Symbol = {
      var result = getModuleOrClass(fullname.toTypeName)
      while (result.isAliasType) result = result.info.typeSymbol
      result
    }

    def getClassIfDefined(fullname: String): Symbol =
      getClassIfDefined(newTypeName(fullname))
    def getClassIfDefined(fullname: Name): Symbol =
      try getClass(fullname.toTypeName)
      catch { case _: MissingRequirementError => NoSymbol }

    def getModuleIfDefined(fullname: String): Symbol =
      getModuleIfDefined(newTermName(fullname))
    def getModuleIfDefined(fullname: Name): Symbol =
      try getModule(fullname.toTermName)
      catch { case _: MissingRequirementError => NoSymbol }

    def getMember(owner: Symbol, name: Name): Symbol = {
      if (owner == NoSymbol) NoSymbol
      else owner.info.nonPrivateMember(name) match {
        case NoSymbol => throw new FatalError(owner + " does not have a member " + name)
        case result   => result
      }
    }
    def packageExists(packageName: String): Boolean =
      getModuleIfDefined(packageName).isPackage

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
        missingHook(owner, name) orElse {
          MissingRequirementError.notFound((if (path.isTermName) "object " else "class ")+path)
        }
      }
    }

    /** If you're looking for a class, pass a type name.
     *  If a module, a term name.
     */
    private def getModuleOrClass(path: Name): Symbol = getModuleOrClass(path, path.length)

    private def newClass(owner: Symbol, name: TypeName, parents: List[Type]): Symbol = {
      val clazz = owner.newClass(NoPosition, name)
      clazz.setInfo(ClassInfoType(parents, new Scope, clazz))
      owner.info.decls.enter(clazz)
      clazz
    }

    private def newCovariantPolyClass(owner: Symbol, name: TypeName, parent: Symbol => Type): Symbol = {
      val clazz  = newClass(owner, name, List())
      val tparam = newTypeParam(clazz, 0) setFlag COVARIANT
      val p      = parent(tparam)
/*      p.typeSymbol.initialize
      println(p.typeSymbol + " flags: " + Flags.flagsToString(p.typeSymbol.flags))
      val parents = /*if (p.typeSymbol.isTrait)
        List(definitions.AnyRefClass.tpe, p)
                    else*/ List(p)
      println("creating " + name + " with parents " + parents) */
      clazz.setInfo(
        polyType(
          List(tparam),
          ClassInfoType(List(AnyRefClass.tpe, p), new Scope, clazz)))
    }

    private def newAlias(owner: Symbol, name: TypeName, alias: Type): Symbol = {
      val tpsym = owner.newAliasType(NoPosition, name)
      tpsym.setInfo(alias)
      owner.info.decls.enter(tpsym)
      tpsym
    }

    private def newMethod(owner: Symbol, name: TermName): Symbol = {
      val msym = owner.newMethod(NoPosition, name.encode)
      owner.info.decls.enter(msym)
      msym
    }

    private[Definitions] def newMethod(owner: Symbol, name: TermName, formals: List[Type], restpe: Type): Symbol = {
      val msym = newMethod(owner, name)
      val params = msym.newSyntheticValueParams(formals)
      msym.setInfo(MethodType(params, restpe))
    }

    /** tcon receives the type parameter symbol as argument */
    private def newPolyMethod(owner: Symbol, name: TermName, tcon: Symbol => Type): Symbol =
      newPolyMethodCon(owner, name, tparam => msym => tcon(tparam))

    /** tcon receives the type parameter symbol and the method symbol as arguments */
    private def newPolyMethodCon(owner: Symbol, name: TermName, tcon: Symbol => Symbol => Type): Symbol = {
      val msym = newMethod(owner, name)
      val tparam = newTypeParam(msym, 0)
      msym.setInfo(polyType(List(tparam), tcon(tparam)(msym)))
    }

    private def newParameterlessMethod(owner: Symbol, name: TermName, restpe: Type) =
      newMethod(owner, name).setInfo(NullaryMethodType(restpe))

    private def newTypeParam(owner: Symbol, index: Int): Symbol =
      owner.newTypeParameter(NoPosition, newTypeName("T" + index)) setInfo TypeBounds.empty

    lazy val boxedClassValues = boxedClass.values.toSet
    lazy val isUnbox = unboxMethod.values.toSet
    lazy val isBox = boxMethod.values.toSet

    /** Is symbol a phantom class for which no runtime representation exists? */
    lazy val isPhantomClass = Set[Symbol](AnyClass, AnyValClass, NullClass, NothingClass)

    /** Is the symbol that of a parent which is added during parsing? */
    lazy val isPossibleSyntheticParent = ProductClass.toSet[Symbol] + ProductRootClass + SerializableClass

    private lazy val scalaValueClassesSet = ScalaValueClasses.toSet
    private lazy val boxedValueClassesSet = boxedClass.values.toSet + BoxedUnitClass

    /** Is symbol a value class? */
    def isValueClass(sym: Symbol) = scalaValueClassesSet(sym)
    def isNonUnitValueClass(sym: Symbol) = (sym != UnitClass) && isValueClass(sym)
    def isScalaValueType(tp: Type) = scalaValueClassesSet(tp.typeSymbol)

    /** Is symbol a boxed value class, e.g. java.lang.Integer? */
    def isBoxedValueClass(sym: Symbol) = boxedValueClassesSet(sym)

    /** If symbol is a value class (boxed or not), return the unboxed
     *  value class.  Otherwise, NoSymbol.
     */
    def unboxedValueClass(sym: Symbol): Symbol =
      if (isValueClass(sym)) sym
      else if (sym == BoxedUnitClass) UnitClass
      else boxedClass.map(_.swap).getOrElse(sym, NoSymbol)

    /** Is type's symbol a numeric value class? */
    def isNumericValueType(tp: Type): Boolean = tp match {
      case TypeRef(_, sym, _) => isNumericValueClass(sym)
      case _ => false
    }

    // todo: reconcile with javaSignature!!!
    def signature(tp: Type): String = {
      def erasure(tp: Type): Type = tp match {
        case st: SubType => erasure(st.supertype)
        case RefinedType(parents, _) => erasure(parents.head)
        case _ => tp
      }
      def flatNameString(sym: Symbol, separator: Char): String =
        if (sym == NoSymbol) ""   // be more resistant to error conditions, e.g. neg/t3222.scala
        else if (sym.owner.isPackageClass) sym.javaClassName
        else flatNameString(sym.owner, separator) + nme.NAME_JOIN_STRING + sym.simpleName
      def signature1(etp: Type): String = {
        if (etp.typeSymbol == ArrayClass) "[" + signature1(erasure(etp.normalize.typeArgs.head))
        else if (isValueClass(etp.typeSymbol)) abbrvTag(etp.typeSymbol).toString()
        else "L" + flatNameString(etp.typeSymbol, '/') + ";"
      }
      val etp = erasure(tp)
      if (etp.typeSymbol == ArrayClass) signature1(etp)
      else flatNameString(etp.typeSymbol, '.')
    }

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

    /** Surgery on the value classes.  Without this, AnyVals defined in source
     *  files end up with an AnyRef parent.  It is likely there is a better way
     *  to evade that AnyRef.
     */
    private def setParents(sym: Symbol, parents: List[Type]): Symbol = sym.rawInfo match {
      case ClassInfoType(_, scope, clazz) =>
        sym setInfo ClassInfoType(parents, scope, clazz)
      case _ =>
        sym
    }

    def init() {
      if (isInitialized) return

      EmptyPackageClass setInfo ClassInfoType(Nil, newPackageScope(EmptyPackageClass), EmptyPackageClass)
      EmptyPackage setInfo EmptyPackageClass.tpe

      RootClass.info.decls enter EmptyPackage
      RootClass.info.decls enter RootPackage

      // members of class scala.Any
      Any_== = newMethod(AnyClass, nme.EQ, anyparam, booltype) setFlag FINAL
      Any_!= = newMethod(AnyClass, nme.NE, anyparam, booltype) setFlag FINAL
      Any_equals   = newMethod(AnyClass, nme.equals_, anyparam, booltype)
      Any_hashCode = newMethod(AnyClass, nme.hashCode_, Nil, inttype)
      Any_toString = newMethod(AnyClass, nme.toString_, Nil, stringtype)
      Any_##       = newMethod(AnyClass, nme.HASHHASH, Nil, inttype) setFlag FINAL

      // Any_getClass requires special handling.  The return type is determined on
      // a per-call-site basis as if the function being called were actually:
      //
      //    // Assuming `target.getClass()`
      //    def getClass[T](target: T): Class[_ <: T]
      //
      // Since getClass is not actually a polymorphic method, this requires compiler
      // participation.  At the "Any" level, the return type is Class[_] as it is in
      // java.lang.Object.  Java also special cases the return type.
      Any_getClass = (
        newMethod(AnyClass, nme.getClass_, Nil, getMember(ObjectClass, nme.getClass_).tpe.resultType)
          setFlag DEFERRED
      )
      Any_isInstanceOf = newPolyMethod(
        AnyClass, nme.isInstanceOf_, tparam => NullaryMethodType(booltype)) setFlag FINAL
      Any_asInstanceOf = newPolyMethod(
        AnyClass, nme.asInstanceOf_, tparam => NullaryMethodType(tparam.typeConstructor)) setFlag FINAL

      // members of class java.lang.{ Object, String }
      Object_## = newMethod(ObjectClass, nme.HASHHASH, Nil, inttype) setFlag FINAL
      Object_== = newMethod(ObjectClass, nme.EQ, anyrefparam, booltype) setFlag FINAL
      Object_!= = newMethod(ObjectClass, nme.NE, anyrefparam, booltype) setFlag FINAL
      Object_eq = newMethod(ObjectClass, nme.eq, anyrefparam, booltype) setFlag FINAL
      Object_ne = newMethod(ObjectClass, nme.ne, anyrefparam, booltype) setFlag FINAL
      Object_synchronized = newPolyMethodCon(
        ObjectClass, nme.synchronized_,
        tparam => msym => MethodType(msym.newSyntheticValueParams(List(tparam.typeConstructor)), tparam.typeConstructor)) setFlag FINAL

      String_+ = newMethod(
        StringClass, "+", anyparam, stringtype) setFlag FINAL

      val forced = List( // force initialization of every symbol that is entered as a side effect
        AnnotationDefaultAttr, // #2264
        RepeatedParamClass,
        JavaRepeatedParamClass,
        ByNameParamClass,
        AnyClass,
        AnyRefClass,
        AnyValClass,
        NullClass,
        NothingClass,
        SingletonClass,
        EqualsPatternClass,
        Object_isInstanceOf,
        Object_asInstanceOf
      )

      /** Removing the anyref parent they acquire from having a source file.
       */
      setParents(AnyValClass, anyparam)
      ScalaValueClasses foreach { sym =>
        setParents(sym, anyvalparam)
      }

      isInitialized = true
    } //init

    var nbScalaCallers: Int = 0
    def newScalaCaller(delegateType: Type): Symbol = {
      assert(forMSIL, "scalaCallers can only be created if target is .NET")
      // object: reference to object on which to call (scala-)method
      val paramTypes: List[Type] = List(ObjectClass.tpe)
      val name: String =  "$scalaCaller$$" + nbScalaCallers
      // tparam => resultType, which is the resultType of PolyType, i.e. the result type after applying the
      // type parameter =-> a MethodType in this case
      // TODO: set type bounds manually (-> MulticastDelegate), see newTypeParam
      val newCaller = newMethod(DelegateClass, name, paramTypes, delegateType) setFlag (FINAL | STATIC)
      // val newCaller = newPolyMethod(DelegateClass, name,
      // tparam => MethodType(paramTypes, tparam.typeConstructor)) setFlag (FINAL | STATIC)
      Delegate_scalaCallers = Delegate_scalaCallers ::: List(newCaller)
      nbScalaCallers += 1
      newCaller
    }

    // def addScalaCallerInfo(scalaCaller: Symbol, methSym: Symbol, delType: Type) {
    // assert(Delegate_scalaCallers contains scalaCaller)
    // Delegate_scalaCallerInfos += (scalaCaller -> (methSym, delType))
    // }

    def addScalaCallerInfo(scalaCaller: Symbol, methSym: Symbol) {
      assert(Delegate_scalaCallers contains scalaCaller)
      Delegate_scalaCallerTargets += (scalaCaller -> methSym)
    }
  }
}
