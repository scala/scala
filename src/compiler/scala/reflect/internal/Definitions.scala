/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import annotation.{ switch }
import scala.collection.{ mutable, immutable }
import Flags._
import PartialFunction._

trait Definitions extends reflect.api.StandardDefinitions {
  self: SymbolTable =>

  /** Since both the value parameter types and the result type may
   *  require access to the type parameter symbols, we model polymorphic
   *  creation as a function from those symbols to (formal types, result type).
   *  The Option is to distinguish between nullary methods and empty-param-list
   *  methods.
   */
  private type PolyMethodCreator = List[Symbol] => (Option[List[Type]], Type)
  
  private def newClass(owner: Symbol, name: TypeName, parents: List[Type], flags: Long = 0L): Symbol = {
    val clazz = owner.newClassSymbol(name, NoPosition, flags)
    clazz setInfoAndEnter ClassInfoType(parents, newScope, clazz)
  }
  private def newMethod(owner: Symbol, name: TermName, formals: List[Type], restpe: Type, flags: Long = 0L): Symbol = {
    val msym   = owner.newMethod(name.encode, NoPosition, flags)
    val params = msym.newSyntheticValueParams(formals)
    msym setInfoAndEnter MethodType(params, restpe)
  }

  // the scala value classes
  trait ValueClassDefinitions {
    self: definitions.type =>

    private[Definitions] def valueCache(name: Name) = {
      val res = (
        if (name.isTypeName) ScalaPackageClass.info member name
        else ScalaPackageClass.info member name suchThat (_ hasFlag MODULE)
      )
      if (res eq NoSymbol)
        abort("Could not find value classes! This is a catastrophic failure.  scala " + scala.util.Properties.versionString)
      else res
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
      tpnme.Object  -> OBJECT_TAG
    )

    private def classesMap[T](f: Name => T) = symbolsMap(ScalaValueClassesNoUnit, f)
    private def symbolsMap[T](syms: List[Symbol], f: Name => T): Map[Symbol, T] = syms zip (syms map (x => f(x.name))) toMap
    private def symbolsMapFilt[T](syms: List[Symbol], p: Name => Boolean, f: Name => T) = symbolsMap(syms filter (x => p(x.name)), f)

    private def boxedName(name: Name) = sn.Boxed(name.toTypeName)

    lazy val abbrvTag         = symbolsMap(ObjectClass :: ScalaValueClasses, nameToTag)
    lazy val numericWeight    = symbolsMapFilt(ScalaValueClasses, nameToWeight.keySet, nameToWeight)
    lazy val boxedModule      = classesMap(x => getModule(boxedName(x)))
    lazy val boxedClass       = classesMap(x => getClass(boxedName(x)))
    lazy val refClass         = classesMap(x => getRequiredClass("scala.runtime." + x + "Ref"))
    lazy val volatileRefClass = classesMap(x => getRequiredClass("scala.runtime.Volatile" + x + "Ref"))
    lazy val boxMethod        = classesMap(x => valueModuleMethod(x, nme.box))
    lazy val unboxMethod      = classesMap(x => valueModuleMethod(x, nme.unbox))

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
      lazy val Boolean_and = getMember(BooleanClass, nme.ZAND)
      lazy val Boolean_or  = getMember(BooleanClass, nme.ZOR)
      lazy val Boolean_not = getMember(BooleanClass, nme.UNARY_!)

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
    def ScalaValueClassCompanions: List[Symbol] = ScalaValueClasses map (_.companionSymbol)
  }

  object definitions extends AbsDefinitions with ValueClassDefinitions {
    private var isInitialized = false
    def isDefinitionsInitialized = isInitialized

    // symbols related to packages
    var emptypackagescope: Scope = null //debug

    // This is the package _root_.  The actual root cannot be referenced at
    // the source level, but _root_ is essentially a function () => <root>.
    lazy val RootPackage: Symbol = {
      val rp = (
        NoSymbol.newValue(nme.ROOTPKG, NoPosition, FINAL | MODULE | PACKAGE | JAVA)
          setInfo NullaryMethodType(RootClass.tpe)
      )
      RootClass.sourceModule = rp
      rp
    }

    // This is the actual root of everything, including the package _root_.
    lazy val RootClass: ModuleClassSymbol = (
      NoSymbol.newModuleClassSymbol(tpnme.ROOT, NoPosition, FINAL | MODULE | PACKAGE | JAVA)
        setInfo rootLoader
    )
    // The empty package, which holds all top level types without given packages.
    lazy val EmptyPackage       = RootClass.newPackage(nme.EMPTY_PACKAGE_NAME, NoPosition, FINAL)
    lazy val EmptyPackageClass  = EmptyPackage.moduleClass

    lazy val JavaLangPackage      = getModule(sn.JavaLang)
    lazy val JavaLangPackageClass = JavaLangPackage.moduleClass
    lazy val ScalaPackage         = getModule(nme.scala_)
    lazy val ScalaPackageClass    = ScalaPackage.moduleClass

    lazy val RuntimePackage       = getRequiredModule("scala.runtime")
    lazy val RuntimePackageClass  = RuntimePackage.moduleClass
    
    lazy val JavaLangEnumClass = getRequiredClass("java.lang.Enum")

    // convenient one-argument parameter lists
    lazy val anyparam     = List(AnyClass.typeConstructor)
    lazy val anyvalparam  = List(AnyValClass.typeConstructor)
    lazy val anyrefparam  = List(AnyRefClass.typeConstructor)

    // private parameter conveniences
    private def booltype    = BooleanClass.typeConstructor
    private def inttype     = IntClass.typeConstructor
    private def stringtype  = StringClass.typeConstructor
    
    // Java types
    def javaTypeName(jclazz: Class[_]): TypeName = newTypeName(jclazz.getName)
    
    def javaTypeToValueClass(jtype: Class[_]): Symbol = jtype match {
      case java.lang.Void.TYPE      => UnitClass
      case java.lang.Byte.TYPE      => ByteClass
      case java.lang.Character.TYPE => CharClass
      case java.lang.Short.TYPE     => ShortClass
      case java.lang.Integer.TYPE   => IntClass
      case java.lang.Long.TYPE      => LongClass
      case java.lang.Float.TYPE     => FloatClass
      case java.lang.Double.TYPE    => DoubleClass
      case java.lang.Boolean.TYPE   => BooleanClass
      case _                        => NoSymbol
    }
    def valueClassToJavaType(sym: Symbol): Class[_] = sym match {
      case UnitClass    => java.lang.Void.TYPE
      case ByteClass    => java.lang.Byte.TYPE
      case CharClass    => java.lang.Character.TYPE
      case ShortClass   => java.lang.Short.TYPE
      case IntClass     => java.lang.Integer.TYPE
      case LongClass    => java.lang.Long.TYPE
      case FloatClass   => java.lang.Float.TYPE
      case DoubleClass  => java.lang.Double.TYPE
      case BooleanClass => java.lang.Boolean.TYPE
      case _            => null
    }

    // top types
    lazy val AnyClass             = newClass(ScalaPackageClass, tpnme.Any, Nil, ABSTRACT)
    lazy val AnyRefClass          = newAlias(ScalaPackageClass, tpnme.AnyRef, ObjectClass.typeConstructor)
    lazy val ObjectClass          = getClass(sn.Object)

    // Note: this is not the type alias AnyRef, it's a companion-like
    // object used by the @specialize annotation.
    def AnyRefModule = getMember(ScalaPackageClass, nme.AnyRef)
    @deprecated("Use AnyRefModule", "2.10.0") 
    def Predef_AnyRef = AnyRefModule

    // bottom types
    lazy val RuntimeNothingClass  = getClass(fulltpnme.RuntimeNothing)
    lazy val RuntimeNullClass     = getClass(fulltpnme.RuntimeNull)

    sealed abstract class BottomClassSymbol(name: TypeName, parent: Symbol) extends ClassSymbol(ScalaPackageClass, NoPosition, name) {
      locally {
        this initFlags ABSTRACT | TRAIT | FINAL
        this setInfoAndEnter ClassInfoType(List(parent.tpe), newScope, this)
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
    lazy val ClassCastExceptionClass        = getRequiredClass("java.lang.ClassCastException")
    lazy val IndexOutOfBoundsExceptionClass = getClass(sn.IOOBException)
    lazy val InvocationTargetExceptionClass = getClass(sn.InvTargetException)
    lazy val MatchErrorClass                = getRequiredClass("scala.MatchError")
    lazy val NonLocalReturnControlClass     = getRequiredClass("scala.runtime.NonLocalReturnControl")
    lazy val NullPointerExceptionClass      = getClass(sn.NPException)
    lazy val ThrowableClass                 = getClass(sn.Throwable)
    lazy val UninitializedErrorClass        = getRequiredClass("scala.UninitializedFieldError")

    // fundamental reference classes
    lazy val ScalaObjectClass           = getMember(ScalaPackageClass, tpnme.ScalaObject)
    lazy val PartialFunctionClass       = getRequiredClass("scala.PartialFunction")
    lazy val AbstractPartialFunctionClass = getRequiredClass("scala.runtime.AbstractPartialFunction")
    lazy val SymbolClass                = getRequiredClass("scala.Symbol")
    lazy val StringClass                = getClass(sn.String)
    lazy val StringModule               = StringClass.linkedClassOfClass
    lazy val ClassClass                 = getClass(sn.Class)
      def Class_getMethod               = getMember(ClassClass, nme.getMethod_)
    lazy val DynamicClass               = getRequiredClass("scala.Dynamic")

    // fundamental modules
    lazy val SysPackage = getPackageObject("scala.sys")
      def Sys_error    = getMember(SysPackage, nme.error)

    // Modules whose members are in the default namespace
    lazy val UnqualifiedModules = List(PredefModule, ScalaPackage, JavaLangPackage)
    // Those modules and their module classes
    lazy val UnqualifiedOwners  = UnqualifiedModules.toSet ++ UnqualifiedModules.map(_.moduleClass)

    lazy val PredefModule: Symbol = getRequiredModule("scala.Predef")
    lazy val PredefModuleClass = PredefModule.moduleClass
      
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
    
    /** Specialization.
     */
    lazy val SpecializableModule  = getRequiredModule("scala.Specializable")
    lazy val GroupOfSpecializable = SpecializableModule.info.member(newTypeName("Group"))

    lazy val ConsoleModule: Symbol      = getRequiredModule("scala.Console")
    lazy val ScalaRunTimeModule: Symbol = getRequiredModule("scala.runtime.ScalaRunTime")
    lazy val SymbolModule: Symbol       = getRequiredModule("scala.Symbol")
    lazy val Symbol_apply               = SymbolModule.info decl nme.apply

      def SeqFactory = getMember(ScalaRunTimeModule, nme.Seq)
      def arrayApplyMethod = getMember(ScalaRunTimeModule, nme.array_apply)
      def arrayUpdateMethod = getMember(ScalaRunTimeModule, nme.array_update)
      def arrayLengthMethod = getMember(ScalaRunTimeModule, nme.array_length)
      def arrayCloneMethod = getMember(ScalaRunTimeModule, nme.array_clone)
      def ensureAccessibleMethod = getMember(ScalaRunTimeModule, nme.ensureAccessible)
      def scalaRuntimeSameElements = getMember(ScalaRunTimeModule, nme.sameElements)

    // classes with special meanings
    lazy val StringAddClass   = getRequiredClass("scala.runtime.StringAdd")
    lazy val ArrowAssocClass  = getRequiredClass("scala.Predef.ArrowAssoc")
    lazy val StringAdd_+      = getMember(StringAddClass, nme.PLUS)
    lazy val NotNullClass     = getRequiredClass("scala.NotNull")
    lazy val ScalaNumberClass           = getRequiredClass("scala.math.ScalaNumber")
    lazy val TraitSetterAnnotationClass = getRequiredClass("scala.runtime.TraitSetter")
    lazy val DelayedInitClass = getRequiredClass("scala.DelayedInit")
      def delayedInitMethod = getMember(DelayedInitClass, nme.delayedInit)
      // a dummy value that communicates that a delayedInit call is compiler-generated
      // from phase UnCurry to phase Constructors
      // !!! This is not used anywhere (it was checked in that way.)
      // def delayedInitArgVal = EmptyPackageClass.newValue(NoPosition, nme.delayedInitArg)
      //   .setInfo(UnitClass.tpe)

    lazy val TypeConstraintClass   = getRequiredClass("scala.annotation.TypeConstraint")
    lazy val SingletonClass        = newClass(ScalaPackageClass, tpnme.Singleton, anyparam, ABSTRACT | TRAIT | FINAL)
    lazy val SerializableClass     = getRequiredClass("scala.Serializable")
    lazy val JavaSerializableClass = getClass(sn.JavaSerializable)
    lazy val ComparableClass       = getRequiredClass("java.lang.Comparable")
    lazy val JavaCloneableClass    = getRequiredClass("java.lang.Cloneable")
    lazy val RemoteInterfaceClass  = getRequiredClass("java.rmi.Remote")
    lazy val RemoteExceptionClass  = getRequiredClass("java.rmi.RemoteException")

    lazy val ByNameParamClass       = specialPolyClass(tpnme.BYNAME_PARAM_CLASS_NAME, COVARIANT)(_ => AnyClass.typeConstructor)
    lazy val EqualsPatternClass     = specialPolyClass(tpnme.EQUALS_PATTERN_NAME, 0L)(_ => AnyClass.typeConstructor)
    lazy val JavaRepeatedParamClass = specialPolyClass(tpnme.JAVA_REPEATED_PARAM_CLASS_NAME, COVARIANT)(tparam => arrayType(tparam.typeConstructor))
    lazy val RepeatedParamClass     = specialPolyClass(tpnme.REPEATED_PARAM_CLASS_NAME, COVARIANT)(tparam => seqType(tparam.typeConstructor))

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

    lazy val MatchingStrategyClass = getRequiredClass("scala.MatchingStrategy")

    // collections classes
    lazy val ConsClass          = getRequiredClass("scala.collection.immutable.$colon$colon")
    lazy val IterableClass      = getRequiredClass("scala.collection.Iterable")
    lazy val IteratorClass      = getRequiredClass("scala.collection.Iterator")
    lazy val ListClass          = getRequiredClass("scala.collection.immutable.List")
    lazy val SeqClass           = getRequiredClass("scala.collection.Seq")
    lazy val StringBuilderClass = getRequiredClass("scala.collection.mutable.StringBuilder")
    lazy val TraversableClass   = getRequiredClass("scala.collection.Traversable")

    lazy val ListModule       = getRequiredModule("scala.collection.immutable.List")
      lazy val List_apply = getMember(ListModule, nme.apply)
    lazy val NilModule        = getRequiredModule("scala.collection.immutable.Nil")
    lazy val SeqModule        = getRequiredModule("scala.collection.Seq")
    lazy val IteratorModule = getRequiredModule("scala.collection.Iterator")
      lazy val Iterator_apply = getMember(IteratorModule, nme.apply)

    // arrays and their members
    lazy val ArrayModule  = getRequiredModule("scala.Array")
      lazy val ArrayModule_overloadedApply = getMember(ArrayModule, nme.apply)
    lazy val ArrayClass   = getRequiredClass("scala.Array")
      lazy val Array_apply  = getMember(ArrayClass, nme.apply)
      lazy val Array_update = getMember(ArrayClass, nme.update)
      lazy val Array_length = getMember(ArrayClass, nme.length)
      lazy val Array_clone  = getMember(ArrayClass, nme.clone_)

    // reflection / structural types
    lazy val SoftReferenceClass     = getRequiredClass("java.lang.ref.SoftReference")
    lazy val WeakReferenceClass     = getRequiredClass("java.lang.ref.WeakReference")
    lazy val MethodClass            = getClass(sn.MethodAsObject)
      def methodClass_setAccessible = getMember(MethodClass, nme.setAccessible)
    lazy val EmptyMethodCacheClass  = getRequiredClass("scala.runtime.EmptyMethodCache")
    lazy val MethodCacheClass       = getRequiredClass("scala.runtime.MethodCache")
      def methodCache_find  = getMember(MethodCacheClass, nme.find_)
      def methodCache_add   = getMember(MethodCacheClass, nme.add_)

    // scala.reflect
    lazy val ReflectApiUniverse = getRequiredClass("scala.reflect.api.Universe")
    lazy val ReflectMacroContext = getRequiredClass("scala.reflect.macro.Context")
    lazy val ReflectRuntimeMirror = getRequiredModule("scala.reflect.runtime.Mirror")
      def freeValueMethod = getMember(ReflectRuntimeMirror, nme.freeValue)
    lazy val ReflectPackage = getPackageObject("scala.reflect")
      def Reflect_mirror = getMember(ReflectPackage, nme.mirror)

    lazy val PartialManifestClass  = getRequiredClass("scala.reflect.ClassManifest")
    lazy val PartialManifestModule = getRequiredModule("scala.reflect.ClassManifest")
    lazy val FullManifestClass     = getRequiredClass("scala.reflect.Manifest")
    lazy val FullManifestModule    = getRequiredModule("scala.reflect.Manifest")
    lazy val OptManifestClass      = getRequiredClass("scala.reflect.OptManifest")
    lazy val NoManifest            = getRequiredModule("scala.reflect.NoManifest")

    lazy val ScalaSignatureAnnotation = getRequiredClass("scala.reflect.ScalaSignature")
    lazy val ScalaLongSignatureAnnotation = getRequiredClass("scala.reflect.ScalaLongSignature")

    // Option classes
    lazy val OptionClass: Symbol = getRequiredClass("scala.Option")
    lazy val SomeClass: Symbol   = getRequiredClass("scala.Some")
    lazy val NoneModule: Symbol  = getRequiredModule("scala.None")
    lazy val SomeModule: Symbol  = getRequiredModule("scala.Some")

    /** Note: don't use this manifest/type function for anything important,
     *  as it is incomplete.  Would love to have things like existential types
     *  working, but very unfortunately the manifests just stuff the relevant
     *  information into the toString method.
     */
    def manifestToType(m: OptManifest[_]): Type = m match {
      case m: ClassManifest[_] =>
        val sym  = manifestToSymbol(m)
        val args = m.typeArguments
      
        if ((sym eq NoSymbol) || args.isEmpty) sym.tpe
        else appliedType(sym.typeConstructor, args map manifestToType)
      case _ =>
        NoType
    }

    def manifestToSymbol(m: ClassManifest[_]): Symbol = m match {
      case x: scala.reflect.AnyValManifest[_] =>
        getMember(ScalaPackageClass, newTypeName("" + x))
      case _                                  => 
        val name = m.erasure.getName
        if (name endsWith nme.MODULE_SUFFIX_STRING)
          getModuleIfDefined(name stripSuffix nme.MODULE_SUFFIX_STRING)
        else
          getClassIfDefined(name)
    }

    // The given symbol represents either String.+ or StringAdd.+
    def isStringAddition(sym: Symbol) = sym == String_+ || sym == StringAdd_+
    def isArrowAssoc(sym: Symbol) = ArrowAssocClass.tpe.decls.toList contains sym

    // The given symbol is a method with the right name and signature to be a runnable java program.
    def isJavaMainMethod(sym: Symbol) = (sym.name == nme.main) && (sym.info match {
      case MethodType(p :: Nil, restpe) => isArrayOfSymbol(p.tpe, StringClass) && restpe.typeSymbol == UnitClass
      case _                            => false
    })
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
      val list = countFrom to arity map (i => getRequiredClass("scala." + name + i))
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
    def wrapArrayMethodName(elemtp: Type): TermName = elemtp.typeSymbol match {
      case ByteClass    => nme.wrapByteArray
      case ShortClass   => nme.wrapShortArray
      case CharClass    => nme.wrapCharArray
      case IntClass     => nme.wrapIntArray
      case LongClass    => nme.wrapLongArray
      case FloatClass   => nme.wrapFloatArray
      case DoubleClass  => nme.wrapDoubleArray
      case BooleanClass => nme.wrapBooleanArray
      case UnitClass    => nme.wrapUnitArray
      case _        => 
        if ((elemtp <:< AnyRefClass.tpe) && !isPhantomClass(elemtp.typeSymbol)) nme.wrapRefArray
        else nme.genericWrapArray
    }

    def tupleField(n: Int, j: Int) = getMember(TupleClass(n), nme.productAccessorName(j))
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

    lazy val ProductRootClass: Symbol = getRequiredClass("scala.Product")
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

    def seqType(arg: Type)       = appliedType(SeqClass.typeConstructor, List(arg))
    def arrayType(arg: Type)     = appliedType(ArrayClass.typeConstructor, List(arg))
    def byNameType(arg: Type)    = appliedType(ByNameParamClass.typeConstructor, List(arg))
    def iteratorOfType(tp: Type) = appliedType(IteratorClass.typeConstructor, List(tp))

    lazy val StringArray   = arrayType(StringClass.tpe)
    lazy val ObjectArray   = arrayType(ObjectClass.tpe)

    def ClassType(arg: Type) =
      if (phase.erasedTypes || forMSIL) ClassClass.tpe
      else appliedType(ClassClass.typeConstructor, List(arg))
    
    def vmClassType(arg: Type): Type = ClassType(arg)
    def vmSignature(sym: Symbol, info: Type): String = signature(info)    // !!!

    /** Given a class symbol C with type parameters T1, T2, ... Tn
     *  which have upper/lower bounds LB1/UB1, LB1/UB2, ..., LBn/UBn,
     *  returns an existential type of the form
     *
     *    C[E1, ..., En] forSome { E1 >: LB1 <: UB1 ... en >: LBn <: UBn }.
     */
    def classExistentialType(clazz: Symbol): Type =
      newExistentialType(clazz.typeParams, clazz.tpe)

    /** Given type U, creates a Type representing Class[_ <: U].
     */
    def boundedClassType(upperBound: Type) =
      appliedTypeAsUpperBounds(ClassClass.typeConstructor, List(upperBound))

    /** To avoid unchecked warnings on polymorphic classes, translate
     *  a Foo[T] into a Foo[_] for use in the pattern matcher.
     */
    @deprecated("Use classExistentialType", "2.10.0")
    def typeCaseType(clazz: Symbol): Type = classExistentialType(clazz)

    //
    // .NET backend
    //

    lazy val ComparatorClass = getRequiredClass("scala.runtime.Comparator")
    // System.ValueType
    lazy val ValueTypeClass: Symbol = getClass(sn.ValueType)
    // System.MulticastDelegate
    lazy val DelegateClass: Symbol = getClass(sn.Delegate)
    var Delegate_scalaCallers: List[Symbol] = List() // Syncnote: No protection necessary yet as only for .NET where reflection is not supported.
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
    lazy val Any_==       = newMethod(AnyClass, nme.EQ, anyparam, booltype, FINAL)
    lazy val Any_!=       = newMethod(AnyClass, nme.NE, anyparam, booltype, FINAL)
    lazy val Any_equals   = newMethod(AnyClass, nme.equals_, anyparam, booltype)
    lazy val Any_hashCode = newMethod(AnyClass, nme.hashCode_, Nil, inttype)
    lazy val Any_toString = newMethod(AnyClass, nme.toString_, Nil, stringtype)
    lazy val Any_##       = newMethod(AnyClass, nme.HASHHASH, Nil, inttype, FINAL)

    // Any_getClass requires special handling.  The return type is determined on
    // a per-call-site basis as if the function being called were actually:
    //
    //    // Assuming `target.getClass()`
    //    def getClass[T](target: T): Class[_ <: T]
    //
    // Since getClass is not actually a polymorphic method, this requires compiler
    // participation.  At the "Any" level, the return type is Class[_] as it is in
    // java.lang.Object.  Java also special cases the return type.
    lazy val Any_getClass     = newMethod(AnyClass, nme.getClass_, Nil, getMember(ObjectClass, nme.getClass_).tpe.resultType, DEFERRED)
    lazy val Any_isInstanceOf = newT1NullaryMethod(AnyClass, nme.isInstanceOf_, FINAL)(_ => booltype)
    lazy val Any_asInstanceOf = newT1NullaryMethod(AnyClass, nme.asInstanceOf_, FINAL)(_.typeConstructor)

    // members of class java.lang.{ Object, String }
    lazy val Object_## = newMethod(ObjectClass, nme.HASHHASH, Nil, inttype, FINAL)
    lazy val Object_== = newMethod(ObjectClass, nme.EQ, anyrefparam, booltype, FINAL)
    lazy val Object_!= = newMethod(ObjectClass, nme.NE, anyrefparam, booltype, FINAL)
    lazy val Object_eq = newMethod(ObjectClass, nme.eq, anyrefparam, booltype, FINAL)
    lazy val Object_ne = newMethod(ObjectClass, nme.ne, anyrefparam, booltype, FINAL)
    lazy val Object_isInstanceOf = newT1NoParamsMethod(ObjectClass, nme.isInstanceOf_Ob, FINAL | SYNTHETIC)(_ => booltype)
    lazy val Object_asInstanceOf = newT1NoParamsMethod(ObjectClass, nme.asInstanceOf_Ob, FINAL | SYNTHETIC)(_.typeConstructor)
    lazy val Object_synchronized = newPolyMethod(1, ObjectClass, nme.synchronized_, FINAL)(tps => 
      (Some(List(tps.head.typeConstructor)), tps.head.typeConstructor)
    )
    lazy val String_+ = newMethod(StringClass, nme.raw.PLUS, anyparam, stringtype, FINAL)

    def Object_getClass  = getMember(ObjectClass, nme.getClass_)
    def Object_clone     = getMember(ObjectClass, nme.clone_)
    def Object_finalize  = getMember(ObjectClass, nme.finalize_)
    def Object_notify    = getMember(ObjectClass, nme.notify_)
    def Object_notifyAll = getMember(ObjectClass, nme.notifyAll_)
    def Object_equals    = getMember(ObjectClass, nme.equals_)
    def Object_hashCode  = getMember(ObjectClass, nme.hashCode_)
    def Object_toString  = getMember(ObjectClass, nme.toString_)

    // boxed classes
    lazy val ObjectRefClass         = getRequiredClass("scala.runtime.ObjectRef")
    lazy val VolatileObjectRefClass = getRequiredClass("scala.runtime.VolatileObjectRef")
    lazy val BoxesRunTimeClass      = getRequiredModule("scala.runtime.BoxesRunTime")
    lazy val BoxedNumberClass       = getClass(sn.BoxedNumber)
    lazy val BoxedCharacterClass    = getClass(sn.BoxedCharacter)
    lazy val BoxedBooleanClass      = getClass(sn.BoxedBoolean)
    lazy val BoxedByteClass         = getRequiredClass("java.lang.Byte")
    lazy val BoxedShortClass        = getRequiredClass("java.lang.Short")
    lazy val BoxedIntClass          = getRequiredClass("java.lang.Integer")
    lazy val BoxedLongClass         = getRequiredClass("java.lang.Long")
    lazy val BoxedFloatClass        = getRequiredClass("java.lang.Float")
    lazy val BoxedDoubleClass       = getRequiredClass("java.lang.Double")

    lazy val BoxedUnitClass         = getRequiredClass("scala.runtime.BoxedUnit")
    lazy val BoxedUnitModule        = getRequiredModule("scala.runtime.BoxedUnit")
      def BoxedUnit_UNIT = getMember(BoxedUnitModule, nme.UNIT)
      def BoxedUnit_TYPE = getMember(BoxedUnitModule, nme.TYPE_)

    // Annotation base classes
    lazy val AnnotationClass            = getRequiredClass("scala.annotation.Annotation")
    lazy val ClassfileAnnotationClass   = getRequiredClass("scala.annotation.ClassfileAnnotation")
    lazy val StaticAnnotationClass      = getRequiredClass("scala.annotation.StaticAnnotation")

    // Annotations
    lazy val BridgeClass                = getRequiredClass("scala.annotation.bridge")
    lazy val ElidableMethodClass        = getRequiredClass("scala.annotation.elidable")
    lazy val ImplicitNotFoundClass      = getRequiredClass("scala.annotation.implicitNotFound")
    lazy val MigrationAnnotationClass   = getRequiredClass("scala.annotation.migration")
    lazy val ScalaStrictFPAttr          = getRequiredClass("scala.annotation.strictfp")
    lazy val SerializableAttr           = getRequiredClass("scala.annotation.serializable") // @serializable is deprecated
    lazy val SwitchClass                = getRequiredClass("scala.annotation.switch")
    lazy val TailrecClass               = getRequiredClass("scala.annotation.tailrec")
    lazy val VarargsClass               = getRequiredClass("scala.annotation.varargs")
    lazy val uncheckedStableClass       = getRequiredClass("scala.annotation.unchecked.uncheckedStable")
    lazy val uncheckedVarianceClass     = getRequiredClass("scala.annotation.unchecked.uncheckedVariance")

    lazy val BeanPropertyAttr           = getRequiredClass("scala.beans.BeanProperty")
    lazy val BooleanBeanPropertyAttr    = getRequiredClass("scala.beans.BooleanBeanProperty")
    lazy val CloneableAttr              = getRequiredClass("scala.cloneable")
    lazy val DeprecatedAttr             = getRequiredClass("scala.deprecated")
    lazy val DeprecatedNameAttr         = getRequiredClass("scala.deprecatedName")
    lazy val NativeAttr                 = getRequiredClass("scala.native")
    lazy val RemoteAttr                 = getRequiredClass("scala.remote")
    lazy val ScalaInlineClass           = getRequiredClass("scala.inline")
    lazy val ScalaNoInlineClass         = getRequiredClass("scala.noinline")
    lazy val SerialVersionUIDAttr       = getRequiredClass("scala.SerialVersionUID")
    lazy val SpecializedClass           = getRequiredClass("scala.specialized")
    lazy val ThrowsClass                = getRequiredClass("scala.throws")
    lazy val TransientAttr              = getRequiredClass("scala.transient")
    lazy val UncheckedClass             = getRequiredClass("scala.unchecked")
    lazy val VolatileAttr               = getRequiredClass("scala.volatile")

    // Meta-annotations
    lazy val BeanGetterTargetClass      = getMetaAnnotation("beanGetter")
    lazy val BeanSetterTargetClass      = getMetaAnnotation("beanSetter")
    lazy val FieldTargetClass           = getMetaAnnotation("field")
    lazy val GetterTargetClass          = getMetaAnnotation("getter")
    lazy val ParamTargetClass           = getMetaAnnotation("param")
    lazy val SetterTargetClass          = getMetaAnnotation("setter")
    // TODO: module, moduleClass? package, packageObject?

    private def getMetaAnnotation(name: String) = getRequiredClass("scala.annotation.meta." + name)
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

    def getPackageObjectClass(fullname: String): Symbol =
      getPackageObject(fullname).companionClass

    def getPackageObject(fullname: String): Symbol =
      getModule(newTermName(fullname)).info member nme.PACKAGE

    def getModule(fullname: Name): Symbol =
      getModuleOrClass(fullname.toTermName)

    def getClass(fullname: Name): Symbol = {
      var result = getModuleOrClass(fullname.toTypeName)
      while (result.isAliasType) result = result.info.typeSymbol
      result
    }
    
    def getRequiredModule(fullname: String): Symbol =
      getModule(newTermNameCached(fullname))
    def getRequiredClass(fullname: String): Symbol = 
      getClass(newTypeNameCached(fullname))

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

    private def newAlias(owner: Symbol, name: TypeName, alias: Type): Symbol =
      owner.newAliasType(name) setInfoAndEnter alias
    
    private def specialPolyClass(name: TypeName, flags: Long)(parentFn: Symbol => Type): Symbol = {
      val clazz   = newClass(ScalaPackageClass, name, Nil)
      val tparam  = clazz.newSyntheticTypeParam("T0", flags)
      val parents = List(AnyRefClass.tpe, parentFn(tparam))
      
      clazz setInfo polyType(List(tparam), ClassInfoType(parents, newScope, clazz))
    }
    
    def newPolyMethod(typeParamCount: Int, owner: Symbol, name: TermName, flags: Long)(createFn: PolyMethodCreator): Symbol = {
      val msym    = owner.newMethod(name.encode, NoPosition, flags)
      val tparams = msym.newSyntheticTypeParams(typeParamCount)
      val mtpe    = createFn(tparams) match {
        case (Some(formals), restpe) => MethodType(msym.newSyntheticValueParams(formals), restpe)
        case (_, restpe)             => NullaryMethodType(restpe)
      }

      msym setInfoAndEnter polyType(tparams, mtpe)
    }
    
    /** T1 means one type parameter.
     */
    def newT1NullaryMethod(owner: Symbol, name: TermName, flags: Long)(createFn: Symbol => Type): Symbol = {
      newPolyMethod(1, owner, name, flags)(tparams => (None, createFn(tparams.head)))
    }
    def newT1NoParamsMethod(owner: Symbol, name: TermName, flags: Long)(createFn: Symbol => Type): Symbol = {
      newPolyMethod(1, owner, name, flags)(tparams => (Some(Nil), createFn(tparams.head)))
    }

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
    def isValueClass(sym: Symbol)         = scalaValueClassesSet(sym)
    def isNonUnitValueClass(sym: Symbol)  = isValueClass(sym) && (sym != UnitClass)
    def isSpecializableClass(sym: Symbol) = isValueClass(sym) || (sym == AnyRefClass)
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
        Any_==,
        Any_!=,
        Any_equals,
        Any_hashCode,
        Any_toString,
        Any_getClass,
        Any_isInstanceOf,
        Any_asInstanceOf,
        Any_##,
        Object_eq,
        Object_ne,
        Object_==,
        Object_!=,
        Object_##,
        Object_synchronized,
        Object_isInstanceOf,
        Object_asInstanceOf,
        String_+
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
      val name = newTermName("$scalaCaller$$" + nbScalaCallers)
      // tparam => resultType, which is the resultType of PolyType, i.e. the result type after applying the
      // type parameter =-> a MethodType in this case
      // TODO: set type bounds manually (-> MulticastDelegate), see newTypeParam
      val newCaller = newMethod(DelegateClass, name, paramTypes, delegateType, FINAL | STATIC)
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
