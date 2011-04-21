/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package symtab

import scala.collection.{ mutable, immutable }
import scala.collection.mutable.{ HashMap }
import scala.tools.nsc.util.NoPosition
import Flags._
import PartialFunction._
import classfile.ClassfileConstants

trait Definitions extends reflect.generic.StandardDefinitions {
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

    def isNumericSubClass(sub: Symbol, sup: Symbol) = {
      val cmp = for (w1 <- numericWeight get sub ; w2 <- numericWeight get sup) yield w2 % w1
      cmp exists (_ == 0)
    }

    /** Is symbol a numeric value class? */
    def isNumericValueClass(sym: Symbol): Boolean =
      numericWeight contains sym

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
    lazy val RootClass: ModuleClassSymbol = NoSymbol.newModuleClass(NoPosition, tpnme.ROOT)
          .setFlag(FINAL | MODULE | PACKAGE | JAVA).setInfo(rootLoader)

    // The empty package, which holds all top level types without given packages.
    lazy val EmptyPackage       = RootClass.newPackage(NoPosition, nme.EMPTY_PACKAGE_NAME).setFlag(FINAL)
    lazy val EmptyPackageClass  = EmptyPackage.moduleClass

    lazy val JavaLangPackage    = getModule(sn.JavaLang)
    lazy val ScalaPackage       = getModule("scala")
    lazy val ScalaPackageClass  = ScalaPackage.tpe.typeSymbol

    lazy val RuntimePackage       = getModule("scala.runtime")
    lazy val RuntimePackageClass  = RuntimePackage.tpe.typeSymbol

    // convenient one-argument parameter lists
    lazy val anyparam     = List(AnyClass.typeConstructor)
    lazy val anyvalparam  = List(AnyValClass.typeConstructor)
    lazy val anyrefparam  = List(AnyRefClass.typeConstructor)

    // private parameter conveniences
    private def booltype    = BooleanClass.typeConstructor
    private def boolparam   = List(booltype)
    private def bytetype    = ByteClass.typeConstructor
    private def byteparam   = List(bytetype)
    private def shorttype   = ShortClass.typeConstructor
    private def shortparam  = List(shorttype)
    private def inttype     = IntClass.typeConstructor
    private def intparam    = List(inttype)
    private def longtype    = LongClass.typeConstructor
    private def longparam   = List(longtype)
    private def floattype   = FloatClass.typeConstructor
    private def floatparam  = List(floattype)
    private def doubletype  = DoubleClass.typeConstructor
    private def doubleparam = List(doubletype)
    private def chartype    = CharClass.typeConstructor
    private def charparam   = List(chartype)
    private def stringtype  = StringClass.typeConstructor

    // top types
    lazy val AnyClass             = newClass(ScalaPackageClass, tpnme.Any, Nil) setFlag (ABSTRACT)
    lazy val AnyRefClass          = newAlias(ScalaPackageClass, tpnme.AnyRef, ObjectClass.typeConstructor)
    lazy val ObjectClass          = getClass(sn.Object)
    lazy val AnyCompanionClass    = getClass("scala.AnyCompanion") setFlag (SEALED | ABSTRACT | TRAIT)
    lazy val AnyValCompanionClass = getClass("scala.AnyValCompanion") setFlag (SEALED | ABSTRACT | TRAIT)

    // bottom types
    lazy val NullClass            = newClass(ScalaPackageClass, tpnme.Null, anyrefparam) setFlag (ABSTRACT | TRAIT | FINAL)
    lazy val NothingClass         = newClass(ScalaPackageClass, tpnme.Nothing, anyparam) setFlag (ABSTRACT | TRAIT | FINAL)
    lazy val RuntimeNothingClass  = getClass(ClassfileConstants.SCALA_NOTHING)
    lazy val RuntimeNullClass     = getClass(ClassfileConstants.SCALA_NULL)

    // exceptions and other throwables
    lazy val ClassCastExceptionClass        = getClass("java.lang.ClassCastException")
    lazy val IndexOutOfBoundsExceptionClass = getClass(sn.IOOBException)
    lazy val InvocationTargetExceptionClass = getClass(sn.InvTargetException)
    lazy val MatchErrorClass                = getClass("scala.MatchError")
    lazy val NonLocalReturnControlClass     = getClass("scala.runtime.NonLocalReturnControl")
    lazy val NullPointerExceptionClass      = getClass(sn.NPException)
    lazy val ThrowableClass                 = getClass(sn.Throwable)
    lazy val UninitializedErrorClass        = getClass("scala.UninitializedFieldError")

    // annotations
    lazy val AnnotationClass            = getClass("scala.annotation.Annotation")
    lazy val ClassfileAnnotationClass   = getClass("scala.annotation.ClassfileAnnotation")
    lazy val StaticAnnotationClass      = getClass("scala.annotation.StaticAnnotation")
    lazy val uncheckedStableClass       = getClass("scala.annotation.unchecked.uncheckedStable")
    lazy val uncheckedVarianceClass     = getClass("scala.annotation.unchecked.uncheckedVariance")
    lazy val UncheckedClass             = getClass("scala.unchecked")
    lazy val ThrowsClass                = getClass("scala.throws")
    lazy val TailrecClass               = getClass("scala.annotation.tailrec")
    lazy val SwitchClass                = getClass("scala.annotation.switch")
    lazy val ElidableMethodClass        = getClass("scala.annotation.elidable")
    lazy val ImplicitNotFoundClass      = getClass("scala.annotation.implicitNotFound")
    lazy val VarargsClass               = getClass("scala.annotation.varargs")
    lazy val FieldTargetClass           = getClass("scala.annotation.target.field")
    lazy val GetterTargetClass          = getClass("scala.annotation.target.getter")
    lazy val SetterTargetClass          = getClass("scala.annotation.target.setter")
    lazy val BeanGetterTargetClass      = getClass("scala.annotation.target.beanGetter")
    lazy val BeanSetterTargetClass      = getClass("scala.annotation.target.beanSetter")
    lazy val ParamTargetClass           = getClass("scala.annotation.target.param")
    lazy val ScalaInlineClass           = getClass("scala.inline")
    lazy val ScalaNoInlineClass         = getClass("scala.noinline")
    lazy val SpecializedClass           = getClass("scala.specialized")
    lazy val BridgeClass                = getClass("scala.annotation.bridge")

    // fundamental reference classes
    lazy val ScalaObjectClass     = getClass("scala.ScalaObject")
    lazy val PartialFunctionClass = getClass("scala.PartialFunction")
    lazy val SymbolClass          = getClass("scala.Symbol")
    lazy val StringClass          = getClass(sn.String)
    lazy val StringModule         = StringClass.linkedClassOfClass
    lazy val ClassClass           = getClass(sn.Class)
      def Class_getMethod = getMember(ClassClass, nme.getMethod_)
    lazy val DynamicClass         = getClass("scala.Dynamic")

    // fundamental modules
    lazy val PredefModule: Symbol = getModule("scala.Predef")
    lazy val PredefModuleClass = PredefModule.tpe.typeSymbol
      def Predef_AnyRef = getMember(PredefModule, "AnyRef") // used by the specialization annotation
      def Predef_classOf = getMember(PredefModule, nme.classOf)
      def Predef_error    = getMember(PredefModule, nme.error)
      def Predef_identity = getMember(PredefModule, nme.identity)
      def Predef_conforms = getMember(PredefModule, nme.conforms)
      def Predef_wrapRefArray = getMember(PredefModule, nme.wrapRefArray)
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
      def scalaRuntimeHash = getMember(ScalaRunTimeModule, "hash")
      def scalaRuntimeSameElements = getMember(ScalaRunTimeModule, nme.sameElements)

    // classes with special meanings
    lazy val NotNullClass     = getClass("scala.NotNull")
    lazy val DelayedInitClass = getClass("scala.DelayedInit")
      def delayedInitMethod = getMember(DelayedInitClass, nme.delayedInit)
      // a dummy value that communicates that a delayedInit call is compiler-generated
      // from phase UnCurry to phase Constructors
      def delayedInitArgVal = EmptyPackageClass.newValue(NoPosition, nme.delayedInitArg)
        .setInfo(UnitClass.tpe)

    lazy val TypeConstraintClass   = getClass("scala.annotation.TypeConstraint")
    lazy val SingletonClass        = newClass(ScalaPackageClass, tpnme.Singleton, anyparam) setFlag (ABSTRACT | TRAIT | FINAL)
    lazy val SerializableClass     = getClass("scala.Serializable")
    lazy val JavaSerializableClass = getClass(sn.JavaSerializable)
    lazy val ComparableClass       = getClass("java.lang.Comparable")

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

    def isScalaVarArgs(params: List[Symbol]) = params.nonEmpty && isScalaRepeatedParamType(params.last.tpe)
    def isVarArgsList(params: List[Symbol])  = params.nonEmpty && isRepeatedParamType(params.last.tpe)
    def isVarArgTypes(formals: List[Type])   = formals.nonEmpty && isRepeatedParamType(formals.last)

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
      def List_apply = getMember(ListModule, nme.apply)
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

    // invoke dynamic support
    lazy val LinkageModule = getModule("java.dyn.Linkage")
      lazy val Linkage_invalidateCallerClass = getMember(LinkageModule, "invalidateCallerClass")
    lazy val DynamicDispatchClass = getModule("scala.runtime.DynamicDispatch")
      lazy val DynamicDispatch_DontSetTarget = getMember(DynamicDispatchClass, "DontSetTarget")

    // Option classes
    lazy val OptionClass: Symbol = getClass("scala.Option")
    lazy val SomeClass: Symbol   = getClass("scala.Some")
    lazy val NoneModule: Symbol  = getModule("scala.None")

    def isOptionType(tp: Type)  = cond(tp.normalize) { case TypeRef(_, OptionClass, List(_)) => true }
    def isSomeType(tp: Type)    = cond(tp.normalize) { case TypeRef(_,   SomeClass, List(_)) => true }
    def isNoneType(tp: Type)    = cond(tp.normalize) { case TypeRef(_,   NoneModule, List(_)) => true }

    def optionType(tp: Type)    = typeRef(OptionClass.typeConstructor.prefix, OptionClass, List(tp))
    def someType(tp: Type)      = typeRef(SomeClass.typeConstructor.prefix, SomeClass, List(tp))
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
      def Product_productPrefix = getMember(ProductRootClass, nme.productPrefix)
      def Product_canEqual = getMember(ProductRootClass, nme.canEqual_)

      def productProj(z:Symbol, j: Int): Symbol = getMember(z, nme.productAccessorName(j))
      def productProj(n: Int,   j: Int): Symbol = productProj(ProductClass(n), j)

      /** returns true if this type is exactly ProductN[T1,...,Tn], not some subclass */
      def isExactProductType(tp: Type): Boolean = cond(tp.normalize) {
        case TypeRef(_, sym, elems) =>
          val len = elems.length
          len <= MaxProductArity && sym == ProductClass(len)
      }

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

    /** if tpe <: ProductN[T1,...,TN], returns Some(T1,...,TN) else None */
    def getProductArgs(tpe: Type): Option[List[Type]] =
      tpe.baseClasses collectFirst { case x if isExactProductType(x.tpe) => tpe.baseType(x).typeArgs }

    def unapplyUnwrap(tpe:Type) = (tpe match {
      case PolyType(_,MethodType(_, res)) => res
      case MethodType(_, res)             => res
      case tpe                            => tpe
    }).normalize

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

    def seqType(arg: Type)    = typeRef(SeqClass.typeConstructor.prefix, SeqClass, List(arg))
    def arrayType(arg: Type)  = typeRef(ArrayClass.typeConstructor.prefix, ArrayClass, List(arg))
    def byNameType(arg: Type) = appliedType(ByNameParamClass.typeConstructor, List(arg))

    def ClassType(arg: Type) =
      if (phase.erasedTypes || forMSIL) ClassClass.tpe
      else appliedType(ClassClass.tpe, List(arg))

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
    lazy val Delegate_scalaCallerTargets: HashMap[Symbol, Symbol] = new HashMap()

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

    // special attributes
    lazy val BeanPropertyAttr: Symbol           = getClass(sn.BeanProperty)
    lazy val BooleanBeanPropertyAttr: Symbol    = getClass(sn.BooleanBeanProperty)
    lazy val CloneableAttr: Symbol              = getClass("scala.cloneable")
    lazy val DeprecatedAttr: Symbol             = getClass("scala.deprecated")
    lazy val DeprecatedNameAttr: Symbol         = getClass("scala.deprecatedName")
    lazy val MigrationAnnotationClass: Symbol   = getClass("scala.annotation.migration")
    lazy val NativeAttr: Symbol                 = getClass("scala.native")
    lazy val RemoteAttr: Symbol                 = getClass("scala.remote")
    lazy val ScalaNumberClass: Symbol           = getClass("scala.math.ScalaNumber")
    lazy val ScalaStrictFPAttr: Symbol          = getClass("scala.annotation.strictfp")
    lazy val SerialVersionUIDAttr: Symbol       = getClass("scala.SerialVersionUID")
    lazy val SerializableAttr: Symbol           = getClass("scala.annotation.serializable") // @serializable is deprecated
    lazy val TraitSetterAnnotationClass: Symbol = getClass("scala.runtime.TraitSetter")
    lazy val TransientAttr: Symbol              = getClass("scala.transient")
    lazy val VolatileAttr: Symbol               = getClass("scala.volatile")

    lazy val AnnotationDefaultAttr: Symbol = {
      val attr = newClass(RuntimePackageClass, tpnme.AnnotationDefaultATTR, List(AnnotationClass.typeConstructor))
      // This attribute needs a constructor so that modifiers in parsed Java code make sense
      attr.info.decls enter (attr newConstructor NoPosition setInfo MethodType(Nil, attr.tpe))
      attr
    }

    def getModule(fullname: Name): Symbol =
      getModuleOrClass(fullname.toTermName)

    def getClass(fullname: Name): Symbol = {
      var result = getModuleOrClass(fullname.toTypeName)
      while (result.isAliasType) result = result.info.typeSymbol
      result
    }

    def getClassIfDefined(fullname: Name): Symbol =
      try {
        getClass(fullname)
      } catch {
        case ex: MissingRequirementError => NoSymbol
      }

    def getMember(owner: Symbol, name: Name): Symbol = {
      if (owner == NoSymbol) NoSymbol
      else owner.info.nonPrivateMember(name) match {
        case NoSymbol => throw new FatalError(owner + " does not have a member " + name)
        case result   => result
      }
    }
    def packageExists(packageName: String): Boolean = {
      try getModuleOrClass(newTermName(packageName)).isPackage
      catch { case _: MissingRequirementError => false }
    }

    /** If you're looking for a class, pass a type name.
     *  If a module, a term name.
     */
    private def getModuleOrClass(path: Name): Symbol = {
      val module   = path.isTermName
      val fullname = path.toTermName
      if (fullname == nme.NO_NAME)
        return NoSymbol

      var sym: Symbol = RootClass
      var i = 0
      var j = fullname.pos('.', i)
      while (j < fullname.length) {
        sym = sym.info.member(fullname.subName(i, j))
        i = j + 1
        j = fullname.pos('.', i)
      }
      val result =
        if (module) sym.info.member(fullname.subName(i, j)).suchThat(_ hasFlag MODULE)
        else sym.info.member(fullname.subName(i, j).toTypeName)
      if (result == NoSymbol) {
        if (settings.debug.value)
          { log(sym.info); log(sym.info.members) }//debug
        throw new MissingRequirementError((if (module) "object " else "class ") + fullname)
      }
      result
    }

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

    private lazy val scalaValueClassesSet = ScalaValueClasses.toSet
    private lazy val boxedValueClassesSet = boxedClass.values.toSet + BoxedUnitClass

    /** Is symbol a value class? */
    def isValueClass(sym: Symbol) = scalaValueClassesSet(sym)
    def isNonUnitValueClass(sym: Symbol) = (sym != UnitClass) && isValueClass(sym)

    /** Is symbol a boxed value class, e.g. java.lang.Integer? */
    def isBoxedValueClass(sym: Symbol) = boxedValueClassesSet(sym)

    /** If symbol is a value class, return the value class, with the exception
     *  that BoxedUnit remains BoxedUnit.  If not a value class, NoSymbol.
     */
    def unboxedValueClass(sym: Symbol): Symbol =
      if (isValueClass(sym)) sym
      else if (sym == BoxedUnitClass) sym
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
        else if (sym.owner.isPackageClass) sym.fullName('.') + (if (sym.isModuleClass) "$" else "")
        else flatNameString(sym.owner, separator) + "$" + sym.simpleName;
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

    def init {
      if (isInitialized) return

      EmptyPackageClass setInfo ClassInfoType(Nil, new Scope, EmptyPackageClass)
      EmptyPackage setInfo EmptyPackageClass.tpe

      RootClass.info.decls enter EmptyPackage
      RootClass.info.decls enter RootPackage

      // members of class scala.Any
      Any_== = newMethod(AnyClass, nme.EQ, anyparam, booltype) setFlag FINAL
      Any_!= = newMethod(AnyClass, nme.NE, anyparam, booltype) setFlag FINAL
      Any_equals = newMethod(AnyClass, nme.equals_, anyparam, booltype)
      Any_hashCode = newMethod(AnyClass, nme.hashCode_, Nil, inttype)
      Any_toString = newMethod(AnyClass, nme.toString_, Nil, stringtype)
      Any_## = newMethod(AnyClass, nme.HASHHASH, Nil, inttype) setFlag FINAL

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
