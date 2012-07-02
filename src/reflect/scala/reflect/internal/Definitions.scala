/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import annotation.{ switch, meta }
import scala.collection.{ mutable, immutable }
import Flags._
import PartialFunction._
import scala.reflect.base.{Universe => BaseUniverse}

trait Definitions extends api.StandardDefinitions {
  self: SymbolTable =>

  import rootMirror.{getModule, getClassByName, getRequiredClass, getRequiredModule, getRequiredPackage, getClassIfDefined, getModuleIfDefined, getPackageObject, getPackageObjectIfDefined, requiredClass, requiredModule}

  object definitions extends DefinitionsClass

  // [Eugene] find a way to make these non-lazy
  lazy val ByteTpe    = definitions.ByteClass.asType
  lazy val ShortTpe   = definitions.ShortClass.asType
  lazy val CharTpe    = definitions.CharClass.asType
  lazy val IntTpe     = definitions.IntClass.asType
  lazy val LongTpe    = definitions.LongClass.asType
  lazy val FloatTpe   = definitions.FloatClass.asType
  lazy val DoubleTpe  = definitions.DoubleClass.asType
  lazy val BooleanTpe = definitions.BooleanClass.asType
  lazy val UnitTpe    = definitions.UnitClass.asType
  lazy val AnyTpe     = definitions.AnyClass.asType
  lazy val ObjectTpe  = definitions.ObjectClass.asType
  lazy val AnyValTpe  = definitions.AnyValClass.asType
  lazy val AnyRefTpe  = definitions.AnyRefClass.asType
  lazy val NothingTpe = definitions.NothingClass.asType
  lazy val NullTpe    = definitions.NullClass.asType

  /** Since both the value parameter types and the result type may
   *  require access to the type parameter symbols, we model polymorphic
   *  creation as a function from those symbols to (formal types, result type).
   *  The Option is to distinguish between nullary methods and empty-param-list
   *  methods.
   */
  private type PolyMethodCreator = List[Symbol] => (Option[List[Type]], Type)

  private def enterNewClass(owner: Symbol, name: TypeName, parents: List[Type], flags: Long = 0L): ClassSymbol = {
    val clazz = owner.newClassSymbol(name, NoPosition, flags)
    clazz setInfoAndEnter ClassInfoType(parents, newScope, clazz)
  }
  private def newMethod(owner: Symbol, name: TermName, formals: List[Type], restpe: Type, flags: Long = 0L): MethodSymbol = {
    val msym   = owner.newMethod(name.encode, NoPosition, flags)
    val params = msym.newSyntheticValueParams(formals)
    msym setInfo MethodType(params, restpe)
  }
  private def enterNewMethod(owner: Symbol, name: TermName, formals: List[Type], restpe: Type, flags: Long = 0L): MethodSymbol =
    owner.info.decls enter newMethod(owner, name, formals, restpe, flags)

  // the scala value classes
  trait ValueClassDefinitions {
    self: DefinitionsClass =>

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
      tpnme.Unit    -> VOID_TAG
    )

    private def catastrophicFailure() =
      abort("Could not find value classes! This is a catastrophic failure.  scala " +
        scala.util.Properties.versionString)

    private def valueClassSymbol(name: TypeName): ClassSymbol = {
      getMember(ScalaPackageClass, name) match {
        case x: ClassSymbol => x
        case _              => catastrophicFailure()
      }
    }
    private def valueClassCompanion(name: TermName): ModuleSymbol = {
      getMember(ScalaPackageClass, name) match {
        case x: ModuleSymbol => x
        case _               => catastrophicFailure()
      }
    }
    private def valueCompanionMember(className: Name, methodName: TermName): TermSymbol =
      getMemberMethod(valueClassCompanion(className.toTermName).moduleClass, methodName)

    private def classesMap[T](f: Name => T) = symbolsMap(ScalaValueClassesNoUnit, f)
    private def symbolsMap[T](syms: List[Symbol], f: Name => T): Map[Symbol, T] = mapFrom(syms)(x => f(x.name))
    private def symbolsMapFilt[T](syms: List[Symbol], p: Name => Boolean, f: Name => T) = symbolsMap(syms filter (x => p(x.name)), f)

    private def boxedName(name: Name) = sn.Boxed(name.toTypeName)

    lazy val abbrvTag         = symbolsMap(ScalaValueClasses, nameToTag) withDefaultValue OBJECT_TAG
    lazy val numericWeight    = symbolsMapFilt(ScalaValueClasses, nameToWeight.keySet, nameToWeight)
    lazy val boxedModule      = classesMap(x => getModule(boxedName(x)))
    lazy val boxedClass       = classesMap(x => getClassByName(boxedName(x)))
    lazy val refClass         = classesMap(x => getRequiredClass("scala.runtime." + x + "Ref"))
    lazy val volatileRefClass = classesMap(x => getRequiredClass("scala.runtime.Volatile" + x + "Ref"))
    lazy val boxMethod        = classesMap(x => valueCompanionMember(x, nme.box))
    lazy val unboxMethod      = classesMap(x => valueCompanionMember(x, nme.unbox))

    def isNumericSubClass(sub: Symbol, sup: Symbol) = (
         (numericWeight contains sub)
      && (numericWeight contains sup)
      && (numericWeight(sup) % numericWeight(sub) == 0)
    )

    /** Is symbol a numeric value class? */
    def isNumericValueClass(sym: Symbol) = ScalaNumericValueClasses contains sym

    def isGetClass(sym: Symbol) =
      (sym.name == nme.getClass_) && flattensToEmpty(sym.paramss)

    lazy val UnitClass    = valueClassSymbol(tpnme.Unit)
    lazy val ByteClass    = valueClassSymbol(tpnme.Byte)
    lazy val ShortClass   = valueClassSymbol(tpnme.Short)
    lazy val CharClass    = valueClassSymbol(tpnme.Char)
    lazy val IntClass     = valueClassSymbol(tpnme.Int)
    lazy val LongClass    = valueClassSymbol(tpnme.Long)
    lazy val FloatClass   = valueClassSymbol(tpnme.Float)
    lazy val DoubleClass  = valueClassSymbol(tpnme.Double)
    lazy val BooleanClass = valueClassSymbol(tpnme.Boolean)
      lazy val Boolean_and = getMemberMethod(BooleanClass, nme.ZAND)
      lazy val Boolean_or  = getMemberMethod(BooleanClass, nme.ZOR)
      lazy val Boolean_not = getMemberMethod(BooleanClass, nme.UNARY_!)

    lazy val ScalaNumericValueClasses = ScalaValueClasses filterNot Set[Symbol](UnitClass, BooleanClass)

    def ScalaValueClassesNoUnit = ScalaValueClasses filterNot (_ eq UnitClass)
    def ScalaValueClasses: List[ClassSymbol] = List(
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
    def ScalaPrimitiveValueClasses: List[ClassSymbol] = ScalaValueClasses
  }

  abstract class DefinitionsClass extends DefinitionsApi with ValueClassDefinitions {
    private var isInitialized = false
    def isDefinitionsInitialized = isInitialized

    // symbols related to packages
    var emptypackagescope: Scope = null //debug

    @deprecated("Moved to rootMirror.RootPackage", "2.10.0")
    val RootPackage: ModuleSymbol = rootMirror.RootPackage

    @deprecated("Moved to rootMirror.RootClass", "2.10.0")
    val RootClass: ClassSymbol = rootMirror.RootClass

    @deprecated("Moved to rootMirror.EmptyPackage", "2.10.0")
    val EmptyPackage: ModuleSymbol = rootMirror.EmptyPackage

    @deprecated("Moved to rootMirror.EmptyPackageClass", "2.10.0")
    val EmptyPackageClass: ClassSymbol = rootMirror.EmptyPackageClass

    // It becomes tricky to create dedicated objects for other symbols because
    // of initialization order issues.
    lazy val JavaLangPackage      = getRequiredPackage(sn.JavaLang)
    lazy val JavaLangPackageClass = JavaLangPackage.moduleClass.asClassSymbol
    lazy val ScalaPackage         = getRequiredPackage(nme.scala_)
    lazy val ScalaPackageClass    = ScalaPackage.moduleClass.asClassSymbol
    lazy val RuntimePackage       = getRequiredPackage("scala.runtime")
    lazy val RuntimePackageClass  = RuntimePackage.moduleClass.asClassSymbol

    lazy val JavaLangEnumClass = requiredClass[java.lang.Enum[_]]

    // convenient one-argument parameter lists
    lazy val anyparam     = List(AnyClass.tpe)
    lazy val anyvalparam  = List(AnyValClass.typeConstructor)
    lazy val anyrefparam  = List(AnyRefClass.typeConstructor)

    // private parameter conveniences
    private def booltype    = BooleanClass.tpe
    private def inttype     = IntClass.tpe
    private def stringtype  = StringClass.tpe

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

    private def fixupAsAnyTrait(tpe: Type): Type = tpe match {
      case ClassInfoType(parents, decls, clazz) =>
        if (parents.head.typeSymbol == AnyClass) tpe
        else {
          assert(parents.head.typeSymbol == ObjectClass, parents)
          ClassInfoType(AnyClass.tpe :: parents.tail, decls, clazz)
        }
      case PolyType(tparams, restpe) =>
        PolyType(tparams, fixupAsAnyTrait(restpe))
//      case _ => tpe
    }

    // top types
    lazy val AnyClass    = enterNewClass(ScalaPackageClass, tpnme.Any, Nil, ABSTRACT)
    lazy val AnyRefClass = newAlias(ScalaPackageClass, tpnme.AnyRef, ObjectClass.tpe)
    lazy val ObjectClass = getRequiredClass(sn.Object.toString)

    // Note: this is not the type alias AnyRef, it's a companion-like
    // object used by the @specialize annotation.
    lazy val AnyRefModule = getMemberModule(ScalaPackageClass, nme.AnyRef)
    @deprecated("Use AnyRefModule", "2.10.0")
    def Predef_AnyRef = AnyRefModule

    lazy val AnyValClass: ClassSymbol = (ScalaPackageClass.info member tpnme.AnyVal orElse {
      val anyval    = enterNewClass(ScalaPackageClass, tpnme.AnyVal, List(AnyClass.tpe, NotNullClass.tpe), ABSTRACT)
      val av_constr = anyval.newClassConstructor(NoPosition)
      anyval.info.decls enter av_constr
      anyval
    }).asInstanceOf[ClassSymbol]

    // bottom types
    lazy val RuntimeNothingClass  = getClassByName(fulltpnme.RuntimeNothing)
    lazy val RuntimeNullClass     = getClassByName(fulltpnme.RuntimeNull)

    sealed abstract class BottomClassSymbol(name: TypeName, parent: Symbol) extends ClassSymbol(ScalaPackageClass, NoPosition, name) {
      locally {
        this initFlags ABSTRACT | FINAL
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
    lazy val ClassCastExceptionClass        = requiredClass[ClassCastException]
    lazy val IndexOutOfBoundsExceptionClass = getClassByName(sn.IOOBException)
    lazy val InvocationTargetExceptionClass = getClassByName(sn.InvTargetException)
    lazy val MatchErrorClass                = requiredClass[MatchError]
    lazy val NonLocalReturnControlClass     = requiredClass[scala.runtime.NonLocalReturnControl[_]]
    lazy val NullPointerExceptionClass      = getClassByName(sn.NPException)
    lazy val ThrowableClass                 = getClassByName(sn.Throwable)
    lazy val UninitializedErrorClass        = requiredClass[UninitializedFieldError]

    // fundamental reference classes
    lazy val PartialFunctionClass       = requiredClass[PartialFunction[_,_]]
    lazy val AbstractPartialFunctionClass = requiredClass[scala.runtime.AbstractPartialFunction[_,_]]
    lazy val SymbolClass                = requiredClass[scala.Symbol]
    lazy val StringClass                = requiredClass[java.lang.String]
    lazy val StringModule               = StringClass.linkedClassOfClass
    lazy val ClassClass                 = requiredClass[java.lang.Class[_]]
      def Class_getMethod               = getMemberMethod(ClassClass, nme.getMethod_)
    lazy val DynamicClass               = requiredClass[Dynamic]

    // fundamental modules
    lazy val SysPackage = getPackageObject("scala.sys")
      def Sys_error    = getMemberMethod(SysPackage, nme.error)

    // Modules whose members are in the default namespace
    // [Eugene++] ScalaPackage and JavaLangPackage are never ever shared between mirrors
    // as a result, `Int` becomes `scala.Int` and `String` becomes `java.lang.String`
    // I could just change `isOmittablePrefix`, but there's more to it, so I'm leaving this as a todo for now
    lazy val UnqualifiedModules = List(PredefModule, ScalaPackage, JavaLangPackage)
    // Those modules and their module classes
    lazy val UnqualifiedOwners  = UnqualifiedModules.toSet ++ UnqualifiedModules.map(_.moduleClass)

    lazy val PredefModule      = requiredModule[scala.Predef.type]
    lazy val PredefModuleClass = PredefModule.moduleClass

      def Predef_classOf      = getMemberMethod(PredefModule, nme.classOf)
      def Predef_identity     = getMemberMethod(PredefModule, nme.identity)
      def Predef_conforms     = getMemberMethod(PredefModule, nme.conforms)
      def Predef_wrapRefArray = getMemberMethod(PredefModule, nme.wrapRefArray)
      def Predef_???          = getMemberMethod(PredefModule, nme.???)
      def Predef_implicitly   = getMemberMethod(PredefModule, nme.implicitly)

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
    lazy val SpecializableModule  = requiredModule[Specializable]
    lazy val GroupOfSpecializable = getMemberClass(SpecializableModule, tpnme.Group)

    lazy val ConsoleModule      = requiredModule[scala.Console.type]
    lazy val ScalaRunTimeModule = requiredModule[scala.runtime.ScalaRunTime.type]
    lazy val SymbolModule       = requiredModule[scala.Symbol.type]
    lazy val Symbol_apply       = getMemberMethod(SymbolModule, nme.apply)

      def SeqFactory = getMember(ScalaRunTimeModule, nme.Seq) // [Eugene++] obsolete?
      def arrayApplyMethod = getMemberMethod(ScalaRunTimeModule, nme.array_apply)
      def arrayUpdateMethod = getMemberMethod(ScalaRunTimeModule, nme.array_update)
      def arrayLengthMethod = getMemberMethod(ScalaRunTimeModule, nme.array_length)
      def arrayCloneMethod = getMemberMethod(ScalaRunTimeModule, nme.array_clone)
      def ensureAccessibleMethod = getMemberMethod(ScalaRunTimeModule, nme.ensureAccessible)
      def scalaRuntimeSameElements = getMemberMethod(ScalaRunTimeModule, nme.sameElements)
      def arrayClassMethod = getMemberMethod(ScalaRunTimeModule, nme.arrayClass)
      def arrayElementClassMethod = getMemberMethod(ScalaRunTimeModule, nme.arrayElementClass)

    // classes with special meanings
    lazy val StringAddClass             = requiredClass[scala.runtime.StringAdd]
    lazy val ArrowAssocClass            = getRequiredClass("scala.Predef.ArrowAssoc") // SI-5731
    lazy val StringAdd_+                = getMemberMethod(StringAddClass, nme.PLUS)
    lazy val NotNullClass               = getRequiredClass("scala.NotNull")
    lazy val ScalaNumberClass           = requiredClass[scala.math.ScalaNumber]
    lazy val TraitSetterAnnotationClass = requiredClass[scala.runtime.TraitSetter]
    lazy val DelayedInitClass           = requiredClass[scala.DelayedInit]
      def delayedInitMethod = getMemberMethod(DelayedInitClass, nme.delayedInit)
      // a dummy value that communicates that a delayedInit call is compiler-generated
      // from phase UnCurry to phase Constructors
      // !!! This is not used anywhere (it was checked in that way.)
      // def delayedInitArgVal = EmptyPackageClass.newValue(NoPosition, nme.delayedInitArg)
      //   .setInfo(UnitClass.tpe)

    lazy val TypeConstraintClass   = requiredClass[scala.annotation.TypeConstraint]
    lazy val SingletonClass        = enterNewClass(ScalaPackageClass, tpnme.Singleton, anyparam, ABSTRACT | TRAIT | FINAL)
    lazy val SerializableClass     = requiredClass[scala.Serializable]
    lazy val JavaSerializableClass = requiredClass[java.io.Serializable] modifyInfo fixupAsAnyTrait
    lazy val ComparableClass       = requiredClass[java.lang.Comparable[_]] modifyInfo fixupAsAnyTrait
    lazy val JavaCloneableClass    = requiredClass[java.lang.Cloneable]
    lazy val JavaNumberClass       = requiredClass[java.lang.Number]
    lazy val RemoteInterfaceClass  = requiredClass[java.rmi.Remote]
    lazy val RemoteExceptionClass  = requiredClass[java.rmi.RemoteException]

    lazy val ByNameParamClass       = specialPolyClass(tpnme.BYNAME_PARAM_CLASS_NAME, COVARIANT)(_ => AnyClass.tpe)
    lazy val EqualsPatternClass     = specialPolyClass(tpnme.EQUALS_PATTERN_NAME, 0L)(_ => AnyClass.tpe)
    lazy val JavaRepeatedParamClass = specialPolyClass(tpnme.JAVA_REPEATED_PARAM_CLASS_NAME, COVARIANT)(tparam => arrayType(tparam.tpe))
    lazy val RepeatedParamClass     = specialPolyClass(tpnme.REPEATED_PARAM_CLASS_NAME, COVARIANT)(tparam => seqType(tparam.tpe))

    def isByNameParamType(tp: Type)        = tp.typeSymbol == ByNameParamClass
    def isScalaRepeatedParamType(tp: Type) = tp.typeSymbol == RepeatedParamClass
    def isJavaRepeatedParamType(tp: Type)  = tp.typeSymbol == JavaRepeatedParamClass
    def isRepeatedParamType(tp: Type)      = isScalaRepeatedParamType(tp) || isJavaRepeatedParamType(tp)
    def isCastSymbol(sym: Symbol)          = sym == Any_asInstanceOf || sym == Object_asInstanceOf

    def isJavaVarArgsMethod(m: Symbol)       = m.isMethod && isJavaVarArgs(m.info.params)
    def isJavaVarArgs(params: Seq[Symbol])  = params.nonEmpty && isJavaRepeatedParamType(params.last.tpe)
    def isScalaVarArgs(params: Seq[Symbol]) = params.nonEmpty && isScalaRepeatedParamType(params.last.tpe)
    def isVarArgsList(params: Seq[Symbol])  = params.nonEmpty && isRepeatedParamType(params.last.tpe)
    def isVarArgTypes(formals: Seq[Type])   = formals.nonEmpty && isRepeatedParamType(formals.last)

    def hasRepeatedParam(tp: Type): Boolean = tp match {
      case MethodType(formals, restpe) => isScalaVarArgs(formals) || hasRepeatedParam(restpe)
      case PolyType(_, restpe)         => hasRepeatedParam(restpe)
      case _                           => false
    }

    def isPrimitiveArray(tp: Type) = tp match {
      case TypeRef(_, ArrayClass, arg :: Nil) => isPrimitiveValueClass(arg.typeSymbol)
      case _                                  => false
    }
    def isReferenceArray(tp: Type) = tp match {
      case TypeRef(_, ArrayClass, arg :: Nil) => arg <:< AnyRefClass.tpe
      case _                                  => false
    }
    def isArrayOfSymbol(tp: Type, elem: Symbol) = tp match {
      case TypeRef(_, ArrayClass, arg :: Nil) => arg.typeSymbol == elem
      case _                                  => false
    }

    lazy val MatchingStrategyClass = getRequiredClass("scala.MatchingStrategy")

    // collections classes
    lazy val ConsClass          = requiredClass[scala.collection.immutable.::[_]]
    lazy val IterableClass      = requiredClass[scala.collection.Iterable[_]]
    lazy val IteratorClass      = requiredClass[scala.collection.Iterator[_]]
    lazy val ListClass          = requiredClass[scala.collection.immutable.List[_]]
    lazy val SeqClass           = requiredClass[scala.collection.Seq[_]]
    lazy val StringBuilderClass = requiredClass[scala.collection.mutable.StringBuilder]
    lazy val TraversableClass   = requiredClass[scala.collection.Traversable[_]]

    lazy val ListModule       = requiredModule[scala.collection.immutable.List.type]
      lazy val List_apply     = getMemberMethod(ListModule, nme.apply)
    lazy val NilModule        = requiredModule[scala.collection.immutable.Nil.type]
    lazy val SeqModule        = requiredModule[scala.collection.Seq.type]
    lazy val IteratorModule   = requiredModule[scala.collection.Iterator.type]
      lazy val Iterator_apply = getMemberMethod(IteratorModule, nme.apply)

    // arrays and their members
    lazy val ArrayModule                   = requiredModule[scala.Array.type]
      lazy val ArrayModule_overloadedApply = getMemberMethod(ArrayModule, nme.apply)
    lazy val ArrayClass                    = getRequiredClass("scala.Array") // requiredClass[scala.Array[_]]
      lazy val Array_apply                 = getMemberMethod(ArrayClass, nme.apply)
      lazy val Array_update                = getMemberMethod(ArrayClass, nme.update)
      lazy val Array_length                = getMemberMethod(ArrayClass, nme.length)
      lazy val Array_clone                 = getMemberMethod(ArrayClass, nme.clone_)

    // reflection / structural types
    lazy val SoftReferenceClass     = requiredClass[java.lang.ref.SoftReference[_]]
    lazy val WeakReferenceClass     = requiredClass[java.lang.ref.WeakReference[_]]
    lazy val MethodClass            = getClassByName(sn.MethodAsObject)
      def methodClass_setAccessible = getMemberMethod(MethodClass, nme.setAccessible)
    lazy val EmptyMethodCacheClass  = requiredClass[scala.runtime.EmptyMethodCache]
    lazy val MethodCacheClass       = requiredClass[scala.runtime.MethodCache]
      def methodCache_find          = getMemberMethod(MethodCacheClass, nme.find_)
      def methodCache_add           = getMemberMethod(MethodCacheClass, nme.add_)

    // scala.reflect
    lazy val ReflectPackage              = requiredModule[scala.reflect.`package`.type]
         def ReflectBasis                = getMemberValue(ReflectPackage, nme.basis)
    lazy val ReflectRuntimePackage       = getPackageObjectIfDefined("scala.reflect.runtime") // defined in scala-reflect.jar, so we need to be careful
         def ReflectRuntimeUniverse      = if (ReflectRuntimePackage != NoSymbol) getMemberValue(ReflectRuntimePackage, nme.universe) else NoSymbol
         def ReflectRuntimeCurrentMirror = if (ReflectRuntimePackage != NoSymbol) getMemberMethod(ReflectRuntimePackage, nme.currentMirror) else NoSymbol

    lazy val PartialManifestClass  = getMemberType(ReflectPackage, tpnme.ClassManifest)
    lazy val PartialManifestModule = requiredModule[scala.reflect.ClassManifestFactory.type]
    lazy val FullManifestClass     = requiredClass[scala.reflect.Manifest[_]]
    lazy val FullManifestModule    = requiredModule[scala.reflect.ManifestFactory.type]
    lazy val OptManifestClass      = requiredClass[scala.reflect.OptManifest[_]]
    lazy val NoManifest            = requiredModule[scala.reflect.NoManifest.type]

    lazy val ExprsClass            = getClassIfDefined("scala.reflect.api.Exprs") // defined in scala-reflect.jar, so we need to be careful
    lazy val ExprClass             = if (ExprsClass != NoSymbol) getMemberClass(ExprsClass, tpnme.Expr) else NoSymbol
         def ExprSplice            = if (ExprsClass != NoSymbol) getMemberMethod(ExprClass, nme.splice) else NoSymbol
         def ExprValue             = if (ExprsClass != NoSymbol) getMemberMethod(ExprClass, nme.value) else NoSymbol
    lazy val ExprModule            = if (ExprsClass != NoSymbol) getMemberModule(ExprsClass, nme.Expr) else NoSymbol

    lazy val ClassTagModule        = requiredModule[scala.reflect.ClassTag[_]]
    lazy val ClassTagClass         = requiredClass[scala.reflect.ClassTag[_]]
    lazy val TypeTagsClass         = requiredClass[scala.reflect.base.TypeTags]
    lazy val AbsTypeTagClass       = getMemberClass(TypeTagsClass, tpnme.AbsTypeTag)
    lazy val AbsTypeTagModule      = getMemberModule(TypeTagsClass, nme.AbsTypeTag)
    lazy val TypeTagClass          = getMemberClass(TypeTagsClass, tpnme.TypeTag)
    lazy val TypeTagModule         = getMemberModule(TypeTagsClass, nme.TypeTag)

    lazy val BaseUniverseClass     = requiredClass[scala.reflect.base.Universe]
    lazy val ApiUniverseClass      = getClassIfDefined("scala.reflect.api.Universe") // defined in scala-reflect.jar, so we need to be careful
         def ApiUniverseReify      = if (ApiUniverseClass != NoSymbol) getMemberMethod(ApiUniverseClass, nme.reify) else NoSymbol
    lazy val JavaUniverseClass     = getClassIfDefined("scala.reflect.api.JavaUniverse") // defined in scala-reflect.jar, so we need to be careful

    lazy val MirrorOfClass         = requiredClass[scala.reflect.base.MirrorOf[_]]

    lazy val TypeCreatorClass      = requiredClass[scala.reflect.base.TypeCreator]
    lazy val TreeCreatorClass      = requiredClass[scala.reflect.base.TreeCreator]

    lazy val MacroContextClass                   = getClassIfDefined("scala.reflect.makro.Context") // defined in scala-reflect.jar, so we need to be careful
         def MacroContextPrefix                  = if (MacroContextClass != NoSymbol) getMemberMethod(MacroContextClass, nme.prefix) else NoSymbol
         def MacroContextPrefixType              = if (MacroContextClass != NoSymbol) getMemberType(MacroContextClass, tpnme.PrefixType) else NoSymbol
         def MacroContextUniverse                = if (MacroContextClass != NoSymbol) getMemberMethod(MacroContextClass, nme.universe) else NoSymbol
         def MacroContextMirror                  = if (MacroContextClass != NoSymbol) getMemberMethod(MacroContextClass, nme.mirror) else NoSymbol
         def MacroContextReify                   = if (MacroContextClass != NoSymbol) getMemberMethod(MacroContextClass, nme.reify) else NoSymbol
    lazy val MacroImplAnnotation                 = requiredClass[scala.reflect.makro.internal.macroImpl]
    lazy val MacroInternalPackage                = getPackageObject("scala.reflect.makro.internal")
         def MacroInternal_materializeClassTag   = getMemberMethod(MacroInternalPackage, nme.materializeClassTag)
         def MacroInternal_materializeAbsTypeTag = getMemberMethod(MacroInternalPackage, nme.materializeAbsTypeTag)
         def MacroInternal_materializeTypeTag    = getMemberMethod(MacroInternalPackage, nme.materializeTypeTag)

    lazy val ScalaSignatureAnnotation = requiredClass[scala.reflect.ScalaSignature]
    lazy val ScalaLongSignatureAnnotation = requiredClass[scala.reflect.ScalaLongSignature]

    // Option classes
    lazy val OptionClass: ClassSymbol = requiredClass[Option[_]]
    lazy val SomeClass: ClassSymbol   = requiredClass[Some[_]]
    lazy val NoneModule: ModuleSymbol = requiredModule[scala.None.type]
    lazy val SomeModule: ModuleSymbol = requiredModule[scala.Some.type]

    def compilerTypeFromTag(tt: BaseUniverse # AbsTypeTag[_]): Type = tt.in(rootMirror).tpe
    def compilerSymbolFromTag(tt: BaseUniverse # AbsTypeTag[_]): Symbol = tt.in(rootMirror).tpe.typeSymbol

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

    def isOptionType(tp: Type)  = tp.typeSymbol isSubClass OptionClass
    def isSomeType(tp: Type)    = tp.typeSymbol eq SomeClass
    def isNoneType(tp: Type)    = tp.typeSymbol eq NoneModule

    // Product, Tuple, Function, AbstractFunction
    private def mkArityArray(name: String, arity: Int, countFrom: Int): Array[ClassSymbol] = {
      val list = countFrom to arity map (i => getRequiredClass("scala." + name + i))
      list.toArray
    }
    def prepend[S >: ClassSymbol : ClassTag](elem0: S, elems: Array[ClassSymbol]): Array[S] = elem0 +: elems

    private def aritySpecificType[S <: Symbol](symbolArray: Array[S], args: List[Type], others: Type*): Type = {
      val arity = args.length
      if (arity >= symbolArray.length) NoType
      else appliedType(symbolArray(arity), args ++ others: _*)
    }

    val MaxTupleArity, MaxProductArity, MaxFunctionArity = 22
    lazy val ProductClass: Array[ClassSymbol] = prepend(UnitClass, mkArityArray("Product", MaxProductArity, 1))
    lazy val TupleClass: Array[Symbol] = prepend(NoSymbol, mkArityArray("Tuple", MaxTupleArity, 1))
    lazy val FunctionClass         = mkArityArray("Function", MaxFunctionArity, 0)
    lazy val AbstractFunctionClass = mkArityArray("runtime.AbstractFunction", MaxFunctionArity, 0)

    /** Creators for TupleN, ProductN, FunctionN. */
    def tupleType(elems: List[Type])                            = aritySpecificType(TupleClass, elems)
    def productType(elems: List[Type])                          = aritySpecificType(ProductClass, elems)
    def functionType(formals: List[Type], restpe: Type)         = aritySpecificType(FunctionClass, formals, restpe)
    def abstractFunctionType(formals: List[Type], restpe: Type) = aritySpecificType(AbstractFunctionClass, formals, restpe)

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

    @deprecated("Use isTupleType", "2.10.0")
    def isTupleTypeOrSubtype(tp: Type) = isTupleType(tp)

    def tupleField(n: Int, j: Int) = getMemberValue(TupleClass(n), nme.productAccessorName(j))
    // NOTE: returns true for NoSymbol since it's included in the TupleClass array -- is this intensional?
    def isTupleSymbol(sym: Symbol) = TupleClass contains unspecializedSymbol(sym)
    def isProductNClass(sym: Symbol) = ProductClass contains sym

    def unspecializedSymbol(sym: Symbol): Symbol = {
      if (sym hasFlag SPECIALIZED) {
        // add initialization from its generic class constructor
        val genericName = nme.unspecializedName(sym.name)
        val member = sym.owner.info.decl(genericName.toTypeName)
        member
      }
      else sym
    }

    // Checks whether the given type is true for the given condition,
    // or if it is a specialized subtype of a type for which it is true.
    //
    // Origins notes:
    // An issue was introduced with specialization in that the implementation
    // of "isTupleType" in Definitions relied upon sym == TupleClass(elems.length).
    // This test is untrue for specialized tuples, causing mysterious behavior
    // because only some tuples are specialized.
    def isPossiblySpecializedType(tp: Type)(cond: Type => Boolean) = {
      cond(tp) || (tp match {
        case TypeRef(pre, sym, args) if sym hasFlag SPECIALIZED =>
          cond(tp baseType unspecializedSymbol(sym))
        case _ =>
          false
      })
    }
    // No normalization.
    def isTupleTypeDirect(tp: Type) = isPossiblySpecializedType(tp) {
      case TypeRef(_, sym, args) if args.nonEmpty =>
        val len = args.length
        len <= MaxTupleArity && sym == TupleClass(len)
      case _ => false
    }
    def isTupleType(tp: Type) = isTupleTypeDirect(tp.normalize)

    lazy val ProductRootClass: ClassSymbol = requiredClass[scala.Product]
      def Product_productArity          = getMemberMethod(ProductRootClass, nme.productArity)
      def Product_productElement        = getMemberMethod(ProductRootClass, nme.productElement)
      def Product_iterator              = getMemberMethod(ProductRootClass, nme.productIterator)
      def Product_productPrefix         = getMemberMethod(ProductRootClass, nme.productPrefix)
      def Product_canEqual              = getMemberMethod(ProductRootClass, nme.canEqual_)
      // def Product_productElementName = getMemberMethod(ProductRootClass, nme.productElementName)

      def productProj(z:Symbol, j: Int): TermSymbol = getMemberValue(z, nme.productAccessorName(j))
      def productProj(n: Int,   j: Int): TermSymbol = productProj(ProductClass(n), j)

      /** returns true if this type is exactly ProductN[T1,...,Tn], not some subclass */
      def isExactProductType(tp: Type): Boolean = isProductNClass(tp.typeSymbol)

    /** if tpe <: ProductN[T1,...,TN], returns List(T1,...,TN) else Nil */
    def getProductArgs(tpe: Type): List[Type] = tpe.baseClasses find isProductNClass match {
      case Some(x)  => tpe.baseType(x).typeArgs
      case _        => Nil
    }

    def unapplyUnwrap(tpe:Type) = tpe.finalResultType.normalize match {
      case RefinedType(p :: _, _) => p.normalize
      case tp                     => tp
    }

    def functionApply(n: Int) = getMemberMethod(FunctionClass(n), nme.apply)

    def abstractFunctionForFunctionType(tp: Type) =
      if (isFunctionType(tp)) abstractFunctionType(tp.typeArgs.init, tp.typeArgs.last)
      else NoType

    def isFunctionType(tp: Type): Boolean = tp.normalize match {
      case TypeRef(_, sym, args) if args.nonEmpty =>
        val arity = args.length - 1   // -1 is the return type
        arity <= MaxFunctionArity && sym == FunctionClass(arity)
      case _ =>
        false
    }

    def isPartialFunctionType(tp: Type): Boolean = {
      val sym = tp.typeSymbol
      (sym eq PartialFunctionClass) || (sym eq AbstractPartialFunctionClass)
    }

    def isSeqType(tp: Type) = elementType(SeqClass, tp.normalize) != NoType

    def elementType(container: Symbol, tp: Type): Type = tp match {
      case TypeRef(_, `container`, arg :: Nil)  => arg
      case _                                    => NoType
    }

    def arrayType(arg: Type)         = appliedType(ArrayClass, arg)
    def byNameType(arg: Type)        = appliedType(ByNameParamClass, arg)
    def iteratorOfType(tp: Type)     = appliedType(IteratorClass, tp)
    def javaRepeatedType(arg: Type)  = appliedType(JavaRepeatedParamClass, arg)
    def optionType(tp: Type)         = appliedType(OptionClass, tp)
    def scalaRepeatedType(arg: Type) = appliedType(RepeatedParamClass, arg)
    def seqType(arg: Type)           = appliedType(SeqClass, arg)
    def someType(tp: Type)           = appliedType(SomeClass, tp)

    def StringArray   = arrayType(StringClass.tpe)
    lazy val ObjectArray   = arrayType(ObjectClass.tpe)

    def ClassType(arg: Type) =
      if (phase.erasedTypes || forMSIL) ClassClass.tpe
      else appliedType(ClassClass, arg)

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
    lazy val ValueTypeClass: ClassSymbol = getClassByName(sn.ValueType)
    // System.MulticastDelegate
    lazy val DelegateClass: ClassSymbol = getClassByName(sn.Delegate)
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
    lazy val Any_==       = enterNewMethod(AnyClass, nme.EQ, anyparam, booltype, FINAL)
    lazy val Any_!=       = enterNewMethod(AnyClass, nme.NE, anyparam, booltype, FINAL)
    lazy val Any_equals   = enterNewMethod(AnyClass, nme.equals_, anyparam, booltype)
    lazy val Any_hashCode = enterNewMethod(AnyClass, nme.hashCode_, Nil, inttype)
    lazy val Any_toString = enterNewMethod(AnyClass, nme.toString_, Nil, stringtype)
    lazy val Any_##       = enterNewMethod(AnyClass, nme.HASHHASH, Nil, inttype, FINAL)

    // Any_getClass requires special handling.  The return type is determined on
    // a per-call-site basis as if the function being called were actually:
    //
    //    // Assuming `target.getClass()`
    //    def getClass[T](target: T): Class[_ <: T]
    //
    // Since getClass is not actually a polymorphic method, this requires compiler
    // participation.  At the "Any" level, the return type is Class[_] as it is in
    // java.lang.Object.  Java also special cases the return type.
    lazy val Any_getClass     = enterNewMethod(AnyClass, nme.getClass_, Nil, getMemberMethod(ObjectClass, nme.getClass_).tpe.resultType, DEFERRED)
    lazy val Any_isInstanceOf = newT1NullaryMethod(AnyClass, nme.isInstanceOf_, FINAL)(_ => booltype)
    lazy val Any_asInstanceOf = newT1NullaryMethod(AnyClass, nme.asInstanceOf_, FINAL)(_.typeConstructor)

  // A type function from T => Class[U], used to determine the return
    // type of getClass calls.  The returned type is:
    //
    //  1. If T is a value type, Class[T].
    //  2. If T is a phantom type (Any or AnyVal), Class[_].
    //  3. If T is a local class, Class[_ <: |T|].
    //  4. Otherwise, Class[_ <: T].
    //
    // Note: AnyVal cannot be Class[_ <: AnyVal] because if the static type of the
    // receiver is AnyVal, it implies the receiver is boxed, so the correct
    // class object is that of java.lang.Integer, not Int.
    //
    // TODO: If T is final, return type could be Class[T].  Should it?
    def getClassReturnType(tp: Type): Type = {
      val sym     = tp.typeSymbol

      if (phase.erasedTypes) ClassClass.tpe
      else if (isPrimitiveValueClass(sym)) ClassType(tp.widen)
      else {
        val eparams    = typeParamsToExistentials(ClassClass, ClassClass.typeParams)
        val upperBound = (
          if (isPhantomClass(sym)) AnyClass.tpe
          else if (sym.isLocalClass) erasure.intersectionDominator(tp.parents)
          else tp.widen
        )

        existentialAbstraction(
          eparams,
          ClassType((eparams.head setInfo TypeBounds.upper(upperBound)).tpe)
        )
      }
    }

    /** Remove references to class Object (other than the head) in a list of parents */
    def removeLaterObjects(tps: List[Type]): List[Type] = tps match {
      case Nil      => Nil
      case x :: xs  => x :: xs.filterNot(_.typeSymbol == ObjectClass)
    }
    /** Remove all but one reference to class Object from a list of parents. */
    def removeRedundantObjects(tps: List[Type]): List[Type] = tps match {
      case Nil      => Nil
      case x :: xs  =>
        if (x.typeSymbol == ObjectClass)
          x :: xs.filterNot(_.typeSymbol == ObjectClass)
        else
          x :: removeRedundantObjects(xs)
    }
    /** Order a list of types with non-trait classes before others. */
    def classesFirst(tps: List[Type]): List[Type] = {
      val (classes, others) = tps partition (t => t.typeSymbol.isClass && !t.typeSymbol.isTrait)
      if (classes.isEmpty || others.isEmpty || (tps startsWith classes)) tps
      else classes ::: others
    }
    /** The following transformations applied to a list of parents.
     *  If any parent is a class/trait, all parents which normalize to
     *  Object are discarded.  Otherwise, all parents which normalize
     *  to Object except the first one found are discarded.
     */
    def normalizedParents(parents: List[Type]): List[Type] = {
      if (parents exists (t => (t.typeSymbol ne ObjectClass) && t.typeSymbol.isClass))
        parents filterNot (_.typeSymbol eq ObjectClass)
      else
        removeRedundantObjects(parents)
    }

    def typeStringNoPackage(tp: Type) =
      "" + tp stripPrefix tp.typeSymbol.enclosingPackage.fullName + "."

    def briefParentsString(parents: List[Type]) =
      normalizedParents(parents) map typeStringNoPackage mkString " with "

    def parentsString(parents: List[Type]) =
      normalizedParents(parents) mkString " with "

    def typeParamsString(tp: Type) = tp match {
      case PolyType(tparams, _) => tparams map (_.defString) mkString ("[", ",", "]")
      case _                    => ""
    }
    def valueParamsString(tp: Type) = tp match {
      case MethodType(params, _) => params map (_.defString) mkString ("(", ",", ")")
      case _                     => ""
    }

    // members of class java.lang.{ Object, String }
    lazy val Object_## = enterNewMethod(ObjectClass, nme.HASHHASH, Nil, inttype, FINAL)
    lazy val Object_== = enterNewMethod(ObjectClass, nme.EQ, anyrefparam, booltype, FINAL)
    lazy val Object_!= = enterNewMethod(ObjectClass, nme.NE, anyrefparam, booltype, FINAL)
    lazy val Object_eq = enterNewMethod(ObjectClass, nme.eq, anyrefparam, booltype, FINAL)
    lazy val Object_ne = enterNewMethod(ObjectClass, nme.ne, anyrefparam, booltype, FINAL)
    lazy val Object_isInstanceOf = newT1NoParamsMethod(ObjectClass, nme.isInstanceOf_Ob, FINAL | SYNTHETIC)(_ => booltype)
    lazy val Object_asInstanceOf = newT1NoParamsMethod(ObjectClass, nme.asInstanceOf_Ob, FINAL | SYNTHETIC)(_.typeConstructor)
    lazy val Object_synchronized = newPolyMethod(1, ObjectClass, nme.synchronized_, FINAL)(tps =>
      (Some(List(tps.head.typeConstructor)), tps.head.typeConstructor)
    )
    lazy val String_+ = enterNewMethod(StringClass, nme.raw.PLUS, anyparam, stringtype, FINAL)

    def Object_getClass  = getMemberMethod(ObjectClass, nme.getClass_)
    def Object_clone     = getMemberMethod(ObjectClass, nme.clone_)
    def Object_finalize  = getMemberMethod(ObjectClass, nme.finalize_)
    def Object_notify    = getMemberMethod(ObjectClass, nme.notify_)
    def Object_notifyAll = getMemberMethod(ObjectClass, nme.notifyAll_)
    def Object_equals    = getMemberMethod(ObjectClass, nme.equals_)
    def Object_hashCode  = getMemberMethod(ObjectClass, nme.hashCode_)
    def Object_toString  = getMemberMethod(ObjectClass, nme.toString_)

    // boxed classes
    lazy val ObjectRefClass         = requiredClass[scala.runtime.ObjectRef[_]]
    lazy val VolatileObjectRefClass = requiredClass[scala.runtime.VolatileObjectRef[_]]
    lazy val RuntimeStaticsModule   = getRequiredModule("scala.runtime.Statics")
    lazy val BoxesRunTimeModule     = getRequiredModule("scala.runtime.BoxesRunTime")
    lazy val BoxesRunTimeClass      = BoxesRunTimeModule.moduleClass
    lazy val BoxedNumberClass       = getClassByName(sn.BoxedNumber)
    lazy val BoxedCharacterClass    = getClassByName(sn.BoxedCharacter)
    lazy val BoxedBooleanClass      = getClassByName(sn.BoxedBoolean)
    lazy val BoxedByteClass         = requiredClass[java.lang.Byte]
    lazy val BoxedShortClass        = requiredClass[java.lang.Short]
    lazy val BoxedIntClass          = requiredClass[java.lang.Integer]
    lazy val BoxedLongClass         = requiredClass[java.lang.Long]
    lazy val BoxedFloatClass        = requiredClass[java.lang.Float]
    lazy val BoxedDoubleClass       = requiredClass[java.lang.Double]

    lazy val Boxes_isNumberOrBool  = getDecl(BoxesRunTimeClass, nme.isBoxedNumberOrBoolean)
    lazy val Boxes_isNumber        = getDecl(BoxesRunTimeClass, nme.isBoxedNumber)

    lazy val BoxedUnitClass         = requiredClass[scala.runtime.BoxedUnit]
    lazy val BoxedUnitModule        = getRequiredModule("scala.runtime.BoxedUnit")
      def BoxedUnit_UNIT            = getMemberValue(BoxedUnitModule, nme.UNIT)
      def BoxedUnit_TYPE            = getMemberValue(BoxedUnitModule, nme.TYPE_)

    // Annotation base classes
    lazy val AnnotationClass            = requiredClass[scala.annotation.Annotation]
    lazy val ClassfileAnnotationClass   = requiredClass[scala.annotation.ClassfileAnnotation]
    lazy val StaticAnnotationClass      = requiredClass[scala.annotation.StaticAnnotation]

    // Annotations
    lazy val BridgeClass                = requiredClass[scala.annotation.bridge]
    lazy val ElidableMethodClass        = requiredClass[scala.annotation.elidable]
    lazy val ImplicitNotFoundClass      = requiredClass[scala.annotation.implicitNotFound]
    lazy val MigrationAnnotationClass   = requiredClass[scala.annotation.migration]
    lazy val ScalaStrictFPAttr          = requiredClass[scala.annotation.strictfp]
    lazy val SerializableAttr           = requiredClass[scala.annotation.serializable] // @serializable is deprecated
    lazy val SwitchClass                = requiredClass[scala.annotation.switch]
    lazy val TailrecClass               = requiredClass[scala.annotation.tailrec]
    lazy val VarargsClass               = requiredClass[scala.annotation.varargs]
    lazy val uncheckedStableClass       = requiredClass[scala.annotation.unchecked.uncheckedStable]
    lazy val uncheckedVarianceClass     = requiredClass[scala.annotation.unchecked.uncheckedVariance]

    lazy val BeanPropertyAttr           = requiredClass[scala.beans.BeanProperty]
    lazy val BooleanBeanPropertyAttr    = requiredClass[scala.beans.BooleanBeanProperty]
    lazy val CloneableAttr              = requiredClass[scala.cloneable]
    lazy val DeprecatedAttr             = requiredClass[scala.deprecated]
    lazy val DeprecatedNameAttr         = requiredClass[scala.deprecatedName]
    lazy val NativeAttr                 = requiredClass[scala.native]
    lazy val RemoteAttr                 = requiredClass[scala.remote]
    lazy val ScalaInlineClass           = requiredClass[scala.inline]
    lazy val ScalaNoInlineClass         = requiredClass[scala.noinline]
    lazy val SerialVersionUIDAttr       = requiredClass[scala.SerialVersionUID]
    lazy val SpecializedClass           = requiredClass[scala.specialized]
    lazy val ThrowsClass                = requiredClass[scala.throws]
    lazy val TransientAttr              = requiredClass[scala.transient]
    lazy val UncheckedClass             = requiredClass[scala.unchecked]
    lazy val UnspecializedClass         = requiredClass[scala.annotation.unspecialized]
    lazy val VolatileAttr               = requiredClass[scala.volatile]

    // Meta-annotations
    lazy val BeanGetterTargetClass      = requiredClass[meta.beanGetter]
    lazy val BeanSetterTargetClass      = requiredClass[meta.beanSetter]
    lazy val FieldTargetClass           = requiredClass[meta.field]
    lazy val GetterTargetClass          = requiredClass[meta.getter]
    lazy val ParamTargetClass           = requiredClass[meta.param]
    lazy val SetterTargetClass          = requiredClass[meta.setter]
    lazy val ClassTargetClass           = requiredClass[meta.companionClass]
    lazy val ObjectTargetClass          = requiredClass[meta.companionObject]
    lazy val MethodTargetClass          = requiredClass[meta.companionMethod]    // TODO: module, moduleClass? package, packageObject?
    lazy val LanguageFeatureAnnot       = requiredClass[meta.languageFeature]

    // Language features
    lazy val languageFeatureModule      = getRequiredModule("scala.languageFeature")
    lazy val experimentalModule         = getMemberModule(languageFeatureModule, nme.experimental)
    lazy val MacrosFeature              = getLanguageFeature("macros", experimentalModule)
    lazy val DynamicsFeature            = getLanguageFeature("dynamics")
    lazy val PostfixOpsFeature          = getLanguageFeature("postfixOps")
    lazy val ReflectiveCallsFeature     = getLanguageFeature("reflectiveCalls")
    lazy val ImplicitConversionsFeature = getLanguageFeature("implicitConversions")
    lazy val HigherKindsFeature         = getLanguageFeature("higherKinds")
    lazy val ExistentialsFeature        = getLanguageFeature("existentials")

    def isMetaAnnotation(sym: Symbol): Boolean = metaAnnotations(sym) || (
      // Trying to allow for deprecated locations
      sym.isAliasType && isMetaAnnotation(sym.info.typeSymbol)
    )
    lazy val metaAnnotations = Set[Symbol](
      FieldTargetClass, ParamTargetClass,
      GetterTargetClass, SetterTargetClass,
      BeanGetterTargetClass, BeanSetterTargetClass
    )

    lazy val AnnotationDefaultAttr: ClassSymbol = {
      val attr = enterNewClass(RuntimePackageClass, tpnme.AnnotationDefaultATTR, List(AnnotationClass.tpe))
      // This attribute needs a constructor so that modifiers in parsed Java code make sense
      attr.info.decls enter attr.newClassConstructor(NoPosition)
      attr
    }

    @deprecated("Moved to rootMirror.getClass", "2.10.0")
    def getClass(fullname: Name): ClassSymbol = rootMirror.getClassByName(fullname)

    @deprecated("Moved to rootMirror.getModule", "2.10.0")
    def getModule(fullname: Name): ModuleSymbol = rootMirror.getModule(fullname)

    private def fatalMissingSymbol(owner: Symbol, name: Name, what: String = "member") = {
      throw new FatalError(owner + " does not have a " + what + " " + name)
    }

    def getLanguageFeature(name: String, owner: Symbol = languageFeatureModule): Symbol =
      // [Eugene++] `getMemberClass` leads to crashes in mixin:
      // "object languageFeature does not have a member class implicitConversions"
      // that's because by that time `implicitConversions` becomes a module
      // getMemberClass(owner, newTypeName(name))
      getMember(owner, newTypeName(name))

    def termMember(owner: Symbol, name: String): Symbol = owner.info.member(newTermName(name))
    def typeMember(owner: Symbol, name: String): Symbol = owner.info.member(newTypeName(name))

    def findNamedMember(fullName: Name, root: Symbol): Symbol = {
      val segs = nme.segments(fullName.toString, fullName.isTermName)
      if (segs.isEmpty || segs.head != root.simpleName) NoSymbol
      else findNamedMember(segs.tail, root)
    }
    def findNamedMember(segs: List[Name], root: Symbol): Symbol =
      if (segs.isEmpty) root
      else findNamedMember(segs.tail, root.info member segs.head)

    def getMember(owner: Symbol, name: Name): Symbol = {
      getMemberIfDefined(owner, name) orElse {
        if (phase.flatClasses && name.isTypeName && !owner.isPackageObjectOrClass) {
          val pkg = owner.owner
          val flatname = nme.flattenedName(owner.name, name)
          getMember(pkg, flatname)
        }
        else fatalMissingSymbol(owner, name)
      }
    }
    def getMemberValue(owner: Symbol, name: Name): TermSymbol = {
      // [Eugene++] should be a ClassCastException instead?
      getMember(owner, name.toTermName) match {
        case x: TermSymbol => x
        case _             => fatalMissingSymbol(owner, name, "member value")
      }
    }
    def getMemberModule(owner: Symbol, name: Name): ModuleSymbol = {
      // [Eugene++] should be a ClassCastException instead?
      getMember(owner, name.toTermName) match {
        case x: ModuleSymbol => x
        case _               => fatalMissingSymbol(owner, name, "member object")
      }
    }
    def getMemberType(owner: Symbol, name: Name): TypeSymbol = {
      // [Eugene++] should be a ClassCastException instead?
      getMember(owner, name.toTypeName) match {
        case x: TypeSymbol => x
        case _             => fatalMissingSymbol(owner, name, "member type")
      }
    }
    def getMemberClass(owner: Symbol, name: Name): ClassSymbol = {
      // [Eugene++] should be a ClassCastException instead?
      val y = getMember(owner, name.toTypeName)
      getMember(owner, name.toTypeName) match {
        case x: ClassSymbol => x
        case _              => fatalMissingSymbol(owner, name, "member class")
      }
    }
    def getMemberMethod(owner: Symbol, name: Name): TermSymbol = {
      // [Eugene++] is this a bug?
      //
      // System.err.println(result.getClass)
      // System.err.println(result.flags)
      // System.err.println("isMethod = " + result.isMethod)
      // System.err.println("isTerm = " + result.isTerm)
      // System.err.println("isValue = " + result.isValue)
      // result.asMethodSymbol
      //
      // prints this:
      //
      // quick.lib:
      //     [javac] Compiling 1 source file to C:\Projects\KeplerUnderRefactoring\build\quick\classes\library
      // [scalacfork] Compiling 769 files to C:\Projects\KeplerUnderRefactoring\build\quick\classes\library
      // [scalacfork] class scala.reflect.internal.Symbols$TermSymbol
      // [scalacfork] 8589934592
      // [scalacfork] isMethod = false
      // [scalacfork] isTerm = true
      // [scalacfork] isValue = true
      // [scalacfork]
      // [scalacfork]      while compiling:  C:\Projects\KeplerUnderRefactoring\src\library\scala\LowPriorityImplicits.scala
      // [scalacfork]        current phase:  cleanup
      // [scalacfork]      library version:  version 2.10.0-20120507-185519-665d1d9127
      // [scalacfork]     compiler version:  version 2.10.0-20120507-185519-665d1d9127
      // [scalacfork]   reconstructed args:  -Xmacros -classpath C:\\Projects\\KeplerUnderRefactoring\\build\\quick\\classes\\library;C:\\Projects\\KeplerUnderRefactoring\\lib\\forkjoin.jar -d C:\\Projects\\KeplerUnderRefactoring\\build\\quick\\classes\\library -sourcepath C:\\Projects\\KeplerUnderRefactoring\\src\\library
      // [scalacfork]
      // [scalacfork] unhandled exception while transforming LowPriorityImplicits.scala
      // [scalacfork] error:
      // [scalacfork]      while compiling:  C:\Projects\KeplerUnderRefactoring\src\library\scala\LowPriorityImplicits.scala
      // [scalacfork]        current phase:  cleanup
      // [scalacfork]      library version:  version 2.10.0-20120507-185519-665d1d9127
      // [scalacfork]     compiler version:  version 2.10.0-20120507-185519-665d1d9127
      // [scalacfork]   reconstructed args:  -Xmacros -classpath C:\\Projects\\KeplerUnderRefactoring\\build\\quick\\classes\\library;C:\\Projects\\KeplerUnderRefactoring\\lib\\forkjoin.jar -d C:\\Projects\\KeplerUnderRefactoring\\build\\quick\\classes\\library -sourcepath C:\\Projects\\KeplerUnderRefactoring\\src\\library
      // [scalacfork]
      // [scalacfork] uncaught exception during compilation: java.lang.ClassCastException
      // [scalacfork] error: java.lang.ClassCastException: value apply
      // [scalacfork]  at scala.reflect.base.Symbols$SymbolBase$class.asMethodSymbol(Symbols.scala:118)
      // [scalacfork]  at scala.reflect.internal.Symbols$SymbolContextApiImpl.asMethodSymbol(Symbols.scala:63)
      // [scalacfork]  at scala.reflect.internal.Definitions$DefinitionsClass.Symbol_apply(Definitions.scala:381)

      // [Eugene++] should be a ClassCastException instead?
      getMember(owner, name.toTermName) match {
        // case x: MethodSymbol => x
        case x: TermSymbol => x
        case _             => fatalMissingSymbol(owner, name, "method")
      }
    }

    def getMemberIfDefined(owner: Symbol, name: Name): Symbol =
      owner.info.nonPrivateMember(name)

    /** Using getDecl rather than getMember may avoid issues with
     *  OverloadedTypes turning up when you don't want them, if you
     *  know the method in question is uniquely declared in the given owner.
     */
    def getDecl(owner: Symbol, name: Name): Symbol = {
      getDeclIfDefined(owner, name) orElse fatalMissingSymbol(owner, name, "decl")
    }
    def getDeclIfDefined(owner: Symbol, name: Name): Symbol =
      owner.info.nonPrivateDecl(name)

    def packageExists(packageName: String): Boolean =
      getModuleIfDefined(packageName).isPackage

    private def newAlias(owner: Symbol, name: TypeName, alias: Type): AliasTypeSymbol =
      owner.newAliasType(name) setInfoAndEnter alias

    private def specialPolyClass(name: TypeName, flags: Long)(parentFn: Symbol => Type): ClassSymbol = {
      val clazz   = enterNewClass(ScalaPackageClass, name, Nil)
      val tparam  = clazz.newSyntheticTypeParam("T0", flags)
      val parents = List(AnyRefClass.tpe, parentFn(tparam))

      clazz setInfo GenPolyType(List(tparam), ClassInfoType(parents, newScope, clazz))
    }

    def newPolyMethod(typeParamCount: Int, owner: Symbol, name: TermName, flags: Long)(createFn: PolyMethodCreator): MethodSymbol = {
      val msym    = owner.newMethod(name.encode, NoPosition, flags)
      val tparams = msym.newSyntheticTypeParams(typeParamCount)
      val mtpe    = createFn(tparams) match {
        case (Some(formals), restpe) => MethodType(msym.newSyntheticValueParams(formals), restpe)
        case (_, restpe)             => NullaryMethodType(restpe)
      }

      msym setInfoAndEnter genPolyType(tparams, mtpe)
    }

    /** T1 means one type parameter.
     */
    def newT1NullaryMethod(owner: Symbol, name: TermName, flags: Long)(createFn: Symbol => Type): MethodSymbol = {
      newPolyMethod(1, owner, name, flags)(tparams => (None, createFn(tparams.head)))
    }
    def newT1NoParamsMethod(owner: Symbol, name: TermName, flags: Long)(createFn: Symbol => Type): MethodSymbol = {
      newPolyMethod(1, owner, name, flags)(tparams => (Some(Nil), createFn(tparams.head)))
    }

    lazy val boxedClassValues = boxedClass.values.toSet[Symbol]
    lazy val isUnbox = unboxMethod.values.toSet[Symbol]
    lazy val isBox = boxMethod.values.toSet[Symbol]

    /** Is symbol a phantom class for which no runtime representation exists? */
    lazy val isPhantomClass = Set[Symbol](AnyClass, AnyValClass, NullClass, NothingClass)

    /** Is the symbol that of a parent which is added during parsing? */
    lazy val isPossibleSyntheticParent = ProductClass.toSet[Symbol] + ProductRootClass + SerializableClass

    private lazy val boxedValueClassesSet = boxedClass.values.toSet[Symbol] + BoxedUnitClass

    /** Is symbol a value class? */
    def isPrimitiveValueClass(sym: Symbol) = ScalaValueClasses contains sym
    def isNonUnitValueClass(sym: Symbol)   = isPrimitiveValueClass(sym) && (sym != UnitClass)
    def isSpecializableClass(sym: Symbol)  = isPrimitiveValueClass(sym) || (sym == AnyRefClass)
    def isPrimitiveValueType(tp: Type)     = isPrimitiveValueClass(tp.typeSymbol)

    /** Is symbol a boxed value class, e.g. java.lang.Integer? */
    def isBoxedValueClass(sym: Symbol) = boxedValueClassesSet(sym)

    /** If symbol is a value class (boxed or not), return the unboxed
     *  value class.  Otherwise, NoSymbol.
     */
    def unboxedValueClass(sym: Symbol): Symbol =
      if (isPrimitiveValueClass(sym)) sym
      else if (sym == BoxedUnitClass) UnitClass
      else boxedClass.map(kvp => (kvp._2: Symbol, kvp._1)).getOrElse(sym, NoSymbol)

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
        else if (isPrimitiveValueClass(etp.typeSymbol)) abbrvTag(etp.typeSymbol).toString()
        else "L" + flatNameString(etp.typeSymbol, '/') + ";"
      }
      val etp = erasure(tp)
      if (etp.typeSymbol == ArrayClass) signature1(etp)
      else flatNameString(etp.typeSymbol, '.')
    }

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
        String_+,
        ComparableClass,
        JavaSerializableClass
      )

      isInitialized = true
    } //init

    var nbScalaCallers: Int = 0
    def newScalaCaller(delegateType: Type): MethodSymbol = {
      assert(forMSIL, "scalaCallers can only be created if target is .NET")
      // object: reference to object on which to call (scala-)method
      val paramTypes: List[Type] = List(ObjectClass.tpe)
      val name = newTermName("$scalaCaller$$" + nbScalaCallers)
      // tparam => resultType, which is the resultType of PolyType, i.e. the result type after applying the
      // type parameter =-> a MethodType in this case
      // TODO: set type bounds manually (-> MulticastDelegate), see newTypeParam
      val newCaller = enterNewMethod(DelegateClass, name, paramTypes, delegateType, FINAL | STATIC)
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
