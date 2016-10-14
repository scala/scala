/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package reflect
package internal

import scala.language.postfixOps

import scala.annotation.meta
import scala.collection.mutable
import Flags._
import scala.reflect.api.{Universe => ApiUniverse}

trait Definitions extends api.StandardDefinitions {
  self: SymbolTable =>

  import rootMirror.{getModuleByName, getPackage, getClassByName, getRequiredClass, getRequiredModule, getClassIfDefined, getModuleIfDefined, getPackageObject, getPackageIfDefined, getPackageObjectIfDefined, requiredClass, requiredModule}

  object definitions extends DefinitionsClass

  /** Since both the value parameter types and the result type may
   *  require access to the type parameter symbols, we model polymorphic
   *  creation as a function from those symbols to (formal types, result type).
   *  The Option is to distinguish between nullary methods and empty-param-list
   *  methods.
   */
  private type PolyMethodCreator = List[Symbol] => (Option[List[Type]], Type)

  private def enterNewClass(owner: Symbol, name: TypeName, parents: List[Type], flags: Long = 0L): ClassSymbol = {
    val clazz = owner.newClassSymbol(name, NoPosition, flags)
    clazz setInfoAndEnter ClassInfoType(parents, newScope, clazz) markAllCompleted
  }
  private def newMethod(owner: Symbol, name: TermName, formals: List[Type], restpe: Type, flags: Long): MethodSymbol = {
    val msym   = owner.newMethod(name.encode, NoPosition, flags)
    val params = msym.newSyntheticValueParams(formals)
    val info = if (owner.isJavaDefined) JavaMethodType(params, restpe) else MethodType(params, restpe)
    msym setInfo info markAllCompleted
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

    private[Definitions] def catastrophicFailure() =
      abort("Could not find value classes! This is a catastrophic failure.  scala " +
        scala.util.Properties.versionString)

    private def valueClassSymbol(name: TypeName): ClassSymbol = {
      getMember(ScalaPackageClass, name) match {
        case x: ClassSymbol => x
        case _              => catastrophicFailure()
      }
    }

    private[Definitions] def classesMap[T](f: Name => T): Map[Symbol, T] = symbolsMap(ScalaValueClassesNoUnit, f)
    private def symbolsMap[T](syms: List[Symbol], f: Name => T): Map[Symbol, T] = mapFrom(syms)(x => f(x.name))
    private def symbolsMapFilt[T](syms: List[Symbol], p: Name => Boolean, f: Name => T) = symbolsMap(syms filter (x => p(x.name)), f)

    private def boxedName(name: Name) = sn.Boxed(name.toTypeName)

    lazy val abbrvTag         = symbolsMap(ScalaValueClasses, nameToTag) withDefaultValue OBJECT_TAG
    lazy val numericWeight    = symbolsMapFilt(ScalaValueClasses, nameToWeight.keySet, nameToWeight)
    lazy val boxedModule      = classesMap(x => getModuleByName(boxedName(x)))
    lazy val boxedClass       = classesMap(x => getClassByName(boxedName(x)))
    lazy val refClass         = classesMap(x => getRequiredClass("scala.runtime." + x + "Ref"))
    lazy val volatileRefClass = classesMap(x => getRequiredClass("scala.runtime.Volatile" + x + "Ref"))
    lazy val lazyHolders      = symbolsMap(ScalaValueClasses, x => getClassIfDefined("scala.runtime.Lazy" + x))
    lazy val LazyRefClass     = getClassIfDefined("scala.runtime.LazyRef")
    lazy val LazyUnitClass    = getClassIfDefined("scala.runtime.LazyUnit")

    lazy val allRefClasses: Set[Symbol] = {
      refClass.values.toSet ++ volatileRefClass.values.toSet ++ Set(VolatileObjectRefClass, ObjectRefClass)
    }

    def isNumericSubClass(sub: Symbol, sup: Symbol) = (
         (numericWeight contains sub)
      && (numericWeight contains sup)
      && (numericWeight(sup) % numericWeight(sub) == 0)
    )

    /** Is symbol a numeric value class? */
    def isNumericValueClass(sym: Symbol) = ScalaNumericValueClasses contains sym

    def isGetClass(sym: Symbol) = (
         sym.name == nme.getClass_ // this condition is for performance only, this is called from `Typer#stabilize`.
      && getClassMethods(sym)
    )

    lazy val UnitClass    = valueClassSymbol(tpnme.Unit)
    lazy val ByteClass    = valueClassSymbol(tpnme.Byte)
    lazy val ShortClass   = valueClassSymbol(tpnme.Short)
    lazy val CharClass    = valueClassSymbol(tpnme.Char)
    lazy val IntClass     = valueClassSymbol(tpnme.Int)
    lazy val LongClass    = valueClassSymbol(tpnme.Long)
    lazy val FloatClass   = valueClassSymbol(tpnme.Float)
    lazy val DoubleClass  = valueClassSymbol(tpnme.Double)
    lazy val BooleanClass = valueClassSymbol(tpnme.Boolean)
          def Boolean_and = getMemberMethod(BooleanClass, nme.ZAND)
          def Boolean_or  = getMemberMethod(BooleanClass, nme.ZOR)
          def Boolean_not = getMemberMethod(BooleanClass, nme.UNARY_!)

    lazy val UnitTpe      = UnitClass.tpe
    lazy val ByteTpe      = ByteClass.tpe
    lazy val ShortTpe     = ShortClass.tpe
    lazy val CharTpe      = CharClass.tpe
    lazy val IntTpe       = IntClass.tpe
    lazy val LongTpe      = LongClass.tpe
    lazy val FloatTpe     = FloatClass.tpe
    lazy val DoubleTpe    = DoubleClass.tpe
    lazy val BooleanTpe   = BooleanClass.tpe

    lazy val ScalaNumericValueClasses = ScalaValueClasses filterNot Set[Symbol](UnitClass, BooleanClass)
    lazy val ScalaValueClassesNoUnit  = ScalaValueClasses filterNot (_ eq UnitClass)
    lazy val ScalaValueClasses: List[ClassSymbol] = List(
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
    def ScalaPrimitiveValueClasses: List[ClassSymbol] = ScalaValueClasses

    def underlyingOfValueClass(clazz: Symbol): Type =
      clazz.derivedValueClassUnbox.tpe.resultType

  }

  abstract class DefinitionsClass extends DefinitionsApi with ValueClassDefinitions {
    private var isInitialized = false
    def isDefinitionsInitialized = isInitialized

    // It becomes tricky to create dedicated objects for other symbols because
    // of initialization order issues.
    lazy val JavaLangPackage      = getPackage(TermName("java.lang"))
    lazy val JavaLangPackageClass = JavaLangPackage.moduleClass.asClass
    lazy val ScalaPackage         = getPackage(TermName("scala"))
    lazy val ScalaPackageClass    = ScalaPackage.moduleClass.asClass
    lazy val RuntimePackage       = getPackage(TermName("scala.runtime"))
    lazy val RuntimePackageClass  = RuntimePackage.moduleClass.asClass

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

    /** Fully initialize the symbol, type, or scope.
     */
    def fullyInitializeSymbol(sym: Symbol): Symbol = {
      sym.initialize
      // Watch out for those darn raw types on method parameters
      if (sym.owner.initialize.isJavaDefined)
        sym.cookJavaRawInfo()

      fullyInitializeType(sym.info)
      fullyInitializeType(sym.tpe_*)
      sym
    }
    def fullyInitializeType(tp: Type): Type = {
      tp.typeParams foreach fullyInitializeSymbol
      mforeach(tp.paramss)(fullyInitializeSymbol)
      tp
    }
    def fullyInitializeScope(scope: Scope): Scope = {
      scope.sorted foreach fullyInitializeSymbol
      scope
    }
    /** Is this symbol a member of Object or Any? */
    def isUniversalMember(sym: Symbol) = ObjectClass isSubClass sym.owner

    /** Is this symbol unimportable? Unimportable symbols include:
     *  - constructors, because <init> is not a real name
     *  - private[this] members, which cannot be referenced from anywhere else
     *  - members of Any or Object, because every instance will inherit a
     *    definition which supersedes the imported one
     */
    def isUnimportable(sym: Symbol) = (
         (sym eq NoSymbol)
      || sym.isConstructor
      || sym.isPrivateLocal
    )
    def isUnimportableUnlessRenamed(sym: Symbol) = isUnimportable(sym) || isUniversalMember(sym)
    def isImportable(sym: Symbol) = !isUnimportable(sym)

    /** Is this type equivalent to Any, AnyVal, or AnyRef? */
    def isTrivialTopType(tp: Type) = (
         tp =:= AnyTpe
      || tp =:= AnyValTpe
      || tp =:= AnyRefTpe
    )

    def isUnitType(tp: Type) = tp.typeSymbol == UnitClass && tp.annotations.isEmpty

    def hasMultipleNonImplicitParamLists(member: Symbol): Boolean = hasMultipleNonImplicitParamLists(member.info)
    def hasMultipleNonImplicitParamLists(info: Type): Boolean = info match {
      case PolyType(_, restpe)                                   => hasMultipleNonImplicitParamLists(restpe)
      case MethodType(_, MethodType(p :: _, _)) if !p.isImplicit => true
      case _                                                     => false
    }

    private def fixupAsAnyTrait(tpe: Type): Type = tpe match {
      case ClassInfoType(parents, decls, clazz) =>
        if (parents.head.typeSymbol == AnyClass) tpe
        else {
          assert(parents.head.typeSymbol == ObjectClass, parents)
          ClassInfoType(AnyTpe :: parents.tail, decls, clazz)
        }
      case PolyType(tparams, restpe) =>
        PolyType(tparams, fixupAsAnyTrait(restpe))
    }

    // top types
    lazy val AnyClass    = enterNewClass(ScalaPackageClass, tpnme.Any, Nil, ABSTRACT) markAllCompleted
    lazy val AnyRefClass = newAlias(ScalaPackageClass, tpnme.AnyRef, ObjectTpe) markAllCompleted
    lazy val ObjectClass = getRequiredClass(sn.Object.toString)

    // Cached types for core monomorphic classes
    lazy val AnyRefTpe       = AnyRefClass.tpe
    lazy val AnyTpe          = AnyClass.tpe
    lazy val AnyValTpe       = AnyValClass.tpe
    lazy val BoxedUnitTpe    = BoxedUnitClass.tpe
    lazy val NothingTpe      = NothingClass.tpe
    lazy val NullTpe         = NullClass.tpe
    lazy val ObjectTpe       = ObjectClass.tpe
    lazy val SerializableTpe = SerializableClass.tpe
    lazy val StringTpe       = StringClass.tpe
    lazy val ThrowableTpe    = ThrowableClass.tpe

    lazy val ConstantTrue  = ConstantType(Constant(true))
    lazy val ConstantFalse = ConstantType(Constant(false))
    lazy val ConstantNull  = ConstantType(Constant(null))

    lazy val AnyValClass: ClassSymbol = (ScalaPackageClass.info member tpnme.AnyVal orElse {
      val anyval    = enterNewClass(ScalaPackageClass, tpnme.AnyVal, AnyTpe :: Nil, ABSTRACT)
      val av_constr = anyval.newClassConstructor(NoPosition)
      anyval.info.decls enter av_constr
      anyval markAllCompleted
    }).asInstanceOf[ClassSymbol]
      def AnyVal_getClass = getMemberMethod(AnyValClass, nme.getClass_)

    // bottom types
    lazy val RuntimeNothingClass  = getClassByName(fulltpnme.RuntimeNothing)
    lazy val RuntimeNullClass     = getClassByName(fulltpnme.RuntimeNull)

    sealed abstract class BottomClassSymbol(name: TypeName, parent: Symbol) extends ClassSymbol(ScalaPackageClass, NoPosition, name) {
      locally {
        this initFlags ABSTRACT | FINAL
        this setInfoAndEnter ClassInfoType(List(parent.tpe), newScope, this)
        this markAllCompleted
      }
      final override def isBottomClass = true
      final override def isThreadsafe(purpose: SymbolOps): Boolean = true
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

    lazy val UninitializedFieldConstructor = UninitializedErrorClass.primaryConstructor

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
    // SI-5941: ScalaPackage and JavaLangPackage are never ever shared between mirrors
    // as a result, `Int` becomes `scala.Int` and `String` becomes `java.lang.String`
    // I could just change `isOmittablePrefix`, but there's more to it, so I'm leaving this as a todo for now
    lazy val UnqualifiedModules = List(PredefModule, ScalaPackage, JavaLangPackage)
    // Those modules and their module classes
    lazy val UnqualifiedOwners  = UnqualifiedModules.toSet ++ UnqualifiedModules.map(_.moduleClass)

    lazy val PredefModule               = requiredModule[scala.Predef.type]
         def Predef_wrapArray(tp: Type) = getMemberMethod(PredefModule, wrapArrayMethodName(tp))
         def Predef_???                 = getMemberMethod(PredefModule, nme.???)
    def isPredefMemberNamed(sym: Symbol, name: Name) = (
      (sym.name == name) && (sym.owner == PredefModule.moduleClass)
    )

    /** Specialization.
     */
    lazy val SpecializableModule  = requiredModule[Specializable]

    lazy val ScalaRunTimeModule = requiredModule[scala.runtime.ScalaRunTime.type]
    lazy val SymbolModule       = requiredModule[scala.Symbol.type]
         def Symbol_apply       = getMemberMethod(SymbolModule, nme.apply)

    // classes with special meanings
    lazy val StringAddClass             = requiredClass[scala.runtime.StringAdd]
    lazy val ScalaNumberClass           = requiredClass[scala.math.ScalaNumber]
    lazy val DelayedInitClass           = requiredClass[scala.DelayedInit]
      def delayedInitMethod = getMemberMethod(DelayedInitClass, nme.delayedInit)

    lazy val TypeConstraintClass   = requiredClass[scala.annotation.TypeConstraint]
    lazy val SingletonClass        = enterNewClass(ScalaPackageClass, tpnme.Singleton, AnyTpe :: Nil, ABSTRACT | TRAIT | FINAL) markAllCompleted
    lazy val SerializableClass     = requiredClass[scala.Serializable]
    lazy val JavaSerializableClass = requiredClass[java.io.Serializable] modifyInfo fixupAsAnyTrait
    lazy val ComparableClass       = requiredClass[java.lang.Comparable[_]] modifyInfo fixupAsAnyTrait
    lazy val JavaCloneableClass    = requiredClass[java.lang.Cloneable]
    lazy val JavaNumberClass       = requiredClass[java.lang.Number]
    lazy val JavaEnumClass         = requiredClass[java.lang.Enum[_]]
    lazy val RemoteInterfaceClass  = requiredClass[java.rmi.Remote]
    lazy val RemoteExceptionClass  = requiredClass[java.rmi.RemoteException]
    lazy val JavaUtilMap           = requiredClass[java.util.Map[_, _]]
    lazy val JavaUtilHashMap       = requiredClass[java.util.HashMap[_, _]]

    lazy val ByNameParamClass       = specialPolyClass(tpnme.BYNAME_PARAM_CLASS_NAME, COVARIANT)(_ => AnyTpe)
    lazy val JavaRepeatedParamClass = specialPolyClass(tpnme.JAVA_REPEATED_PARAM_CLASS_NAME, COVARIANT)(tparam => arrayType(tparam.tpe))
    lazy val RepeatedParamClass     = specialPolyClass(tpnme.REPEATED_PARAM_CLASS_NAME, COVARIANT)(tparam => seqType(tparam.tpe))

    def isByNameParamType(tp: Type)        = tp.typeSymbol == ByNameParamClass
    def isScalaRepeatedParamType(tp: Type) = tp.typeSymbol == RepeatedParamClass
    def isJavaRepeatedParamType(tp: Type)  = tp.typeSymbol == JavaRepeatedParamClass
    def isRepeatedParamType(tp: Type)      = isScalaRepeatedParamType(tp) || isJavaRepeatedParamType(tp)
    def isRepeated(param: Symbol)          = isRepeatedParamType(param.tpe_*)
    def isByName(param: Symbol)            = isByNameParamType(param.tpe_*)
    def isCastSymbol(sym: Symbol)          = sym == Any_asInstanceOf || sym == Object_asInstanceOf

    def isJavaVarArgsMethod(m: Symbol)      = m.isMethod && isJavaVarArgs(m.info.params)
    def isJavaVarArgs(params: Seq[Symbol])  = params.nonEmpty && isJavaRepeatedParamType(params.last.tpe)
    def isScalaVarArgs(params: Seq[Symbol]) = params.nonEmpty && isScalaRepeatedParamType(params.last.tpe)
    def isVarArgsList(params: Seq[Symbol])  = params.nonEmpty && isRepeatedParamType(params.last.tpe)
    def isVarArgTypes(formals: Seq[Type])   = formals.nonEmpty && isRepeatedParamType(formals.last)

    def firstParamType(tpe: Type): Type = tpe.paramTypes match {
      case p :: _ => p
      case _      => NoType
    }
    def isImplicitParamss(paramss: List[List[Symbol]]) = paramss match {
      case (p :: _) :: _ => p.isImplicit
      case _             => false
    }

    def hasRepeatedParam(tp: Type): Boolean = tp match {
      case MethodType(formals, restpe) => isScalaVarArgs(formals) || hasRepeatedParam(restpe)
      case PolyType(_, restpe)         => hasRepeatedParam(restpe)
      case _                           => false
    }

    // wrapping and unwrapping
    def dropByName(tp: Type): Type = elementExtract(ByNameParamClass, tp) orElse tp
    def dropRepeated(tp: Type): Type = (
      if (isJavaRepeatedParamType(tp)) elementExtract(JavaRepeatedParamClass, tp) orElse tp
      else if (isScalaRepeatedParamType(tp)) elementExtract(RepeatedParamClass, tp) orElse tp
      else tp
    )
    def repeatedToSingle(tp: Type): Type                     = elementExtract(RepeatedParamClass, tp) orElse elementExtract(JavaRepeatedParamClass, tp) orElse tp
     // We don't need to deal with JavaRepeatedParamClass here, as `repeatedToSeq` is only called in the patmat translation for Scala sources.
    def repeatedToSeq(tp: Type): Type                        = elementTransform(RepeatedParamClass, tp)(seqType) orElse tp
    def seqToRepeated(tp: Type): Type                        = elementTransform(SeqClass, tp)(scalaRepeatedType) orElse tp
    def isReferenceArray(tp: Type)                           = elementTest(ArrayClass, tp)(_ <:< AnyRefTpe)
    def isArrayOfSymbol(tp: Type, elem: Symbol)              = elementTest(ArrayClass, tp)(_.typeSymbol == elem)
    def elementType(container: Symbol, tp: Type): Type       = elementExtract(container, tp)

    // collections classes
    lazy val ConsClass              = requiredClass[scala.collection.immutable.::[_]]
    lazy val IteratorClass          = requiredClass[scala.collection.Iterator[_]]
    lazy val IterableClass          = requiredClass[scala.collection.Iterable[_]]
    lazy val ListClass              = requiredClass[scala.collection.immutable.List[_]]
    lazy val SeqClass               = requiredClass[scala.collection.Seq[_]]
    lazy val JavaStringBuilderClass = requiredClass[java.lang.StringBuilder]
    lazy val JavaStringBufferClass  = requiredClass[java.lang.StringBuffer]
    lazy val JavaCharSequenceClass  = requiredClass[java.lang.CharSequence]
    lazy val TraversableClass       = requiredClass[scala.collection.Traversable[_]]

    lazy val ListModule       = requiredModule[scala.collection.immutable.List.type]
         def List_apply       = getMemberMethod(ListModule, nme.apply)
    lazy val NilModule        = requiredModule[scala.collection.immutable.Nil.type]
    lazy val SeqModule        = requiredModule[scala.collection.Seq.type]

    // arrays and their members
    lazy val ArrayModule                   = requiredModule[scala.Array.type]
      lazy val ArrayModule_overloadedApply = getMemberMethod(ArrayModule, nme.apply)
           def ArrayModule_genericApply    = ArrayModule_overloadedApply.suchThat(_.paramss.flatten.last.tpe.typeSymbol == ClassTagClass) // [T: ClassTag](xs: T*): Array[T]
           def ArrayModule_apply(tp: Type) = ArrayModule_overloadedApply.suchThat(_.tpe.resultType =:= arrayType(tp)) // (p1: AnyVal1, ps: AnyVal1*): Array[AnyVal1]
    lazy val ArrayClass                    = getRequiredClass("scala.Array") // requiredClass[scala.Array[_]]
      lazy val Array_apply                 = getMemberMethod(ArrayClass, nme.apply)
      lazy val Array_update                = getMemberMethod(ArrayClass, nme.update)
      lazy val Array_length                = getMemberMethod(ArrayClass, nme.length)
      lazy val Array_clone                 = getMemberMethod(ArrayClass, nme.clone_)

    // reflection / structural types
    lazy val SoftReferenceClass     = requiredClass[java.lang.ref.SoftReference[_]]
    lazy val MethodClass            = getClassByName(sn.MethodAsObject)
    lazy val EmptyMethodCacheClass  = requiredClass[scala.runtime.EmptyMethodCache]
    lazy val MethodCacheClass       = requiredClass[scala.runtime.MethodCache]
      def methodCache_find          = getMemberMethod(MethodCacheClass, nme.find_)
      def methodCache_add           = getMemberMethod(MethodCacheClass, nme.add_)
    lazy val StructuralCallSite     = getClassIfDefined("scala.runtime.StructuralCallSite")
      def StructuralCallSite_bootstrap = getMemberMethod(StructuralCallSite.linkedClassOfClass, sn.Bootstrap)
    // Marker for invokedynamic runtime.StructuralCall.bootstrap
    lazy val StructuralCallSite_dummy = NoSymbol.newMethodSymbol(nme.apply).setInfo(NullaryMethodType(StructuralCallSite.tpe))
      def StructuralCallSite_find              = getMemberIfDefined(StructuralCallSite, nme.find_)
      def StructuralCallSite_add               = getMemberIfDefined(StructuralCallSite, nme.add_)
      def StructuralCallSite_getParameterTypes = getMemberIfDefined(StructuralCallSite, nme.parameterTypes)
    lazy val SymbolLiteral        = getClassIfDefined("scala.runtime.SymbolLiteral")
      def SymbolLiteral_bootstrap = getMemberIfDefined(SymbolLiteral.linkedClassOfClass, sn.Bootstrap)
      def SymbolLiteral_dummy     = NoSymbol.newMethodSymbol(nme.apply).setInfo(NullaryMethodType(SymbolModule.companionClass.tpe))

    // XML
    lazy val ScalaXmlTopScope = getModuleIfDefined("scala.xml.TopScope")
    lazy val ScalaXmlPackage  = getPackageIfDefined(TermName("scala.xml"))

    // scala.reflect
    lazy val ReflectPackage              = requiredModule[scala.reflect.`package`.type]
    lazy val ReflectApiPackage           = getPackageObjectIfDefined("scala.reflect.api") // defined in scala-reflect.jar, so we need to be careful
    lazy val ReflectRuntimePackage       = getPackageObjectIfDefined("scala.reflect.runtime") // defined in scala-reflect.jar, so we need to be careful
         def ReflectRuntimeUniverse      = ReflectRuntimePackage.map(sym => getMemberValue(sym, nme.universe))
         def ReflectRuntimeCurrentMirror = ReflectRuntimePackage.map(sym => getMemberMethod(sym, nme.currentMirror))

    lazy val UniverseClass    = getClassIfDefined("scala.reflect.api.Universe") // defined in scala-reflect.jar, so we need to be careful
         def UniverseInternal = getMemberValue(UniverseClass, nme.internal)

    lazy val PartialManifestModule = requiredModule[scala.reflect.ClassManifestFactory.type]
    lazy val FullManifestClass     = requiredClass[scala.reflect.Manifest[_]]
    lazy val FullManifestModule    = requiredModule[scala.reflect.ManifestFactory.type]
    lazy val OptManifestClass      = requiredClass[scala.reflect.OptManifest[_]]
    lazy val NoManifest            = requiredModule[scala.reflect.NoManifest.type]

    lazy val TreesClass            = getClassIfDefined("scala.reflect.api.Trees") // defined in scala-reflect.jar, so we need to be careful

    lazy val ExprsClass            = getClassIfDefined("scala.reflect.api.Exprs") // defined in scala-reflect.jar, so we need to be careful
         def ExprClass             = ExprsClass.map(sym => getMemberClass(sym, tpnme.Expr))
         def ExprSplice            = ExprClass.map(sym => getMemberMethod(sym, nme.splice))
         def ExprValue             = ExprClass.map(sym => getMemberMethod(sym, nme.value))

    lazy val ClassTagModule         = requiredModule[scala.reflect.ClassTag[_]]
    lazy val ClassTagClass          = requiredClass[scala.reflect.ClassTag[_]]
    lazy val TypeTagsClass          = getClassIfDefined("scala.reflect.api.TypeTags") // defined in scala-reflect.jar, so we need to be careful

    lazy val ApiUniverseClass      = getClassIfDefined("scala.reflect.api.Universe") // defined in scala-reflect.jar, so we need to be careful
    lazy val JavaUniverseClass     = getClassIfDefined("scala.reflect.api.JavaUniverse") // defined in scala-reflect.jar, so we need to be careful

    lazy val MirrorClass           = getClassIfDefined("scala.reflect.api.Mirror") // defined in scala-reflect.jar, so we need to be careful

    lazy val TypeCreatorClass      = getClassIfDefined("scala.reflect.api.TypeCreator") // defined in scala-reflect.jar, so we need to be careful
    lazy val TreeCreatorClass      = getClassIfDefined("scala.reflect.api.TreeCreator") // defined in scala-reflect.jar, so we need to be careful

    private def Context_210               = if (settings.isScala211) NoSymbol else getClassIfDefined("scala.reflect.macros.Context") // needed under -Xsource:2.10
    lazy val BlackboxContextClass         = getClassIfDefined("scala.reflect.macros.blackbox.Context").orElse(Context_210) // defined in scala-reflect.jar, so we need to be careful

    lazy val WhiteboxContextClass         = getClassIfDefined("scala.reflect.macros.whitebox.Context").orElse(Context_210) // defined in scala-reflect.jar, so we need to be careful
         def MacroContextPrefix           = BlackboxContextClass.map(sym => getMemberMethod(sym, nme.prefix))
         def MacroContextPrefixType       = BlackboxContextClass.map(sym => getTypeMember(sym, tpnme.PrefixType))
         def MacroContextUniverse         = BlackboxContextClass.map(sym => getMemberMethod(sym, nme.universe))
         def MacroContextExprClass        = BlackboxContextClass.map(sym => getTypeMember(sym, tpnme.Expr))
         def MacroContextWeakTypeTagClass = BlackboxContextClass.map(sym => getTypeMember(sym, tpnme.WeakTypeTag))
         def MacroContextTreeType         = BlackboxContextClass.map(sym => getTypeMember(sym, tpnme.Tree))
    lazy val MacroImplAnnotation          = requiredClass[scala.reflect.macros.internal.macroImpl]

    lazy val StringContextClass           = requiredClass[scala.StringContext]

    // SI-8392 a reflection universe on classpath may not have
    // quasiquotes, if e.g. crosstyping with -Xsource on
    lazy val QuasiquoteClass             = if (ApiUniverseClass != NoSymbol) getMemberIfDefined(ApiUniverseClass, tpnme.Quasiquote) else NoSymbol
    lazy val QuasiquoteClass_api         = if (QuasiquoteClass != NoSymbol) getMember(QuasiquoteClass, tpnme.api) else NoSymbol
    lazy val QuasiquoteClass_api_apply   = if (QuasiquoteClass_api != NoSymbol) getMember(QuasiquoteClass_api, nme.apply) else NoSymbol
    lazy val QuasiquoteClass_api_unapply = if (QuasiquoteClass_api != NoSymbol) getMember(QuasiquoteClass_api, nme.unapply) else NoSymbol

    lazy val ScalaSignatureAnnotation = requiredClass[scala.reflect.ScalaSignature]
    lazy val ScalaLongSignatureAnnotation = requiredClass[scala.reflect.ScalaLongSignature]

    lazy val MethodHandle = getClassIfDefined("java.lang.invoke.MethodHandle")

    // Option classes
    lazy val OptionClass: ClassSymbol   = requiredClass[Option[_]]
    lazy val OptionModule: ModuleSymbol = requiredModule[scala.Option.type]
    lazy val SomeClass: ClassSymbol     = requiredClass[Some[_]]
    lazy val NoneModule: ModuleSymbol   = requiredModule[scala.None.type]
    lazy val SomeModule: ModuleSymbol   = requiredModule[scala.Some.type]

    def compilerTypeFromTag(tt: ApiUniverse # WeakTypeTag[_]): Type = tt.in(rootMirror).tpe
    def compilerSymbolFromTag(tt: ApiUniverse # WeakTypeTag[_]): Symbol = tt.in(rootMirror).tpe.typeSymbol

    // The given symbol is a method with the right name and signature to be a runnable java program.
    def isJavaMainMethod(sym: Symbol) = (sym.name == nme.main) && (sym.info match {
      case MethodType(p :: Nil, restpe) => isArrayOfSymbol(p.tpe, StringClass) && restpe.typeSymbol == UnitClass
      case _                            => false
    })
    // The given class has a main method.
    def hasJavaMainMethod(sym: Symbol): Boolean =
      (sym.tpe member nme.main).alternatives exists isJavaMainMethod

    class VarArityClass(name: String, maxArity: Int, countFrom: Int = 0, init: Option[ClassSymbol] = None) extends VarArityClassApi {
      private val offset = countFrom - init.size
      private def isDefinedAt(i: Int) = i < seq.length + offset && i >= offset
      val seq: IndexedSeq[ClassSymbol] = (init ++: countFrom.to(maxArity).map { i => getRequiredClass("scala." + name + i) }).toVector
      def apply(i: Int) = if (isDefinedAt(i)) seq(i - offset) else NoSymbol
      def specificType(args: List[Type], others: Type*): Type = {
        val arity = args.length
        if (!isDefinedAt(arity)) NoType
        else appliedType(apply(arity), args ++ others: _*)
      }
    }
    // would be created synthetically for the default args. We call all objects in this method from the generated code
    // in JavaUniverseForce, so it is clearer to define this explicitly define this in source.
    object VarArityClass

    val MaxTupleArity, MaxProductArity, MaxFunctionArity = 22

    lazy val ProductClass          = new VarArityClass("Product", MaxProductArity, countFrom = 1, init = Some(UnitClass))
    lazy val TupleClass            = new VarArityClass("Tuple", MaxTupleArity, countFrom = 1)
    lazy val FunctionClass         = new VarArityClass("Function", MaxFunctionArity)
    lazy val AbstractFunctionClass = new VarArityClass("runtime.AbstractFunction", MaxFunctionArity)

    /** Creators for TupleN, ProductN, FunctionN. */
    def tupleType(elems: List[Type])                            = TupleClass.specificType(elems)
    def functionType(formals: List[Type], restpe: Type)         = FunctionClass.specificType(formals, restpe)
    def abstractFunctionType(formals: List[Type], restpe: Type) = AbstractFunctionClass.specificType(formals, restpe)

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
        if ((elemtp <:< AnyRefTpe) && !isPhantomClass(elemtp.typeSymbol)) nme.wrapRefArray
        else nme.genericWrapArray
    }

    def isTupleSymbol(sym: Symbol) = TupleClass.seq contains unspecializedSymbol(sym)
    def isFunctionSymbol(sym: Symbol) = FunctionClass.seq contains unspecializedSymbol(sym)
    def isProductNSymbol(sym: Symbol) = ProductClass.seq contains unspecializedSymbol(sym)

    def unspecializedSymbol(sym: Symbol): Symbol = {
      if (sym hasFlag SPECIALIZED) {
        // add initialization from its generic class constructor
        val genericName = nme.unspecializedName(sym.name)
        val member = sym.owner.info.decl(genericName.toTypeName)
        member
      }
      else sym
    }
    def unspecializedTypeArgs(tp: Type): List[Type] =
      (tp baseType unspecializedSymbol(tp.typeSymbolDirect)).typeArgs

    object MacroContextType {
      def unapply(tp: Type) = {
        def isOneOfContextTypes(tp: Type) =
          tp =:= BlackboxContextClass.tpe || tp =:= WhiteboxContextClass.tpe
        def isPrefix(sym: Symbol) =
          sym.allOverriddenSymbols.contains(MacroContextPrefixType)

        tp.dealias match {
          case RefinedType(List(tp), Scope(sym)) if isOneOfContextTypes(tp) && isPrefix(sym) => Some(tp)
          case tp if isOneOfContextTypes(tp) => Some(tp)
          case _ => None
        }
      }
    }

    def isMacroContextType(tp: Type) = MacroContextType.unapply(tp).isDefined

    def isWhiteboxContextType(tp: Type) =
      isMacroContextType(tp) && (tp <:< WhiteboxContextClass.tpe)

    private def macroBundleParamInfo(tp: Type) = {
      val ctor = tp.erasure.typeSymbol.primaryConstructor
      ctor.paramss match {
        case List(List(c)) =>
          val sym = c.info.typeSymbol
          val isContextCompatible = sym.isNonBottomSubClass(BlackboxContextClass) || sym.isNonBottomSubClass(WhiteboxContextClass)
          if (isContextCompatible) c.info else NoType
        case _ =>
          NoType
      }
    }

    def looksLikeMacroBundleType(tp: Type) =
      macroBundleParamInfo(tp) != NoType

    def isMacroBundleType(tp: Type) = {
      val isMonomorphic = tp.typeSymbol.typeParams.isEmpty
      val isContextCompatible = isMacroContextType(macroBundleParamInfo(tp))
      val hasSingleConstructor = !tp.declaration(nme.CONSTRUCTOR).isOverloaded
      val nonAbstract = !tp.erasure.typeSymbol.isAbstractClass
      isMonomorphic && isContextCompatible && hasSingleConstructor && nonAbstract
    }

    def isBlackboxMacroBundleType(tp: Type) = {
      val isBundle = isMacroBundleType(tp)
      val unwrappedContext = MacroContextType.unapply(macroBundleParamInfo(tp)).getOrElse(NoType)
      val isBlackbox = unwrappedContext =:= BlackboxContextClass.tpe
      isBundle && isBlackbox
    }

    def isListType(tp: Type)     = tp <:< classExistentialType(ListClass)
    def isIterableType(tp: Type) = tp <:< classExistentialType(IterableClass)

    // These "direct" calls perform no dealiasing. They are most needed when
    // printing types when one wants to preserve the true nature of the type.
    def isFunctionTypeDirect(tp: Type) = !tp.isHigherKinded && isFunctionSymbol(tp.typeSymbolDirect)
    def isTupleTypeDirect(tp: Type)    = !tp.isHigherKinded && isTupleSymbol(tp.typeSymbolDirect)

    // Note that these call .dealiasWiden and not .normalize, the latter of which
    // tends to change the course of events by forcing types.
    def isFunctionType(tp: Type)       = isFunctionTypeDirect(tp.dealiasWiden)

    // the number of arguments expected by the function described by `tp` (a FunctionN or SAM type),
    // or `-1` if `tp` does not represent a function type or SAM
    // for use during typers (after fields, samOf will be confused by abstract accessors for trait fields)
    def functionArityFromType(tp: Type) = {
      val dealiased = tp.dealiasWiden
      if (isFunctionTypeDirect(dealiased)) dealiased.typeArgs.length - 1
      else samOf(tp) match {
        case samSym if samSym.exists => samSym.info.params.length
        case _ => -1
      }
    }

    // the argument types expected by the function described by `tp` (a FunctionN or SAM type),
    // or `Nil` if `tp` does not represent a function type or SAM (or if it happens to be Function0...)
    def functionOrSamArgTypes(tp: Type): List[Type] = {
      val dealiased = tp.dealiasWiden
      if (isFunctionTypeDirect(dealiased)) dealiased.typeArgs.init
      else samOf(tp) match {
        case samSym if samSym.exists => tp.memberInfo(samSym).paramTypes
        case _ => Nil
      }
    }

    // the SAM's parameters and the Function's formals must have the same length
    // (varargs etc don't come into play, as we're comparing signatures, not checking an application)
    def samMatchesFunctionBasedOnArity(sam: Symbol, formals: List[Any]): Boolean =
      sam.exists && sameLength(sam.info.params, formals)

    def isTupleType(tp: Type)          = isTupleTypeDirect(tp.dealiasWiden)
    def tupleComponents(tp: Type)      = tp.dealiasWiden.typeArgs

    lazy val ProductRootClass: ClassSymbol = requiredClass[scala.Product]
      def Product_productArity          = getMemberMethod(ProductRootClass, nme.productArity)
      def Product_productElement        = getMemberMethod(ProductRootClass, nme.productElement)
      def Product_iterator              = getMemberMethod(ProductRootClass, nme.productIterator)
      def Product_productPrefix         = getMemberMethod(ProductRootClass, nme.productPrefix)
      def Product_canEqual              = getMemberMethod(ProductRootClass, nme.canEqual_)

      def productProj(z:Symbol, j: Int): TermSymbol = getMemberValue(z, nme.productAccessorName(j))

    /** if tpe <: ProductN[T1,...,TN], returns List(T1,...,TN) else Nil */
    @deprecated("no longer used", "2.11.0") def getProductArgs(tpe: Type): List[Type] = tpe.baseClasses find isProductNSymbol match {
      case Some(x)  => tpe.baseType(x).typeArgs
      case _        => Nil
    }

    @deprecated("no longer used", "2.11.0") def unapplyUnwrap(tpe:Type) = tpe.finalResultType.dealiasWiden match {
      case RefinedType(p :: _, _) => p.dealiasWiden
      case tp                     => tp
    }

    def getterMemberTypes(tpe: Type, getters: List[Symbol]): List[Type] =
      getters map (m => dropNullaryMethod(tpe memberType m))

    def dropNullaryMethod(tp: Type) = tp match {
      case NullaryMethodType(restpe) => restpe
      case _                         => tp
    }

    /** An implementation of finalResultType which does only what
     *  finalResultType is documented to do. Defining it externally to
     *  Type helps ensure people can't come to depend on accidental
     *  aspects of its behavior. This is all of it!
     */
    def finalResultType(tp: Type): Type = tp match {
      case PolyType(_, restpe)       => finalResultType(restpe)
      case MethodType(_, restpe)     => finalResultType(restpe)
      case NullaryMethodType(restpe) => finalResultType(restpe)
      case _                         => tp
    }
    /** Similarly, putting all the isStable logic in one place.
     *  This makes it like 1000x easier to see the overall logic
     *  of the method.
     */
    def isStable(tp: Type): Boolean = tp match {
      case _: SingletonType                             => true
      case NoPrefix                                     => true
      case TypeRef(_, NothingClass | SingletonClass, _) => true
      case TypeRef(_, sym, _) if sym.isAbstractType     => tp.bounds.hi.typeSymbol isSubClass SingletonClass
      case TypeRef(pre, sym, _) if sym.isModuleClass    => isStable(pre)
      case TypeRef(_, _, _) if tp ne tp.dealias         => isStable(tp.dealias)
      case TypeVar(origin, _)                           => isStable(origin)
      case AnnotatedType(_, atp)                        => isStable(atp)    // Really?
      case _: SimpleTypeProxy                           => isStable(tp.underlying)
      case _                                            => false
    }
    def isVolatile(tp: Type): Boolean = {
      // need to be careful not to fall into an infinite recursion here
      // because volatile checking is done before all cycles are detected.
      // the case to avoid is an abstract type directly or
      // indirectly upper-bounded by itself. See #2918
      def isVolatileAbstractType: Boolean = {
        def sym = tp.typeSymbol
        def volatileUpperBound = isVolatile(tp.bounds.hi)
        def safeIsVolatile = (
          if (volatileRecursions < TypeConstants.LogVolatileThreshold)
            volatileUpperBound
          // we can return true when pendingVolatiles contains sym, because
          // a cycle will be detected afterwards and an error will result anyway.
          else pendingVolatiles(sym) || {
            pendingVolatiles += sym
            try volatileUpperBound finally pendingVolatiles -= sym
          }
        )
        volatileRecursions += 1
        try safeIsVolatile finally volatileRecursions -= 1
      }
      /** A refined type P1 with ... with Pn { decls } is volatile if
       *  one of the parent types Pi is an abstract type, and
       *  either i > 1, or decls or a following parent Pj, j > 1, contributes
       *  an abstract member.
       *  A type contributes an abstract member if it has an abstract member which
       *  is also a member of the whole refined type. A scope `decls` contributes
       *  an abstract member if it has an abstract definition which is also
       *  a member of the whole type.
       */
      def isVolatileRefinedType: Boolean = {
        val RefinedType(parents, decls)         = tp
        def isVisibleDeferred(m: Symbol)        = m.isDeferred && ((tp nonPrivateMember m.name).alternatives contains m)
        def contributesAbstractMembers(p: Type) = p.deferredMembers exists isVisibleDeferred
        def dropConcreteParents                 = parents dropWhile (p => !p.typeSymbol.isAbstractType)

        (parents exists isVolatile) || {
          dropConcreteParents match {
            case Nil => false
            case ps  => (ps ne parents) || (ps.tail exists contributesAbstractMembers) || (decls exists isVisibleDeferred)
          }
        }
      }

      tp match {
        case ThisType(_)                              => false
        case SingleType(_, sym)                       => isVolatile(tp.underlying) && (sym.hasVolatileType || !sym.isStable)
        case NullaryMethodType(restpe)                => isVolatile(restpe)
        case PolyType(_, restpe)                      => isVolatile(restpe)
        case TypeRef(_, _, _) if tp ne tp.dealias     => isVolatile(tp.dealias)
        case TypeRef(_, sym, _) if sym.isAbstractType => isVolatileAbstractType
        case RefinedType(_, _)                        => isVolatileRefinedType
        case TypeVar(origin, _)                       => isVolatile(origin)
        case _: SimpleTypeProxy                       => isVolatile(tp.underlying)
        case _                                        => false
      }
    }

    private[this] var volatileRecursions: Int = 0
    private[this] val pendingVolatiles = mutable.HashSet[Symbol]()
    def functionNBaseType(tp: Type): Type = tp.baseClasses find isFunctionSymbol match {
      case Some(sym) => tp baseType unspecializedSymbol(sym)
      case _         => tp
    }

    def isPartialFunctionType(tp: Type): Boolean = {
      val sym = tp.typeSymbol
      (sym eq PartialFunctionClass) || (sym eq AbstractPartialFunctionClass)
    }

    private[this] val doSam = settings.isScala212 || (settings.isScala211 && settings.Xexperimental)

    /** The single abstract method declared by type `tp` (or `NoSymbol` if it cannot be found).
     *
     * The method must be monomorphic and have exactly one parameter list.
     * The class defining the method is a supertype of `tp` that
     * has a public no-arg primary constructor and it can be subclassed (not final or sealed).
     */
    def samOf(tp: Type): Symbol = if (!doSam) NoSymbol else if (!isNonRefinementClassType(unwrapToClass(tp))) NoSymbol else {
      // look at erased type because we (only) care about what ends up in bytecode
      // (e.g., an alias type is fine as long as is compiles to a single-abstract-method)
      val tpSym: Symbol = erasure.javaErasure(tp).typeSymbol

      if (tpSym.exists && tpSym.isClass && !(tpSym hasFlag (FINAL | SEALED))
          // if tp has a constructor (its class is not a trait), it must be public and must not take any arguments
          // (implementation restriction: implicit argument lists are excluded to simplify type inference in adaptToSAM)
          && { val ctor = tpSym.primaryConstructor
            !ctor.exists || (!ctor.isOverloaded && ctor.isPublic && ctor.info.params.isEmpty && ctor.info.paramSectionCount <= 1)}
          // we won't be able to create an instance of tp if it doesn't correspond to its self type
          // (checking conformance gets complicated when tp is not fully defined, so let's just rule out self types entirely)
          && !tpSym.hasSelfType
      ) {

        // find the single abstract member, if there is one
        // don't go out requiring DEFERRED members, as you will get them even if there's a concrete override:
        //    scala> abstract class X { def m: Int }
        //    scala> class Y extends X { def m: Int = 1}
        //    scala> typeOf[Y].deferredMembers
        //    Scopes(method m, method getClass)
        //
        //    scala> typeOf[Y].members.filter(_.isDeferred)
        //    Scopes()
        // must filter out "universal" members (getClass is deferred for some reason)
        val deferredMembers = (
          tp.membersBasedOnFlags(excludedFlags = BridgeAndPrivateFlags, requiredFlags = METHOD).toList.filter(
            mem => mem.isDeferred && !isUniversalMember(mem)
          ) // TODO: test
        )

        // if there is only one, it's monomorphic and has a single argument list
        if (deferredMembers.lengthCompare(1) == 0 &&
            deferredMembers.head.typeParams.isEmpty &&
            deferredMembers.head.info.paramSectionCount == 1)
          deferredMembers.head
        else NoSymbol
      } else NoSymbol
    }

    def arrayType(arg: Type)         = appliedType(ArrayClass, arg)
    def byNameType(arg: Type)        = appliedType(ByNameParamClass, arg)
    def iteratorOfType(tp: Type)     = appliedType(IteratorClass, tp)
    def javaRepeatedType(arg: Type)  = appliedType(JavaRepeatedParamClass, arg)
    def optionType(tp: Type)         = appliedType(OptionClass, tp)
    def scalaRepeatedType(arg: Type) = appliedType(RepeatedParamClass, arg)
    def seqType(arg: Type)           = appliedType(SeqClass, arg)

    // FYI the long clunky name is because it's really hard to put "get" into the
    // name of a method without it sounding like the method "get"s something, whereas
    // this method is about a type member which just happens to be named get.
    def typeOfMemberNamedGet(tp: Type)   = typeArgOfBaseTypeOr(tp, OptionClass)(resultOfMatchingMethod(tp, nme.get)())
    def typeOfMemberNamedHead(tp: Type)  = typeArgOfBaseTypeOr(tp, SeqClass)(resultOfMatchingMethod(tp, nme.head)())
    def typeOfMemberNamedApply(tp: Type) = typeArgOfBaseTypeOr(tp, SeqClass)(resultOfMatchingMethod(tp, nme.apply)(IntTpe))
    def typeOfMemberNamedDrop(tp: Type)  = typeArgOfBaseTypeOr(tp, SeqClass)(resultOfMatchingMethod(tp, nme.drop)(IntTpe))
    def typesOfSelectors(tp: Type)       =
      if (isTupleType(tp)) tupleComponents(tp)
      else getterMemberTypes(tp, productSelectors(tp))

    // SI-8128 Still using the type argument of the base type at Seq/Option if this is an old-style (2.10 compatible)
    //         extractor to limit exposure to regressions like the reported problem with existentials.
    //         TODO fix the existential problem in the general case, see test/pending/pos/t8128.scala
    private def typeArgOfBaseTypeOr(tp: Type, baseClass: Symbol)(or: => Type): Type = (tp baseType baseClass).typeArgs match {
      case x :: Nil =>
        val x1 = x
        val x2 = repackExistential(x1)
        x2
      case _        => or
    }

    // Can't only check for _1 thanks to pos/t796.
    def hasSelectors(tp: Type) = (
         (tp.members containsName nme._1)
      && (tp.members containsName nme._2)
    )

    /** Returns the method symbols for members _1, _2, ..., _N
     *  which exist in the given type.
     */
    def productSelectors(tpe: Type): List[Symbol] = {
      def loop(n: Int): List[Symbol] = tpe member TermName("_" + n) match {
        case NoSymbol                => Nil
        case m if m.paramss.nonEmpty => Nil
        case m                       => m :: loop(n + 1)
      }
      // Since ErrorType always returns a symbol from a call to member, we
      // had better not start looking for _1, _2, etc. expecting it to run out.
      if (tpe.isErroneous) Nil else loop(1)
    }

    /** If `tp` has a term member `name`, the first parameter list of which
     *  matches `paramTypes`, and which either has no further parameter
     *  lists or only an implicit one, then the result type of the matching
     *  method. Otherwise, NoType.
     */
    def resultOfMatchingMethod(tp: Type, name: TermName)(paramTypes: Type*): Type = {
      def matchesParams(member: Symbol) = member.paramss match {
        case Nil        => paramTypes.isEmpty
        case ps :: rest => (rest.isEmpty || isImplicitParamss(rest)) && (ps corresponds paramTypes)(_.tpe =:= _)
      }
      tp member name filter matchesParams match {
        case NoSymbol => NoType
        case member   => (tp memberType member).finalResultType
      }
    }

    def ClassType(arg: Type) = if (phase.erasedTypes) ClassClass.tpe else appliedType(ClassClass, arg)

    /** Can we tell by inspecting the symbol that it will never
     *  at any phase have type parameters?
     */
    def neverHasTypeParameters(sym: Symbol) = sym match {
      case _: RefinementClassSymbol => true
      case _: ModuleClassSymbol     => true
      case _                        =>
        (
             sym.isPrimitiveValueClass
          || sym.isAnonymousClass
          || sym.initialize.isMonomorphicType
        )
    }

    def EnumType(sym: Symbol) = {
      // given (in java): "class A { enum E { VAL1 } }"
      //  - sym: the symbol of the actual enumeration value (VAL1)
      //  - .owner: the ModuleClassSymbol of the enumeration (object E)
      //  - .linkedClassOfClass: the ClassSymbol of the enumeration (class E)
      // SI-6613 Subsequent runs of the resident compiler demand the phase discipline here.
      enteringPhaseNotLaterThan(picklerPhase)(sym.owner.linkedClassOfClass).tpe
    }

    /** Given a class symbol C with type parameters T1, T2, ... Tn
     *  which have upper/lower bounds LB1/UB1, LB1/UB2, ..., LBn/UBn,
     *  returns an existential type of the form
     *
     *    C[E1, ..., En] forSome { E1 >: LB1 <: UB1 ... en >: LBn <: UBn }.
     */
    // TODO Review the way this is used. I see two potential problems:
    //  1. `existentialAbstraction` here doesn't create fresh existential type symbols, it just
    //     uses the class type parameter symbols directly as the list of quantified symbols.
    //     See SI-8244 for the trouble that this can cause.
    //     Compare with callers of `typeParamsToExistentials` (used in Java raw type handling)
    //  2. Why don't we require a prefix? Could its omission lead to wrong results in CheckabilityChecker?
    def classExistentialType(clazz: Symbol): Type =
      existentialAbstraction(clazz.typeParams, clazz.tpe_*)

    // members of class scala.Any

    // TODO these aren't final! They are now overridden in AnyRef/Object. Prior to the fix
    //      for SI-8129, they were actually *overloaded* by the members in AnyRef/Object.
    //      We should unfinalize these, override in AnyValClass, and make the overrides final.
    //      Refchecks never actually looks at these, so it's just for consistency.
    lazy val Any_==       = enterNewMethod(AnyClass, nme.EQ, AnyTpe :: Nil, BooleanTpe, FINAL)
    lazy val Any_!=       = enterNewMethod(AnyClass, nme.NE, AnyTpe :: Nil, BooleanTpe, FINAL)

    lazy val Any_equals   = enterNewMethod(AnyClass, nme.equals_, AnyTpe :: Nil, BooleanTpe)
    lazy val Any_hashCode = enterNewMethod(AnyClass, nme.hashCode_, Nil, IntTpe)
    lazy val Any_toString = enterNewMethod(AnyClass, nme.toString_, Nil, StringTpe)
    lazy val Any_##       = enterNewMethod(AnyClass, nme.HASHHASH, Nil, IntTpe, FINAL)

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
    lazy val Any_isInstanceOf = newT1NullaryMethod(AnyClass, nme.isInstanceOf_, FINAL)(_ => BooleanTpe)
    lazy val Any_asInstanceOf = newT1NullaryMethod(AnyClass, nme.asInstanceOf_, FINAL)(_.typeConstructor)

    lazy val primitiveGetClassMethods = Set[Symbol](Any_getClass, AnyVal_getClass) ++ (
      ScalaValueClasses map (_.tpe member nme.getClass_)
    )

    lazy val getClassMethods: Set[Symbol] = primitiveGetClassMethods + Object_getClass

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
          if (isPhantomClass(sym)) AnyTpe
          else if (sym.isLocalClass) erasure.intersectionDominator(tp.parents)
          else tp.widen
        )

        existentialAbstraction(
          eparams,
          ClassType((eparams.head setInfo TypeBounds.upper(upperBound)).tpe)
        )
      }
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

    /** Flatten curried parameter lists of a method type. */
    def allParameters(tpe: Type): List[Symbol] = tpe match {
      case MethodType(params, res) => params ::: allParameters(res)
      case _                       => Nil
    }

    def typeStringNoPackage(tp: Type) =
      "" + tp stripPrefix tp.typeSymbol.enclosingPackage.fullName + "."

    def briefParentsString(parents: List[Type]) =
      normalizedParents(parents) map typeStringNoPackage mkString " with "

    def parentsString(parents: List[Type]) =
      normalizedParents(parents) mkString " with "

    def valueParamsString(tp: Type) = tp match {
      case MethodType(params, _) => params map (_.defString) mkString ("(", ",", ")")
      case _                     => ""
    }

    // members of class java.lang.{ Object, String }
    lazy val Object_## = enterNewMethod(ObjectClass, nme.HASHHASH, Nil, IntTpe, FINAL)
    lazy val Object_== = enterNewMethod(ObjectClass, nme.EQ, AnyTpe :: Nil, BooleanTpe, FINAL)
    lazy val Object_!= = enterNewMethod(ObjectClass, nme.NE, AnyTpe :: Nil, BooleanTpe, FINAL)
    lazy val Object_eq = enterNewMethod(ObjectClass, nme.eq, AnyRefTpe :: Nil, BooleanTpe, FINAL)
    lazy val Object_ne = enterNewMethod(ObjectClass, nme.ne, AnyRefTpe :: Nil, BooleanTpe, FINAL)
    lazy val Object_isInstanceOf = newT1NoParamsMethod(ObjectClass, nme.isInstanceOf_Ob, FINAL | SYNTHETIC | ARTIFACT)(_ => BooleanTpe)
    lazy val Object_asInstanceOf = newT1NoParamsMethod(ObjectClass, nme.asInstanceOf_Ob, FINAL | SYNTHETIC | ARTIFACT)(_.typeConstructor)
    lazy val Object_synchronized = newPolyMethod(1, ObjectClass, nme.synchronized_, FINAL)(tps =>
      (Some(List(tps.head.typeConstructor)), tps.head.typeConstructor)
    )
    lazy val String_+ = enterNewMethod(StringClass, nme.raw.PLUS, AnyTpe :: Nil, StringTpe, FINAL)

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

    lazy val BoxedUnitClass         = requiredClass[scala.runtime.BoxedUnit]
    lazy val BoxedUnitModule        = getRequiredModule("scala.runtime.BoxedUnit")
      def BoxedUnit_UNIT            = getMemberValue(BoxedUnitModule, nme.UNIT)
      def BoxedUnit_TYPE            = getMemberValue(BoxedUnitModule, nme.TYPE_)

    // Annotation base classes
    lazy val AnnotationClass            = requiredClass[scala.annotation.Annotation]
    lazy val ClassfileAnnotationClass   = requiredClass[scala.annotation.ClassfileAnnotation]
    lazy val StaticAnnotationClass      = requiredClass[scala.annotation.StaticAnnotation]

    // Java retention annotations
    lazy val AnnotationRetentionAttr       = requiredClass[java.lang.annotation.Retention]
    lazy val AnnotationRetentionPolicyAttr = requiredClass[java.lang.annotation.RetentionPolicy]

    // Annotations
    lazy val BridgeClass                = requiredClass[scala.annotation.bridge]
    lazy val ElidableMethodClass        = requiredClass[scala.annotation.elidable]
    lazy val ImplicitNotFoundClass      = requiredClass[scala.annotation.implicitNotFound]
    lazy val ImplicitAmbiguousClass     = getClassIfDefined("scala.annotation.implicitAmbiguous")
    lazy val MigrationAnnotationClass   = requiredClass[scala.annotation.migration]
    lazy val ScalaStrictFPAttr          = requiredClass[scala.annotation.strictfp]
    lazy val SwitchClass                = requiredClass[scala.annotation.switch]
    lazy val TailrecClass               = requiredClass[scala.annotation.tailrec]
    lazy val VarargsClass               = requiredClass[scala.annotation.varargs]
    lazy val uncheckedStableClass       = requiredClass[scala.annotation.unchecked.uncheckedStable]
    lazy val uncheckedVarianceClass     = requiredClass[scala.annotation.unchecked.uncheckedVariance]

    lazy val BeanPropertyAttr           = requiredClass[scala.beans.BeanProperty]
    lazy val BooleanBeanPropertyAttr    = requiredClass[scala.beans.BooleanBeanProperty]
    lazy val CompileTimeOnlyAttr        = getClassIfDefined("scala.annotation.compileTimeOnly")
    lazy val DeprecatedAttr             = requiredClass[scala.deprecated]
    lazy val DeprecatedNameAttr         = requiredClass[scala.deprecatedName]
    lazy val DeprecatedInheritanceAttr  = requiredClass[scala.deprecatedInheritance]
    lazy val DeprecatedOverridingAttr   = requiredClass[scala.deprecatedOverriding]
    lazy val NativeAttr                 = requiredClass[scala.native]
    lazy val RemoteAttr                 = requiredClass[scala.remote]
    lazy val ScalaInlineClass           = requiredClass[scala.inline]
    lazy val ScalaNoInlineClass         = requiredClass[scala.noinline]
    lazy val SerialVersionUIDAttr       = requiredClass[scala.SerialVersionUID]
    lazy val SerialVersionUIDAnnotation = AnnotationInfo(SerialVersionUIDAttr.tpe, List(), List(nme.value -> LiteralAnnotArg(Constant(0))))
    lazy val SpecializedClass           = requiredClass[scala.specialized]
    lazy val ThrowsClass                = requiredClass[scala.throws[_]]
    lazy val TransientAttr              = requiredClass[scala.transient]
    lazy val UncheckedClass             = requiredClass[scala.unchecked]
    lazy val UncheckedBoundsClass       = getClassIfDefined("scala.reflect.internal.annotations.uncheckedBounds")
    lazy val UnspecializedClass         = requiredClass[scala.annotation.unspecialized]
    lazy val VolatileAttr               = requiredClass[scala.volatile]

    // Meta-annotations
    lazy val BeanGetterTargetClass      = requiredClass[meta.beanGetter]
    lazy val BeanSetterTargetClass      = requiredClass[meta.beanSetter]
    lazy val FieldTargetClass           = requiredClass[meta.field]
    lazy val GetterTargetClass          = requiredClass[meta.getter]
    lazy val ParamTargetClass           = requiredClass[meta.param]
    lazy val SetterTargetClass          = requiredClass[meta.setter]
    lazy val ObjectTargetClass          = requiredClass[meta.companionObject]
    lazy val ClassTargetClass           = requiredClass[meta.companionClass]
    lazy val MethodTargetClass          = requiredClass[meta.companionMethod]    // TODO: module, moduleClass? package, packageObject?
    lazy val LanguageFeatureAnnot       = requiredClass[meta.languageFeature]

    lazy val JUnitAnnotations = List("Test", "Ignore", "Before", "After", "BeforeClass", "AfterClass").map(n => getClassIfDefined("org.junit." + n))

    // Language features
    lazy val languageFeatureModule      = getRequiredModule("scala.languageFeature")

    def isMetaAnnotation(sym: Symbol): Boolean = metaAnnotations(sym) || (
      // Trying to allow for deprecated locations
      sym.isAliasType && isMetaAnnotation(sym.info.typeSymbol)
    )
    lazy val metaAnnotations: Set[Symbol] = getPackage(TermName("scala.annotation.meta")).info.members filter (_ isSubClass StaticAnnotationClass) toSet

    // According to the scala.annotation.meta package object:
    // * By default, annotations on (`val`-, `var`- or plain) constructor parameters
    // * end up on the parameter, not on any other entity. Annotations on fields
    // * by default only end up on the field.
    def defaultAnnotationTarget(t: Tree): Symbol = t match {
      case ClassDef(_, _, _, _)                                  => ClassTargetClass
      case ModuleDef(_, _, _)                                    => ObjectTargetClass
      case vd @ ValDef(_, _, _, _) if vd.symbol.isParamAccessor  => ParamTargetClass
      case vd @ ValDef(_, _, _, _) if vd.symbol.isValueParameter => ParamTargetClass
      case ValDef(_, _, _, _)                                    => FieldTargetClass
      case DefDef(_, _, _, _, _, _)                              => MethodTargetClass
      case _                                                     => GetterTargetClass
    }

    lazy val AnnotationDefaultAttr: ClassSymbol = {
      val sym = RuntimePackageClass.newClassSymbol(tpnme.AnnotationDefaultATTR, NoPosition, 0L)
      sym setInfo ClassInfoType(List(AnnotationClass.tpe), newScope, sym)
      markAllCompleted(sym)
      RuntimePackageClass.info.decls.toList.filter(_.name == sym.name) match {
        case existing :: _ =>
          existing.asInstanceOf[ClassSymbol]
        case _ =>
          RuntimePackageClass.info.decls enter sym
          // This attribute needs a constructor so that modifiers in parsed Java code make sense
          sym.info.decls enter sym.newClassConstructor(NoPosition)
          sym
      }
    }

    private def fatalMissingSymbol(owner: Symbol, name: Name, what: String = "member", addendum: String = "") = {
      throw new FatalError(owner + " does not have a " + what + " " + name + addendum)
    }

    def getLanguageFeature(name: String, owner: Symbol = languageFeatureModule): Symbol = getMember(owner, newTypeName(name))

    def termMember(owner: Symbol, name: String): Symbol = owner.info.member(newTermName(name))

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
          val flatname = tpnme.flattenedName(owner.name, name)
          getMember(pkg, flatname)
        }
        else fatalMissingSymbol(owner, name)
      }
    }
    def getMemberValue(owner: Symbol, name: Name): TermSymbol = {
      getMember(owner, name.toTermName) match {
        case x: TermSymbol => x
        case _             => fatalMissingSymbol(owner, name, "member value")
      }
    }
    def getMemberModule(owner: Symbol, name: Name): ModuleSymbol = {
      getMember(owner, name.toTermName) match {
        case x: ModuleSymbol => x
        case NoSymbol        => fatalMissingSymbol(owner, name, "member object")
        case other           => fatalMissingSymbol(owner, name, "member object", addendum = s". A symbol ${other} of kind ${other.accurateKindString} already exists.")
      }
    }
    def getTypeMember(owner: Symbol, name: Name): TypeSymbol = {
      getMember(owner, name.toTypeName) match {
        case x: TypeSymbol => x
        case _             => fatalMissingSymbol(owner, name, "type member")
      }
    }
    def getMemberClass(owner: Symbol, name: Name): ClassSymbol = {
      getMember(owner, name.toTypeName) match {
        case x: ClassSymbol => x
        case _              => fatalMissingSymbol(owner, name, "member class")
      }
    }
    def getMemberMethod(owner: Symbol, name: Name): TermSymbol = {
      getMember(owner, name.toTermName) match {
        case x: TermSymbol => x
        case _             => fatalMissingSymbol(owner, name, "method")
      }
    }

    private lazy val erasurePhase = findPhaseWithName("erasure")
    def getMemberIfDefined(owner: Symbol, name: Name): Symbol =
      // findMember considered harmful after erasure; e.g.
      //
      // scala> exitingErasure(Symbol_apply).isOverloaded
      // res27: Boolean = true
      //
      enteringPhaseNotLaterThan(erasurePhase )(
        owner.info.nonPrivateMember(name)
      )

    /** Using getDecl rather than getMember may avoid issues with
     *  OverloadedTypes turning up when you don't want them, if you
     *  know the method in question is uniquely declared in the given owner.
     */
    def getDecl(owner: Symbol, name: Name): Symbol = {
      getDeclIfDefined(owner, name) orElse fatalMissingSymbol(owner, name, "decl")
    }
    def getDeclIfDefined(owner: Symbol, name: Name): Symbol =
      owner.info.nonPrivateDecl(name)

    private def newAlias(owner: Symbol, name: TypeName, alias: Type): AliasTypeSymbol =
      owner.newAliasType(name) setInfoAndEnter alias

    private def specialPolyClass(name: TypeName, flags: Long)(parentFn: Symbol => Type): ClassSymbol = {
      val clazz   = enterNewClass(ScalaPackageClass, name, Nil)
      val tparam  = clazz.newSyntheticTypeParam("T0", flags)
      val parents = List(AnyRefTpe, parentFn(tparam))

      clazz setInfo GenPolyType(List(tparam), ClassInfoType(parents, newScope, clazz)) markAllCompleted
    }

    def newPolyMethod(typeParamCount: Int, owner: Symbol, name: TermName, flags: Long)(createFn: PolyMethodCreator): MethodSymbol = {
      val msym    = owner.newMethod(name.encode, NoPosition, flags)
      val tparams = msym.newSyntheticTypeParams(typeParamCount)
      val mtpe    = createFn(tparams) match {
        case (Some(formals), restpe) => MethodType(msym.newSyntheticValueParams(formals), restpe)
        case (_, restpe)             => NullaryMethodType(restpe)
      }

      msym setInfoAndEnter genPolyType(tparams, mtpe) markAllCompleted
    }

    /** T1 means one type parameter.
     */
    def newT1NullaryMethod(owner: Symbol, name: TermName, flags: Long)(createFn: Symbol => Type): MethodSymbol = {
      newPolyMethod(1, owner, name, flags)(tparams => (None, createFn(tparams.head)))
    }
    def newT1NoParamsMethod(owner: Symbol, name: TermName, flags: Long)(createFn: Symbol => Type): MethodSymbol = {
      newPolyMethod(1, owner, name, flags)(tparams => (Some(Nil), createFn(tparams.head)))
    }

    /** Is symbol a phantom class for which no runtime representation exists? */
    lazy val isPhantomClass = Set[Symbol](AnyClass, AnyValClass, NullClass, NothingClass)
    /** Lists core classes that don't have underlying bytecode, but are synthesized on-the-fly in every reflection universe */
    lazy val syntheticCoreClasses = List(
      AnnotationDefaultAttr, // #2264
      RepeatedParamClass,
      JavaRepeatedParamClass,
      ByNameParamClass,
      AnyClass,
      AnyRefClass,
      AnyValClass,
      NullClass,
      NothingClass,
      SingletonClass
    )
    /** Lists core methods that don't have underlying bytecode, but are synthesized on-the-fly in every reflection universe */
    lazy val syntheticCoreMethods = List(
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
    /** Lists core classes that do have underlying bytecode, but are adjusted on-the-fly in every reflection universe */
    lazy val hijackedCoreClasses = List(
      ComparableClass,
      JavaSerializableClass
    )
    /** Lists symbols that are synthesized or hijacked by the compiler.
     *
     *  Such symbols either don't have any underlying bytecode at all ("synthesized")
     *  or get loaded from bytecode but have their metadata adjusted ("hijacked").
     */
    lazy val symbolsNotPresentInBytecode = syntheticCoreClasses ++ syntheticCoreMethods ++ hijackedCoreClasses

    /** Is the symbol that of a parent which is added during parsing? */
    lazy val isPossibleSyntheticParent = ProductClass.seq.toSet[Symbol] + ProductRootClass + SerializableClass

    private lazy val boxedValueClassesSet = boxedClass.values.toSet[Symbol] + BoxedUnitClass

    /** Is symbol a value class? */
    def isPrimitiveValueClass(sym: Symbol) = ScalaValueClasses contains sym
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
      case _                  => false
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
        else if (sym.isTopLevel) sym.javaClassName
        else flatNameString(sym.owner, separator) + nme.NAME_JOIN_STRING + sym.simpleName
      def signature1(etp: Type): String = {
        if (etp.typeSymbol == ArrayClass) "[" + signature1(erasure(etp.dealiasWiden.typeArgs.head))
        else if (isPrimitiveValueClass(etp.typeSymbol)) abbrvTag(etp.typeSymbol).toString()
        else "L" + flatNameString(etp.typeSymbol, '/') + ";"
      }
      val etp = erasure(tp)
      if (etp.typeSymbol == ArrayClass) signature1(etp)
      else flatNameString(etp.typeSymbol, '.')
    }

    // documented in JavaUniverse.init
    def init() {
      if (isInitialized) return
      ObjectClass.initialize
      ScalaPackageClass.initialize
      symbolsNotPresentInBytecode
      NoSymbol
      isInitialized = true
    } //init

    class UniverseDependentTypes(universe: Tree) {
      lazy val nameType         = universeMemberType(tpnme.Name)
      lazy val modsType         = universeMemberType(tpnme.Modifiers)
      lazy val flagsType        = universeMemberType(tpnme.FlagSet)
      lazy val symbolType       = universeMemberType(tpnme.Symbol)
      lazy val treeType         = universeMemberType(tpnme.Tree)
      lazy val caseDefType      = universeMemberType(tpnme.CaseDef)
      lazy val liftableType     = universeMemberType(tpnme.Liftable)
      lazy val unliftableType   = universeMemberType(tpnme.Unliftable)
      lazy val iterableTreeType = appliedType(IterableClass, treeType)
      lazy val listTreeType     = appliedType(ListClass, treeType)
      lazy val listListTreeType = appliedType(ListClass, listTreeType)

      def universeMemberType(name: TypeName) = universe.tpe.memberType(getTypeMember(universe.symbol, name))
    }

    /** Efficient access to member symbols which must be looked up each run. Access via `currentRun.runDefinitions` */
    final class RunDefinitions {
      lazy val StringAdd_+ = getMemberMethod(StringAddClass, nme.PLUS)

      // The given symbol represents either String.+ or StringAdd.+
      // TODO: this misses Predef.any2stringadd
      def isStringAddition(sym: Symbol) = sym == String_+ || sym == StringAdd_+

      lazy val StringContext_f = getMemberMethod(StringContextClass, nme.f)

      lazy val ArrowAssocClass = getMemberClass(PredefModule, TypeName("ArrowAssoc")) // SI-5731
      def isArrowAssoc(sym: Symbol) = sym.owner == ArrowAssocClass

      lazy val Boxes_isNumberOrBool  = getDecl(BoxesRunTimeClass, nme.isBoxedNumberOrBoolean)
      lazy val Boxes_isNumber        = getDecl(BoxesRunTimeClass, nme.isBoxedNumber)

      private def valueClassCompanion(name: TermName): ModuleSymbol = {
        getMember(ScalaPackageClass, name) match {
          case x: ModuleSymbol => x
          case _               => catastrophicFailure()
        }
      }

      private def valueCompanionMember(className: Name, methodName: TermName): TermSymbol =
        getMemberMethod(valueClassCompanion(className.toTermName).moduleClass, methodName)

      lazy val boxMethod        = classesMap(x => valueCompanionMember(x, nme.box))
      lazy val unboxMethod      = classesMap(x => valueCompanionMember(x, nme.unbox))
      lazy val isUnbox          = unboxMethod.values.toSet[Symbol]
      lazy val isBox            = boxMethod.values.toSet[Symbol]

      lazy val Boolean_and = definitions.Boolean_and
      lazy val Boolean_or = definitions.Boolean_or
      lazy val Boolean_not = definitions.Boolean_not

      lazy val Option_apply = getMemberMethod(OptionModule, nme.apply)
      lazy val List_apply = DefinitionsClass.this.List_apply

      /**
       * Is the given symbol `List.apply`?
       * To to avoid bootstrapping cycles, this return false if the given symbol or List itself is not initialized.
       */
      def isListApply(sym: Symbol) = sym.isInitialized && ListModule.hasCompleteInfo && sym == List_apply
      def isPredefClassOf(sym: Symbol) = if (PredefModule.hasCompleteInfo) sym == Predef_classOf else isPredefMemberNamed(sym, nme.classOf)

      lazy val TagMaterializers = Map[Symbol, Symbol](
        ClassTagClass    -> materializeClassTag,
        WeakTypeTagClass -> materializeWeakTypeTag,
        TypeTagClass     -> materializeTypeTag
      )
      lazy val TagSymbols = TagMaterializers.keySet
      lazy val Predef_conforms     = (getMemberIfDefined(PredefModule, nme.conforms)
                               orElse getMemberMethod(PredefModule, TermName("conforms"))) // TODO: predicate on -Xsource:2.10 (for now, needed for transition from M8 -> RC1)
      lazy val Predef_classOf      = getMemberMethod(PredefModule, nme.classOf)
      lazy val Predef_implicitly   = getMemberMethod(PredefModule, nme.implicitly)
      lazy val Predef_wrapRefArray = getMemberMethod(PredefModule, nme.wrapRefArray)
      lazy val Predef_???          = DefinitionsClass.this.Predef_???

      lazy val arrayApplyMethod       = getMemberMethod(ScalaRunTimeModule, nme.array_apply)
      lazy val arrayUpdateMethod      = getMemberMethod(ScalaRunTimeModule, nme.array_update)
      lazy val arrayLengthMethod      = getMemberMethod(ScalaRunTimeModule, nme.array_length)
      lazy val arrayCloneMethod       = getMemberMethod(ScalaRunTimeModule, nme.array_clone)
      lazy val ensureAccessibleMethod = getMemberMethod(ScalaRunTimeModule, nme.ensureAccessible)
      lazy val arrayClassMethod       = getMemberMethod(ScalaRunTimeModule, nme.arrayClass)
      lazy val traversableDropMethod  = getMemberMethod(ScalaRunTimeModule, nme.drop)

      lazy val GroupOfSpecializable = getMemberClass(SpecializableModule, tpnme.Group)

      lazy val WeakTypeTagClass = TypeTagsClass.map(sym => getMemberClass(sym, tpnme.WeakTypeTag))
      lazy val WeakTypeTagModule = TypeTagsClass.map(sym => getMemberModule(sym, nme.WeakTypeTag))
      lazy val TypeTagClass = TypeTagsClass.map(sym => getMemberClass(sym, tpnme.TypeTag))
      lazy val TypeTagModule = TypeTagsClass.map(sym => getMemberModule(sym, nme.TypeTag))
      lazy val MacroContextUniverse = DefinitionsClass.this.MacroContextUniverse

      lazy val materializeClassTag    = getMemberMethod(ReflectPackage, nme.materializeClassTag)
      lazy val materializeWeakTypeTag = ReflectApiPackage.map(sym => getMemberMethod(sym, nme.materializeWeakTypeTag))
      lazy val materializeTypeTag     = ReflectApiPackage.map(sym => getMemberMethod(sym, nme.materializeTypeTag))

      lazy val experimentalModule         = getMemberModule(languageFeatureModule, nme.experimental)
      lazy val MacrosFeature              = getLanguageFeature("macros", experimentalModule)
      lazy val DynamicsFeature            = getLanguageFeature("dynamics")
      lazy val PostfixOpsFeature          = getLanguageFeature("postfixOps")
      lazy val ReflectiveCallsFeature     = getLanguageFeature("reflectiveCalls")
      lazy val ImplicitConversionsFeature = getLanguageFeature("implicitConversions")
      lazy val HigherKindsFeature         = getLanguageFeature("higherKinds")
      lazy val ExistentialsFeature        = getLanguageFeature("existentials")

      lazy val ApiUniverseReify = ApiUniverseClass.map(sym => getMemberMethod(sym, nme.reify))

      lazy val ReflectRuntimeUniverse      = DefinitionsClass.this.ReflectRuntimeUniverse
      lazy val ReflectRuntimeCurrentMirror = DefinitionsClass.this.ReflectRuntimeCurrentMirror

      lazy val TreesTreeType         = TreesClass.map(sym => getTypeMember(sym, tpnme.Tree))
      object TreeType { def unapply(tpe: Type): Boolean = tpe.typeSymbol.overrideChain contains TreesTreeType }
      object SubtreeType { def unapply(tpe: Type): Boolean = tpe.typeSymbol.overrideChain exists (_.tpe <:< TreesTreeType.tpe) }

      object ExprClassOf { def unapply(tp: Type): Option[Type] = elementExtractOption(ExprClass, tp) }

      lazy val PartialManifestClass  = getTypeMember(ReflectPackage, tpnme.ClassManifest)
      lazy val ManifestSymbols = Set[Symbol](PartialManifestClass, FullManifestClass, OptManifestClass)

      def isPolymorphicSignature(sym: Symbol) = PolySigMethods(sym)
      private lazy val PolySigMethods: Set[Symbol] = Set[Symbol](MethodHandle.info.decl(sn.Invoke), MethodHandle.info.decl(sn.InvokeExact)).filter(_.exists)

      lazy val Scala_Java8_CompatPackage = rootMirror.getPackageIfDefined("scala.runtime.java8")
    }
  }
}
