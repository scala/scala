/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
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

import scala.annotation.{meta, migration, nowarn, tailrec}
import scala.collection.mutable
import Flags._
import scala.reflect.api.{Universe => ApiUniverse}
import PartialFunction.cond

trait Definitions extends api.StandardDefinitions {
  self: SymbolTable =>

  import rootMirror.{getModuleByName, getPackage, getClassByName, getRequiredClass, getRequiredModule, getClassIfDefined, getModuleIfDefined, getPackageIfDefined, getPackageObjectIfDefined, requiredClass, requiredModule}

  object definitions extends DefinitionsClass

  /** Since both the value parameter types and the result type may
   *  require access to the type parameter symbols, we model polymorphic
   *  creation as a function from those symbols to (formal types, result type).
   *  The Option is to distinguish between nullary methods and nilary methods.
   */
  private type PolyMethodCreator = List[Symbol] => (Option[List[Type]], Type)

  private def enterNewClass(owner: Symbol, name: TypeName, parents: List[Type], flags: Long = 0L): ClassSymbol = {
    val clazz = owner.newClassSymbol(name, NoPosition, flags)
    clazz.setInfoAndEnter(ClassInfoType(parents, newScope, clazz)).markAllCompleted()
  }
  private def newMethod(owner: Symbol, name: TermName, formals: List[Type], mkMeth: List[TermSymbol] => Type, flags: Long): MethodSymbol = {
    val msym   = owner.newMethod(name.encode, NoPosition, flags)
    val params = msym.newSyntheticValueParams(formals)
    val info = mkMeth(params)
    msym.setInfo(info).markAllCompleted()
  }
  private def enterNewMethod(owner: Symbol, name: TermName, formals: List[Type], restpe: Type, flags: Long = 0L): MethodSymbol =
    owner.info.decls enter newMethod(owner, name, formals, MethodType(_, restpe), flags)
  private def enterNewNullaryMethod(owner: Symbol, name: TermName, restpe: Type, flags: Long): MethodSymbol =
    owner.info.decls enter newMethod(owner, name, Nil, _ => NullaryMethodType(restpe), flags)

  // the scala value classes
  trait ValueClassDefinitions {
    self: DefinitionsClass =>

    import ClassfileConstants._

    private[this] val nameToWeight = Map[Name, Int](
      tpnme.Byte   -> 2,
      tpnme.Char   -> 3,
      tpnme.Short  -> 4,
      tpnme.Int    -> 12,
      tpnme.Long   -> 24,
      tpnme.Float  -> 48,
      tpnme.Double -> 96
    )

    private[this] val nameToTag = Map[Name, Char](
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
         isNumericValueClass(sub)
      && isNumericValueClass(sup)
      && (numericWeight(sup) % numericWeight(sub) == 0)
    )

    /** Is symbol a numeric value class? */
    def isNumericValueClass(sym: Symbol) = ScalaNumericValueClassesSet contains sym

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
    lazy val ScalaValueClassesSet: SymbolSet = new SymbolSet(ScalaValueClasses)
    lazy val ScalaNumericValueClassesSet: SymbolSet = new SymbolSet(ScalaNumericValueClasses)
    final class SymbolSet(syms: List[Symbol]) {
      private[this] val ids: Array[Symbol] = syms.toArray
      private[this] val commonOwner =
        if (syms.isEmpty) null else {
          val hhOwner = syms.head.rawowner
          if (syms.tail.forall(_.rawowner == hhOwner)) hhOwner else null
        }
      final def contains(sym: Symbol): Boolean = {
        if (commonOwner != null && (commonOwner ne sym.rawowner))
          return false
        val array = ids
        var i = 0
        while (i < array.length) {
          if (array(i) eq sym) return true
          i += 1
        }
        false
      }
    }
    def ScalaPrimitiveValueClasses: List[ClassSymbol] = ScalaValueClasses

    def underlyingOfValueClass(clazz: Symbol): Type =
      clazz.derivedValueClassUnbox.tpe.resultType

  }

  abstract class DefinitionsClass extends DefinitionsApi with ValueClassDefinitions {
    private[this] var isInitialized = false
    def isDefinitionsInitialized = isInitialized

    // It becomes tricky to create dedicated objects for other symbols because
    // of initialization order issues.
    lazy val JavaLangPackage      = getPackage("java.lang")
    lazy val JavaLangPackageClass = JavaLangPackage.moduleClass.asClass
    lazy val ScalaPackage         = getPackage("scala")
    lazy val ScalaPackageClass    = ScalaPackage.moduleClass.asClass
    lazy val RuntimePackage       = getPackage("scala.runtime")
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
     *    definition which supersedes the imported one, unless renamed
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

    private def fixupAsAnyTrait(tpe: Type): Type = tpe match {
      case ClassInfoType(parents, decls, clazz) =>
        if (parents.head.typeSymbol == AnyClass) tpe
        else {
          assert(parents.head.typeSymbol == ObjectClass, parents)
          ClassInfoType(AnyTpe :: parents.tail, decls, clazz)
        }
      case PolyType(tparams, restpe) => PolyType(tparams, fixupAsAnyTrait(restpe))
      case _                         => throw new MatchError(tpe)
    }

    // top types
    lazy val AnyClass    = enterNewClass(ScalaPackageClass, tpnme.Any, Nil, ABSTRACT).markAllCompleted()
    lazy val AnyRefClass = newAlias(ScalaPackageClass, tpnme.AnyRef, ObjectTpe).markAllCompleted()
    lazy val ObjectClass = getRequiredClass("java.lang.Object")

    // Cached types for core monomorphic classes
    lazy val AnyRefTpe       = AnyRefClass.tpe
    lazy val AnyTpe          = AnyClass.tpe
    lazy val AnyValTpe       = AnyValClass.tpe
    lazy val BoxedUnitTpe    = BoxedUnitClass.tpe
    lazy val NothingTpe      = NothingClass.tpe
    lazy val NullTpe         = NullClass.tpe

    /** Represents `java.lang.Object` as referenced from Scala code. */
    lazy val ObjectTpe       = ObjectClass.tpe

    /** ObjectTpeJava is a TypeRef that's structurally equal to ObjectTpe, but with its own object identity.
     *
     * When referenced from Java (source or bytecode), `Object` should be considered equal to Scala's `Any`,
     * as these types are both conceptually the top of the subtyping lattice of the respective languages.
     *
     * We use `ObjectTpeJava`'s identity to equate it, but not `ObjectTpe`, to `AnyTpe` in subtyping and type equality.
     */
    lazy val ObjectTpeJava   = new ObjectTpeJavaRef

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
      anyval.markAllCompleted()
    }).asInstanceOf[ClassSymbol]
      def AnyVal_getClass = getMemberMethod(AnyValClass, nme.getClass_)

    // bottom types
    lazy val RuntimeNothingClass = requiredClass[scala.runtime.Nothing$]
    lazy val RuntimeNullClass    = requiredClass[scala.runtime.Null$]

    sealed abstract class BottomClassSymbol(name: TypeName, parent: Symbol) extends ClassSymbol(ScalaPackageClass, NoPosition, name) {
      locally {
        this initFlags ABSTRACT | FINAL
        this setInfoAndEnter ClassInfoType(List(parent.tpe), newScope, this)
        this.markAllCompleted()
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
    lazy val IndexOutOfBoundsExceptionClass = getClassByName("java.lang.IndexOutOfBoundsException")
    lazy val InvocationTargetExceptionClass = getClassByName("java.lang.reflect.InvocationTargetException")
    lazy val MatchErrorClass                = requiredClass[MatchError]
    lazy val NonLocalReturnControlClass     = requiredClass[scala.runtime.NonLocalReturnControl[_]]
    lazy val NullPointerExceptionClass      = getClassByName("java.lang.NullPointerException")
    lazy val ThrowableClass                 = getClassByName("java.lang.Throwable")
    lazy val UninitializedErrorClass        = requiredClass[UninitializedFieldError]
    lazy val RuntimeExceptionClass          = requiredClass[RuntimeException]
    lazy val IllegalArgExceptionClass       = requiredClass[IllegalArgumentException]

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

    // Modules whose members are in the default namespace
    // scala/bug#5941: ScalaPackage and JavaLangPackage are never ever shared between mirrors
    // as a result, `Int` becomes `scala.Int` and `String` becomes `java.lang.String`
    // I could just change `isOmittablePrefix`, but there's more to it, so I'm leaving this as a todo for now
    lazy val UnqualifiedModules = List(PredefModule, ScalaPackage, JavaLangPackage)
    // Those modules and their module classes
    lazy val UnqualifiedOwners  = UnqualifiedModules.toSet ++ UnqualifiedModules.map(_.moduleClass)

    lazy val PredefModule               = requiredModule[scala.Predef.type]
         def Predef_???                 = getMemberMethod(PredefModule, nme.???)
         def Predef_locally             = getMemberMethod(PredefModule, nme.locally)
    def isPredefMemberNamed(sym: Symbol, name: Name) = (
      (sym.name == name) && (sym.owner == PredefModule.moduleClass)
    )

    def wrapVarargsArrayMethod(tp: Type) = getMemberMethod(ScalaRunTimeModule, wrapVarargsArrayMethodName(tp))

    /** Specialization.
     */
    lazy val SpecializableModule  = requiredModule[Specializable]

    lazy val ScalaRunTimeModule = requiredModule[scala.runtime.ScalaRunTime.type]
    lazy val SymbolModule       = requiredModule[scala.Symbol.type]
         def Symbol_apply       = getMemberMethod(SymbolModule, nme.apply)

    // classes with special meanings
    lazy val ScalaNumberClass  = requiredClass[scala.math.ScalaNumber]
    lazy val DelayedInitClass  = requiredClass[scala.DelayedInit]: @nowarn("cat=deprecation")
         def delayedInitMethod = getMemberMethod(DelayedInitClass, nme.delayedInit)

    lazy val TypeConstraintClass     = requiredClass[scala.annotation.TypeConstraint]
    lazy val SingletonClass          = enterNewClass(ScalaPackageClass, tpnme.Singleton, AnyTpe :: Nil, ABSTRACT | TRAIT | FINAL).markAllCompleted()
    lazy val ListOfSingletonClassTpe = SingletonClass.tpe :: Nil
    lazy val SerializableClass       = requiredClass[java.io.Serializable] modifyInfo fixupAsAnyTrait
    lazy val ComparableClass         = requiredClass[java.lang.Comparable[_]] modifyInfo fixupAsAnyTrait
    lazy val JavaCloneableClass      = requiredClass[java.lang.Cloneable] modifyInfo fixupAsAnyTrait
    lazy val JavaNumberClass         = requiredClass[java.lang.Number]
    lazy val JavaEnumClass           = requiredClass[java.lang.Enum[_]]
    lazy val JavaUtilMap             = requiredClass[java.util.Map[_, _]]
    lazy val JavaUtilHashMap         = requiredClass[java.util.HashMap[_, _]]
    lazy val JavaRecordClass         = getClassIfDefined("java.lang.Record")

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
    def isTypeTestSymbol(sym: Symbol)      = sym == Any_isInstanceOf || sym == Object_isInstanceOf

    def isJavaVarArgsMethod(m: Symbol)      = m.isMethod && (m.rawInfo match {
      case completer: LazyType => completer.isJavaVarargsMethod
      case _ => isJavaVarArgs(m.info.params)
    })
    def isJavaVarArgs(params: scala.collection.Seq[Symbol])  = !params.isEmpty && isJavaRepeatedParamType(params.last.tpe)
    def isScalaVarArgs(params: scala.collection.Seq[Symbol]) = !params.isEmpty && isScalaRepeatedParamType(params.last.tpe)
    def isVarArgsList(params: scala.collection.Seq[Symbol])  = !params.isEmpty && isRepeatedParamType(params.last.tpe)
    def isVarArgTypes(formals: scala.collection.Seq[Type])   = !formals.isEmpty && isRepeatedParamType(formals.last)

    def firstParamType(tpe: Type): Type = tpe.paramTypes match {
      case p :: _ => p
      case _      => NoType
    }
    def isImplicitParamss(paramss: List[List[Symbol]]) = paramss match {
      case (p :: _) :: _ => p.isImplicit
      case _             => false
    }

    @tailrec
    final def hasRepeatedParam(tp: Type): Boolean = tp match {
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
    def repeatedToSingle(tp: Type): Type               = elementExtract(RepeatedParamClass, tp) orElse elementExtract(JavaRepeatedParamClass, tp) orElse tp
     // We don't need to deal with JavaRepeatedParamClass here, as `repeatedToSeq` is only called in the patmat translation for Scala sources.
    def repeatedToSeq(tp: Type): Type                  = elementTransform(RepeatedParamClass, tp)(seqType) orElse tp
    def seqToRepeated(tp: Type): Type                  = elementTransform(SeqClass, tp)(scalaRepeatedType) orElse tp
    def isReferenceArray(tp: Type)                     = elementTest(ArrayClass, tp)(elemtp => elemtp <:< AnyRefTpe || (elemtp eq ObjectTpeJava))
    def isArrayOfSymbol(tp: Type, elem: Symbol)        = elementTest(ArrayClass, tp)(_.typeSymbol == elem)
    def elementType(container: Symbol, tp: Type): Type = elementExtract(container, tp)

    // Classes treated specially with respect to -Ywarn-unused
    lazy val SubTypeClass       = requiredClass[scala.<:<[_,_]]
    lazy val SameTypeClass      = requiredClass[scala.=:=[_,_]]
    lazy val DummyImplicitClass = requiredClass[scala.DummyImplicit]

    // collections classes
    lazy val ConsClass              = requiredClass[scala.collection.immutable.::[_]]
    lazy val IteratorClass          = requiredClass[scala.collection.Iterator[_]]
    lazy val IterableClass          = requiredClass[scala.collection.Iterable[_]]
    lazy val ListClass              = requiredClass[scala.collection.immutable.List[_]]
         def List_cons              = getMemberMethod(ListClass, nme.CONS)
    @migration("SeqClass now refers to scala.collection.immutable.Seq", "2.13.0")
    lazy val SeqClass               = requiredClass[scala.collection.immutable.Seq[_]]
    lazy val SeqFactoryClass        = requiredModule[scala.collection.SeqFactory.type]
    lazy val UnapplySeqWrapperClass = getTypeMember(SeqFactoryClass, tpnme.UnapplySeqWrapper)

    lazy val JavaStringBuilderClass = requiredClass[java.lang.StringBuilder]
    lazy val JavaStringBufferClass  = requiredClass[java.lang.StringBuffer]
    lazy val JavaCharSequenceClass  = requiredClass[java.lang.CharSequence]
    @deprecated("Use IterableClass instead of TraversableClass", "2.13.0")
         def TraversableClass       = IterableClass

    lazy val ListModule       = requiredModule[scala.collection.immutable.List.type]
         def List_apply       = getMemberMethod(ListModule, nme.apply)
    lazy val ListModuleAlias  = getMemberValue(ScalaPackageClass, nme.List)
    lazy val NilModule        = requiredModule[scala.collection.immutable.Nil.type]
    @migration("SeqModule now refers to scala.collection.immutable.Seq", "2.13.0")
    lazy val SeqModule        = requiredModule[scala.collection.immutable.Seq.type]
    lazy val SeqModuleAlias   = getMemberValue(ScalaPackageClass, nme.Seq)
    lazy val Collection_SeqModule       = requiredModule[scala.collection.Seq.type]

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
    lazy val MethodClass            = getClassByName("java.lang.reflect.Method")
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
    lazy val ScalaXmlPackage  = getPackageIfDefined("scala.xml")

    // scala.reflect
    lazy val ReflectPackage              = requiredModule[scala.reflect.`package`.type]
    lazy val ReflectApiPackage           = getPackageObjectIfDefined("scala.reflect.api") // defined in scala-reflect.jar, so we need to be careful
    lazy val ReflectRuntimePackage       = getPackageObjectIfDefined("scala.reflect.runtime") // defined in scala-reflect.jar, so we need to be careful
         def ReflectRuntimeUniverse      = ReflectRuntimePackage.map(sym => getDeclValue(sym, nme.universe))
         def ReflectRuntimeCurrentMirror = ReflectRuntimePackage.map(sym => getDeclMethod(sym, nme.currentMirror))

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
    lazy val ApiQuasiquotesClass   = getClassIfDefined("scala.reflect.api.Quasiquotes") // defined in scala-reflect.jar, so we need to be careful
    lazy val JavaUniverseClass     = getClassIfDefined("scala.reflect.api.JavaUniverse") // defined in scala-reflect.jar, so we need to be careful

    lazy val MirrorClass           = getClassIfDefined("scala.reflect.api.Mirror") // defined in scala-reflect.jar, so we need to be careful

    lazy val TypeCreatorClass      = getClassIfDefined("scala.reflect.api.TypeCreator") // defined in scala-reflect.jar, so we need to be careful
    lazy val TreeCreatorClass      = getClassIfDefined("scala.reflect.api.TreeCreator") // defined in scala-reflect.jar, so we need to be careful

    lazy val BlackboxContextClass         = getClassIfDefined("scala.reflect.macros.blackbox.Context") // defined in scala-reflect.jar, so we need to be careful

    lazy val WhiteboxContextClass         = getClassIfDefined("scala.reflect.macros.whitebox.Context") // defined in scala-reflect.jar, so we need to be careful
         def MacroContextPrefix           = BlackboxContextClass.map(sym => getMemberMethod(sym, nme.prefix))
         def MacroContextPrefixType       = BlackboxContextClass.map(sym => getTypeMember(sym, tpnme.PrefixType))
         def MacroContextUniverse         = BlackboxContextClass.map(sym => getMemberMethod(sym, nme.universe))
         def MacroContextExprClass        = BlackboxContextClass.map(sym => getTypeMember(sym, tpnme.Expr))
         def MacroContextWeakTypeTagClass = BlackboxContextClass.map(sym => getTypeMember(sym, tpnme.WeakTypeTag))
         def MacroContextTreeType         = BlackboxContextClass.map(sym => getTypeMember(sym, tpnme.Tree))
    lazy val MacroImplAnnotation          = requiredClass[scala.reflect.macros.internal.macroImpl]

    /**Implementation of a class that is identical to `scala.reflect.macros.internal.macroImpl`,
     * but only exists at compile time
     */
    lazy val MacroImplLocationAnnotation = {
      val internalPkg       = MacroImplAnnotation.owner.suchThat(_.isPackageClass)
      val MacroImplLocation = internalPkg.newClassSymbol(tpnme.macroImplLocation, NoPosition)
      MacroImplLocation.setPrivateWithin(ScalaPackage)
      MacroImplLocation.setInfoAndEnter(ClassInfoType(AnnotationClass.tpe :: Nil, newScope, MacroImplLocation))
      // getter
      MacroImplLocation.newMethod(
        name     = nme.unpickledMacroImpl,
        newFlags = STABLE | ACCESSOR | PARAMACCESSOR
      ).setInfoAndEnter(internal.nullaryMethodType(AnyTpe)).markAllCompleted()
      // field
      MacroImplLocation.newValue(
        name     = nme.unpickledMacroImpl,
        newFlags = PRIVATE | LOCAL | PARAMACCESSOR
      ).setInfoAndEnter(AnyTpe).markAllCompleted()
      // ctor
      val ctor  = MacroImplLocation.newConstructor(NoPosition)
      val param = ctor.newValueParameter(nme.unpickledMacroImpl).setInfo(AnyTpe)
      ctor.setInfoAndEnter(MethodType(param :: Nil, MacroImplLocation.tpe)).markAllCompleted()
      MacroImplLocation.addAnnotation(
        sym = CompileTimeOnlyAttr,
        arg = Literal(Constant(
          s"illegal reference to $MacroImplLocation, it is an implementation detail of unpickling TASTy"))
      )
      MacroImplLocation.markAllCompleted()
    }

    lazy val StringContextClass           = requiredClass[scala.StringContext]
    lazy val StringContextModule          = requiredModule[scala.StringContext.type]

    lazy val ValueOfClass                 = getClassIfDefined("scala.ValueOf")

    // scala/bug#8392 a reflection universe on classpath may not have
    // quasiquotes, if e.g. crosstyping with -Xsource on
    lazy val QuasiquoteClass             = if (ApiUniverseClass != NoSymbol) ApiQuasiquotesClass.info.decl(tpnme.Quasiquote) else NoSymbol
    lazy val QuasiquoteClass_api         = if (QuasiquoteClass != NoSymbol) QuasiquoteClass.info.decl(tpnme.api) else NoSymbol
    lazy val QuasiquoteClass_api_apply   = if (QuasiquoteClass_api != NoSymbol) getDeclMethod(QuasiquoteClass_api, nme.apply) else NoSymbol
    lazy val QuasiquoteClass_api_unapply = if (QuasiquoteClass_api != NoSymbol) getDeclMethod(QuasiquoteClass_api, nme.unapply) else NoSymbol

    lazy val ScalaSignatureAnnotation = requiredClass[scala.reflect.ScalaSignature]
    lazy val ScalaLongSignatureAnnotation = requiredClass[scala.reflect.ScalaLongSignature]

    lazy val MethodHandleClass = getClassIfDefined("java.lang.invoke.MethodHandle")
    lazy val VarHandleClass = getClassIfDefined("java.lang.invoke.VarHandle")

    // Option classes
    lazy val OptionClass: ClassSymbol   = requiredClass[Option[_]]
    lazy val OptionModule: ModuleSymbol = requiredModule[scala.Option.type]
    lazy val SomeClass: ClassSymbol     = requiredClass[Some[_]]
    lazy val NoneModule: ModuleSymbol   = requiredModule[scala.None.type]
    lazy val SomeModule: ModuleSymbol   = requiredModule[scala.Some.type]

    // Serialization
    lazy val ModuleSerializationProxyClass: ClassSymbol = requiredClass[scala.runtime.ModuleSerializationProxy]

    def compilerTypeFromTag(tt: ApiUniverse # WeakTypeTag[_]): Type = tt.in(rootMirror).tpe
    def compilerSymbolFromTag(tt: ApiUniverse # WeakTypeTag[_]): Symbol = tt.in(rootMirror).tpe.typeSymbol

    // The given symbol is a method with the right name and signature to be a runnable java program.
    def isJavaMainMethod(sym: Symbol) = (sym.name == nme.main) && (sym.info match {
      case MethodType(p :: Nil, restpe) => isArrayOfSymbol(p.tpe, StringClass) && restpe.typeSymbol == UnitClass
      case _                            => false
    })
    // The given class has a main method.
    def hasJavaMainMethod(sym: Symbol): Boolean = sym.tpe.member(nme.main).alternatives.exists(isJavaMainMethod)

    class VarArityClass(name: String, maxArity: Int, countFrom: Int = 0, init: Option[ClassSymbol] = None) extends VarArityClassApi {
      private[this] val offset = countFrom - init.size
      private def isDefinedAt(i: Int) = i < seq.length + offset && i >= offset
      val seq: IndexedSeq[ClassSymbol] = (init ++: countFrom.to(maxArity).map { i => getRequiredClass("scala." + name + i) }).toVector
      private[this] val symSet = new SymbolSet(seq.toList)
      def contains(sym: Symbol): Boolean = symSet.contains(sym)
      def apply(i: Int) = if (isDefinedAt(i)) seq(i - offset) else NoSymbol
      def specificType(args: List[Type], others: List[Type] = Nil): Type = {
        val arity = args.length
        if (!isDefinedAt(arity)) NoType
        else appliedType(apply(arity), args ::: others)
      }
    }
    // would be created synthetically for the default args. We call all objects in this method from the generated code
    // in JavaUniverseForce, so it is clearer to define this explicitly define this in source.
    object VarArityClass

    val MaxTupleArity, MaxProductArity, MaxFunctionArity = 22
    // A unit test checks these are kept in synch with the library.
    val MaxTupleAritySpecialized, MaxProductAritySpecialized, MaxFunctionAritySpecialized = 2

    lazy val ProductClass          = new VarArityClass("Product", MaxProductArity, countFrom = 1, init = Some(UnitClass))
    lazy val TupleClass            = new VarArityClass("Tuple", MaxTupleArity, countFrom = 1)
    lazy val FunctionClass         = new VarArityClass("Function", MaxFunctionArity)
    lazy val AbstractFunctionClass = new VarArityClass("runtime.AbstractFunction", MaxFunctionArity)

    /** Creators for TupleN, ProductN, FunctionN. */
    def tupleType(elems: List[Type])                            = TupleClass.specificType(elems)
    def functionType(formals: List[Type], restpe: Type)         = FunctionClass.specificType(formals, restpe :: Nil)
    def abstractFunctionType(formals: List[Type], restpe: Type) = AbstractFunctionClass.specificType(formals, restpe :: Nil)

    def wrapVarargsArrayMethodName(elemtp: Type): TermName = elemtp.typeSymbol match {
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

    def isTupleSymbol(sym: Symbol) = TupleClass contains unspecializedSymbol(sym)
    def isFunctionSymbol(sym: Symbol) = FunctionClass contains unspecializedSymbol(sym)
    def isAbstractFunctionSymbol(sym: Symbol) = AbstractFunctionClass contains unspecializedSymbol(sym)
    def isProductNSymbol(sym: Symbol) = ProductClass contains unspecializedSymbol(sym)

    lazy val TryClass = requiredClass[scala.util.Try[_]]
    lazy val FailureClass = requiredClass[scala.util.Failure[_]]
    lazy val SuccessClass = requiredClass[scala.util.Success[_]]
    lazy val FutureClass = requiredClass[scala.concurrent.Future[_]]
    lazy val PromiseClass = requiredClass[scala.concurrent.Promise[_]]
    lazy val NonFatalClass = requiredClass[scala.util.control.NonFatal.type]

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

    def isListType(tp: Type)     = tp.typeSymbol.isNonBottomSubClass(ListClass)
    def isIterableType(tp: Type) = tp.typeSymbol.isNonBottomSubClass(IterableClass)

    // These "direct" calls perform no dealiasing. They are most needed when
    // printing types when one wants to preserve the true nature of the type.
    def isFunctionTypeDirect(tp: Type) = !tp.isHigherKinded && isFunctionSymbol(tp.typeSymbolDirect)
    def isTupleTypeDirect(tp: Type)    = !tp.isHigherKinded && isTupleSymbol(tp.typeSymbolDirect)

    // Note that these call .dealiasWiden and not .normalize, the latter of which
    // tends to change the course of events by forcing types.
    def isFunctionType(tp: Type)       = isFunctionTypeDirect(tp.dealiasWiden)

    // Are we expecting something function-ish? This considers FunctionN / SAM / ProtoType that matches functions
    def isFunctionProto(pt: Type): Boolean =
      (isFunctionType(pt)
       || (pt match { case pt: ProtoType => pt.expectsFunctionType  case _ => false }) // TODO: this does not work for Function0
       || samOf(pt).exists
      )

    // @requires pt.typeSymbol == PartialFunctionClass
    def partialFunctionArgResTypeFromProto(pt: Type): (Type, Type) =
      pt match {
        case oap: OverloadedArgProto => (oap.hofParamTypes.head, WildcardType)
        case _                       =>
          val arg :: res :: Nil = pt.baseType(PartialFunctionClass).typeArgs: @unchecked
          (arg, res)
      }

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

    // the argument types expected by the function described by `tp` (a FunctionN or PartialFunction or SAM type),
    // or `Nil` if `tp` does not represent a function type or PartialFunction or SAM (or if it happens to be Function0...)
    def functionOrPfOrSamArgTypes(tp: Type): List[Type] = {
      val dealiased = tp.dealiasWiden
      if (isFunctionTypeDirect(dealiased) || isPartialFunctionType(dealiased)) dealiased.typeArgs.init
      else samOf(tp) match {
        case samSym if samSym.exists => tp.memberInfo(samSym).paramTypes
        case _ => Nil
      }
    }

    /**
      * Convert a SAM type to the corresponding FunctionType,
      * extrapolating BoundedWildcardTypes in the process
      * (no type precision is lost by the extrapolation,
      *  but this facilitates dealing with the types arising from Java's use-site variance).
      */
    def samToFunctionType(tp: Type, sam: Symbol = NoSymbol): Type =
      tp match {
        case pt: ProtoType => pt.asFunctionType
        case _ =>
          val samSym = sam orElse samOf(tp)

          def correspondingFunctionSymbol = {
            val numVparams = samSym.info.params.length
            if (numVparams > definitions.MaxFunctionArity) NoSymbol
            else FunctionClass(numVparams)
          }

          if (samSym.exists && tp.typeSymbol != correspondingFunctionSymbol) // don't treat Functions as SAMs
            wildcardExtrapolation(methodToExpressionTp(tp memberInfo samSym))
          else NoType
      }

    /** Automatically perform the following conversions on expression types:
      *  A method type becomes the corresponding function type.
      *  A nullary method type becomes its result type.
      *  Implicit parameters are skipped.
      *  This method seems to be performance critical.
      */
    final def methodToExpressionTp(tp: Type): Type = tp match {
      case PolyType(_, restpe) =>
        logResult(sm"""|Normalizing PolyType in infer:
                       |  was: $restpe
                       |  now""")(methodToExpressionTp(restpe))
      case mt @ MethodType(_, restpe) if mt.isImplicit             => methodToExpressionTp(restpe)
      case mt @ MethodType(_, restpe) if !mt.isDependentMethodType =>
        if (phase.erasedTypes) FunctionClass(mt.params.length).tpe
        else functionType(mt.paramTypes, methodToExpressionTp(restpe))
      case NullaryMethodType(restpe)                               => methodToExpressionTp(restpe)
      case ExistentialType(tparams, qtpe)                          => newExistentialType(tparams, methodToExpressionTp(qtpe))
      case _                                                       => tp // @MAT aliases already handled by subtyping
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
      def Product_productElementName        = getMemberIfDefined(ProductRootClass, nme.productElementName)
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

    def dropNullaryMethod(tp: Type) = tp match {
      case NullaryMethodType(restpe) => restpe
      case _                         => tp
    }

    /** An implementation of finalResultType which does only what
     *  finalResultType is documented to do. Defining it externally to
     *  Type helps ensure people can't come to depend on accidental
     *  aspects of its behavior. This is all of it!
     */
    @tailrec
    final def finalResultType(tp: Type): Type = tp match {
      case PolyType(_, restpe)       => finalResultType(restpe)
      case MethodType(_, restpe)     => finalResultType(restpe)
      case NullaryMethodType(restpe) => finalResultType(restpe)
      case _                         => tp
    }
    /** Similarly, putting all the isStable logic in one place.
     *  This makes it like 1000x easier to see the overall logic
     *  of the method.
     */
    @tailrec
    final def isStable(tp: Type): Boolean = tp match {
      case NoPrefix | _: SingletonType                  => true
      case TypeRef(_, NothingClass | SingletonClass, _) => true
      case TypeRef(_, sym, _) if sym.isAbstractType     => tp.upperBound.typeSymbol.isSubClass(SingletonClass)
      case TypeRef(pre, sym, _) if sym.isModuleClass    => isStable(pre)
      case _: TypeRef                                   => val norm = tp.normalize; (norm ne tp) && isStable(norm)
      case TypeVar(origin, _)                           => isStable(origin)
      case ExistentialType(qs, underlying)              => isStable(deriveTypeWithWildcards(qs)(underlying))
      case _: SimpleTypeProxy                           => isStable(tp.underlying)
      case _                                            => false
    }
    final def isVolatile(tp: Type): Boolean = {
      // need to be careful not to fall into an infinite recursion here
      // because volatile checking is done before all cycles are detected.
      // the case to avoid is an abstract type directly or
      // indirectly upper-bounded by itself. See #2918
      def isVolatileAbstractType: Boolean = {
        def sym = tp.typeSymbol
        def volatileUpperBound = isVolatile(tp.upperBound)
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
      /*  A refined type P1 with ... with Pn { decls } is volatile if
       *  one of the parent types Pi is an abstract type, and
       *  either i > 1, or decls or a following parent Pj, j > 1, contributes
       *  an abstract member.
       *  A type contributes an abstract member if it has an abstract member which
       *  is also a member of the whole refined type. A scope `decls` contributes
       *  an abstract member if it has an abstract definition which is also
       *  a member of the whole type.
       */
      def isVolatileRefinedType: Boolean = {
        val RefinedType(parents, decls)         = (tp: @unchecked)
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
      def isVolatileTypeRef(tr: TypeRef) = {
        val dealised = tr.dealias
        if (dealised ne tr) isVolatile(dealised)
        else if (tr.sym.isAbstractType) isVolatileAbstractType
        else false
      }

      tp match {
        case ThisType(_)                              => false
        case SingleType(_, sym)                       => isVolatile(tp.underlying) && (sym.hasVolatileType || !sym.isStable)
        case NullaryMethodType(restpe)                => isVolatile(restpe)
        case PolyType(_, restpe)                      => isVolatile(restpe)
        case tr: TypeRef                              => isVolatileTypeRef(tr)
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

    private[this] val samCache = perRunCaches.newAnyRefMap[Symbol, Symbol]()
    /** The single abstract method declared by type `tp` (or `NoSymbol` if it cannot be found).
      *
      * The method must be monomorphic and have exactly one parameter list.
      * The class defining the method is a supertype of `tp` that
      * has a public no-arg primary constructor and it can be subclassed (not final or sealed).
      *
      * Note that this is also used during erasure (TODO: maybe we could simplify typedFunction for post-typer usage and avoid this?),
      * and the caching means that samOf is effectively computed during typer (assuming the same inputs were presented to samOf during that phase).
      * It's kind of strange that erasure sees deferredMembers that typer does not (see commented out assert below)
      */
    def samOf(tp: Type): Symbol =
      if (isNonRefinementClassType(unwrapToClass(tp))) { // TODO: is this really faster than computing tpSym below? how about just `tp.typeSymbol.isClass` (and !tpSym.isRefinementClass)?
        // look at erased type because we (only) care about what ends up in bytecode
        // (e.g., an alias type is fine as long as is compiles to a single-abstract-method)
        val tpSym: Symbol = erasure.javaErasure(tp).typeSymbol

        def compute: Symbol = {
          if (tpSym.exists && tpSym.isClass && !(tpSym hasFlag (FINAL | SEALED))
              // if tp has a constructor (its class is not a trait), it must be public and must not take any arguments
              // (implementation restriction: implicit argument lists are excluded to simplify type inference in adaptToSAM)
              && {
                val ctor = tpSym.primaryConstructor
                !ctor.exists || (!ctor.isOverloaded && ctor.isPublic && ctor.info.params.isEmpty && ctor.info.paramSectionCount <= 1)
              }
              // we won't be able to create an instance of tp if it doesn't correspond to its self type
              // (checking conformance gets complicated when tp is not fully defined, so let's just rule out self types entirely)
              && !tpSym.hasSelfType) {
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
            val deferredMembers =
              tpSym.info.membersBasedOnFlags(excludedFlags = BridgeAndPrivateFlags, requiredFlags = METHOD)
              .toList
              .filter(mem => mem.isDeferred && !isUniversalMember(mem))

            // if there is only one, it's monomorphic and has a single argument list
            if (deferredMembers.lengthCompare(1) == 0 &&
                deferredMembers.head.typeParams.isEmpty &&
                deferredMembers.head.info.paramSectionCount == 1)
              deferredMembers.head
            else NoSymbol
          } else NoSymbol
        }

        // fails in test/files/jvm/t10512b.scala
        // { val res = samCache.getOrElseUpdate(tpSym, compute); assert(compute eq res, s"samOf($tp) cache discrepancy $compute <-> $res")

        samCache.getOrElseUpdate(tpSym, compute)
      } else NoSymbol

    def samOfProto(pt: Type): Symbol =
      pt match {
        case proto: ProtoType => samOf(proto.underlying) // TODO: add more semantic accessor to ProtoType?
        case pt               => samOf(pt)
      }

    def arrayType(arg: Type)         = appliedType(ArrayClass, arg :: Nil)
    def byNameType(arg: Type)        = appliedType(ByNameParamClass, arg :: Nil)
    def iteratorOfType(tp: Type)     = appliedType(IteratorClass, tp :: Nil)
    def javaRepeatedType(arg: Type)  = appliedType(JavaRepeatedParamClass, arg :: Nil)
    def optionType(tp: Type)         = appliedType(OptionClass, tp :: Nil)
    def scalaRepeatedType(arg: Type) = appliedType(RepeatedParamClass, arg :: Nil)
    def seqType(arg: Type)           = appliedType(SeqClass, arg :: Nil)

    // For name-based pattern matching, derive the "element type" (type argument of Option/Seq)
    // from the relevant part of the signature of various members (get/head/apply/drop)
    def elementTypeFromGet(tp: Type)   = typeArgOfBaseTypeOr(tp, OptionClass)(resultOfMatchingMethod(tp, nme.get)())
    def elementTypeFromApply(tp: Type) = typeArgOfBaseTypeOr(tp, SeqClass)(resultOfMatchingMethod(tp, nme.apply)(IntTpe))
    def resultOfIsEmpty(tp: Type)      = resultOfMatchingMethod(tp, nme.isEmpty)()

    // scala/bug#8128 Still using the type argument of the base type at Seq/Option if this is an old-style (2.10 compatible)
    //         extractor to limit exposure to regressions like the reported problem with existentials.
    //         TODO fix the existential problem in the general case, see test/pending/pos/t8128.scala
    private def typeArgOfBaseTypeOr(tp: Type, baseClass: Symbol)(or: => Type): Type = (tp baseType baseClass).typeArgs match {
      case x :: Nil => repackExistential(x)
      case _        => or
    }

    /** If `tp` has a term member `name`, the first parameter list of which
     *  matches `paramTypes`, and which either has no further parameter
     *  lists or only an implicit one, then the result type of the matching
     *  method. Otherwise, NoType.
     */
    private def resultOfMatchingMethod(tp: Type, name: TermName)(paramTypes: Type*): Type = {
      def matchesParams(member: Symbol) = member.paramss match {
        case Nil        => paramTypes.isEmpty
        case ps :: rest => (rest.isEmpty || isImplicitParamss(rest)) && (ps corresponds paramTypes)(_.tpe =:= _)
      }
      tp member name filter matchesParams match {
        case NoSymbol => NoType
        case member   => (tp memberType member).finalResultType
      }
    }

    def ClassType(arg: Type) =
      if (phase.erasedTypes) ClassClass.tpe
      else appliedType(ClassClass, arg :: Nil)

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
      // scala/bug#6613 Subsequent runs of the resident compiler demand the phase discipline here.
      enteringPhaseNotLaterThan(picklerPhase)(sym.owner.linkedClassOfClass).tpe
    }

    /** Given a class symbol C with type parameters T1, T2, ... Tn
     *  which have upper/lower bounds LB1/UB1, LB2/UB2, ..., LBn/UBn,
     *  returns an existential type of the form
     *
     *    C[E1, ..., En] forSome { E1 >: LB1 <: UB1 ... En >: LBn <: UBn }.
     */
    def classExistentialType(prefix: Type, clazz: Symbol): Type = {
      val eparams = typeParamsToExistentials(clazz, clazz.unsafeTypeParams)
      newExistentialType(eparams, typeRef(prefix, clazz, eparams.map(_.tpeHK)))
    }

    // members of class scala.Any

    // TODO these aren't final! They are now overridden in AnyRef/Object. Prior to the fix
    //      for scala/bug#8129, they were actually *overloaded* by the members in AnyRef/Object.
    //      We should unfinalize these, override in AnyValClass, and make the overrides final.
    //      Refchecks never actually looks at these, so it's just for consistency.
    lazy val Any_==       = enterNewMethod(AnyClass, nme.EQ, AnyTpe :: Nil, BooleanTpe, FINAL)
    lazy val Any_!=       = enterNewMethod(AnyClass, nme.NE, AnyTpe :: Nil, BooleanTpe, FINAL)

    lazy val Any_equals   = enterNewMethod(AnyClass, nme.equals_, AnyTpe :: Nil, BooleanTpe)
    lazy val Any_hashCode = enterNewMethod(AnyClass, nme.hashCode_, Nil, IntTpe)
    lazy val Any_toString = enterNewMethod(AnyClass, nme.toString_, Nil, StringTpe)
    lazy val Any_##       = enterNewNullaryMethod(AnyClass, nme.HASHHASH, IntTpe, FINAL)

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
    lazy val Any_isInstanceOf = enterNewT1NullaryMethod(AnyClass, nme.isInstanceOf_, FINAL)(_ => BooleanTpe)
    lazy val Any_asInstanceOf = enterNewT1NullaryMethod(AnyClass, nme.asInstanceOf_, FINAL)(_.typeConstructor)

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
      case MethodType(params, _) => params.map(_.defString).mkString("(", ", ", ")")
      case _                     => ""
    }

    // members of class java.lang.{ Object, String }
    lazy val Object_## = enterNewNullaryMethod(ObjectClass, nme.HASHHASH, IntTpe, FINAL)
    lazy val Object_== = enterNewMethod(ObjectClass, nme.EQ, AnyTpe :: Nil, BooleanTpe, FINAL)
    lazy val Object_!= = enterNewMethod(ObjectClass, nme.NE, AnyTpe :: Nil, BooleanTpe, FINAL)
    lazy val Object_eq = enterNewMethod(ObjectClass, nme.eq, AnyRefTpe :: Nil, BooleanTpe, FINAL)
    lazy val Object_ne = enterNewMethod(ObjectClass, nme.ne, AnyRefTpe :: Nil, BooleanTpe, FINAL)
    lazy val Object_isInstanceOf = newT1NilaryMethod(ObjectClass, nme.isInstanceOf_Ob, FINAL | SYNTHETIC | ARTIFACT)(_ => BooleanTpe)
    lazy val Object_asInstanceOf = newT1NilaryMethod(ObjectClass, nme.asInstanceOf_Ob, FINAL | SYNTHETIC | ARTIFACT)(_.typeConstructor)
    lazy val Object_synchronized = enterNewT1Method(ObjectClass, nme.synchronized_, FINAL)(_.typeConstructor)
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
    lazy val ConstantAnnotationClass    = getClassIfDefined("scala.annotation.ConstantAnnotation")
    lazy val StaticAnnotationClass      = requiredClass[scala.annotation.StaticAnnotation]

    // Java annotation annotations
    lazy val AnnotationRetentionAttr       = requiredClass[java.lang.annotation.Retention]
    lazy val AnnotationRetentionPolicyAttr = requiredClass[java.lang.annotation.RetentionPolicy]
    lazy val AnnotationRepeatableAttr      = requiredClass[java.lang.annotation.Repeatable]

    // Annotations
    lazy val ElidableMethodClass        = requiredClass[scala.annotation.elidable]
    lazy val ImplicitNotFoundClass      = requiredClass[scala.annotation.implicitNotFound]
    lazy val ImplicitAmbiguousClass     = getClassIfDefined("scala.annotation.implicitAmbiguous")
    lazy val MigrationAnnotationClass   = requiredClass[scala.annotation.migration]
    lazy val ScalaStrictFPAttr          = requiredClass[scala.annotation.strictfp]
    lazy val SwitchClass                = requiredClass[scala.annotation.switch]
    lazy val TailrecClass               = requiredClass[scala.annotation.tailrec]
    lazy val VarargsClass               = requiredClass[scala.annotation.varargs]
    lazy val NowarnClass                = getClassIfDefined("scala.annotation.nowarn")
    lazy val uncheckedStableClass       = requiredClass[scala.annotation.unchecked.uncheckedStable]
    lazy val uncheckedVarianceClass     = requiredClass[scala.annotation.unchecked.uncheckedVariance]

    // Tasty Unpickling Helpers - only access when Scala 3 library is expected to be available
    lazy val ChildAnnotationClass        = getClassIfDefined("scala.annotation.internal.Child")
    lazy val RepeatedAnnotationClass     = getClassIfDefined("scala.annotation.internal.Repeated")
    lazy val TargetNameAnnotationClass   = getClassIfDefined("scala.annotation.targetName")
    lazy val StaticMethodAnnotationClass = getClassIfDefined("scala.annotation.static")
    lazy val PolyFunctionClass           = getClassIfDefined("scala.PolyFunction")
    lazy val ExperimentalAnnotationClass = getClassIfDefined("scala.annotation.experimental")

    lazy val BeanPropertyAttr           = requiredClass[scala.beans.BeanProperty]
    lazy val BooleanBeanPropertyAttr    = requiredClass[scala.beans.BooleanBeanProperty]
    lazy val CompileTimeOnlyAttr        = getClassIfDefined("scala.annotation.compileTimeOnly")
    lazy val DeprecatedAttr             = requiredClass[scala.deprecated]
    lazy val DeprecatedNameAttr         = requiredClass[scala.deprecatedName]
    lazy val DeprecatedInheritanceAttr  = requiredClass[scala.deprecatedInheritance]
    lazy val DeprecatedOverridingAttr   = requiredClass[scala.deprecatedOverriding]
    lazy val NativeAttr                 = requiredClass[scala.native]
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
    lazy val UnusedClass                = requiredClass[scala.annotation.unused]
    lazy val VolatileAttr               = requiredClass[scala.volatile]
    lazy val JavaDeprecatedAttr         = requiredClass[java.lang.Deprecated]
    lazy val FunctionalInterfaceClass   = requiredClass[java.lang.FunctionalInterface]

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

    // Used by macro annotations
    lazy val InheritedAttr = requiredClass[java.lang.annotation.Inherited]

    lazy val JUnitAnnotations = List("Test", "Ignore", "Before", "After", "BeforeClass", "AfterClass").map(n => getClassIfDefined("org.junit." + n))

    // Language features
    lazy val languageFeatureModule      = getRequiredModule("scala.languageFeature")

    @tailrec
    final def isMetaAnnotation(sym: Symbol): Boolean = metaAnnotations(sym) || (
      // Trying to allow for deprecated locations
      sym.isAliasType && isMetaAnnotation(sym.info.typeSymbol)
    )
    lazy val metaAnnotations: Set[Symbol] = getPackage("scala.annotation.meta").info.members.filter(_ isSubClass StaticAnnotationClass).toSet

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
      sym setInfo ClassInfoType(List(StaticAnnotationClass.tpe), newScope, sym)
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
      throw new FatalError(s"$owner does not have a $what ${name}${addendum}")
    }

    def getLanguageFeature(name: String, owner: Symbol = languageFeatureModule): Symbol = getMember(owner, newTypeName(name))

    def termMember(owner: Symbol, name: String): Symbol = owner.info.member(newTermName(name))

    def findNamedMember(fullName: Name, root: Symbol): Symbol = {
      val segs = nme.segments(fullName.toString, fullName.isTermName)
      if (segs.isEmpty || segs.head != root.simpleName) NoSymbol
      else findNamedMember(segs.tail, root)
    }
    @tailrec
    final def findNamedMember(segs: List[Name], root: Symbol): Symbol =
      if (segs.isEmpty) root
      else findNamedMember(segs.tail, root.info member segs.head)

    def getMember(owner: Symbol, name: Name): Symbol = {
      getMemberIfDefined(owner, name) orElse {
        if (phase.flatClasses && name.isTypeName && !owner.isPackageObjectOrClass) {
          val pkg = owner.owner
          val flatname = tpnme.flattenedName(owner, name)
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
      def miss = fatalMissingSymbol(owner, name, "method")
      getMember(owner, name.toTermName) match {
        case x: TermSymbol => x.filter(_.isMethod).orElse(miss).asInstanceOf[TermSymbol]
        case _             => miss
      }
    }
    def getDeclMethod(owner: Symbol, name: Name): TermSymbol = {
      getDecl(owner, name.toTermName) match {
        case x: TermSymbol => x
        case _             => fatalMissingSymbol(owner, name, "method")
      }
    }
    def getDeclValue(owner: Symbol, name: Name): TermSymbol = {
      getDecl(owner, name.toTermName) match {
        case x: TermSymbol => x
        case _             => fatalMissingSymbol(owner, name, "declared value")
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

    // TODO: this is an unfortunate trade-off: on the one hand, `T*` is not a first-class type, and it shouldn't be compatible with T.
    // This matters for overloading resolution, where a vararg method should be seen as less specific than a non-vararg one,
    // since you can pass a T to a method that expects a T*, but you can't pass a T* to a method that takes a T
    // (except if you allow converting T* to Seq[T], which should not be done through subtyping but instead using a conversion, IMO.)
    // On the other hand, inside a method body, an argument of type T* can be treated as a Seq[T].
    private def specialPolyClass(name: TypeName, flags: Long)(parentFn: Symbol => Type): ClassSymbol = {
      val clazz   = enterNewClass(ScalaPackageClass, name, Nil)
      val tparam  = clazz.newSyntheticTypeParam("T0", flags)
      val parents = List(AnyRefTpe, parentFn(tparam))

      clazz.setInfo(GenPolyType(List(tparam), ClassInfoType(parents, newScope, clazz))).markAllCompleted()
    }

    def newPolyMethod(typeParamCount: Int, owner: Symbol, name: TermName, flags: Long)(createFn: PolyMethodCreator): MethodSymbol = {
      val msym    = owner.newMethod(name.encode, NoPosition, flags)
      val tparams = msym.newSyntheticTypeParams(typeParamCount)
      val mtpe    = createFn(tparams) match {
        case (Some(formals), restpe) => MethodType(msym.newSyntheticValueParams(formals), restpe)
        case (_, restpe)             => NullaryMethodType(restpe)
      }

      msym.setInfo(genPolyType(tparams, mtpe)).markAllCompleted()
    }
    def enterNewPolyMethod(typeParamCount: Int, owner: Symbol, name: TermName, flags: Long)(createFn: PolyMethodCreator): MethodSymbol = {
      val m = newPolyMethod(typeParamCount, owner, name, flags)(createFn)
      owner.info.decls.enter(m)
      m
    }
    /** T1 means one type parameter. Nullary means no param lists.
     */
    def newT1NullaryMethod(owner: Symbol, name: TermName, flags: Long)(createFn: Symbol => Type): MethodSymbol =
      newPolyMethod(1, owner, name, flags)(tparams => (None, createFn(tparams.head)))
    def enterNewT1NullaryMethod(owner: Symbol, name: TermName, flags: Long)(createFn: Symbol => Type): MethodSymbol =
      enterNewPolyMethod(1, owner, name, flags)(tparams => (None, createFn(tparams.head)))

    /** Nilary means one empty param list. The method takes parens.
     */
    def newT1NilaryMethod(owner: Symbol, name: TermName, flags: Long)(createFn: Symbol => Type): MethodSymbol =
      newPolyMethod(1, owner, name, flags)(tparams => (util.SomeOfNil, createFn(tparams.head)))
    def enterNewT1NilaryMethod(owner: Symbol, name: TermName, flags: Long)(createFn: Symbol => Type): MethodSymbol =
      enterNewPolyMethod(1, owner, name, flags)(tparams => (util.SomeOfNil, createFn(tparams.head)))

    /** (T1) => T1.
     */
    def newT1Method(owner: Symbol, name: TermName, flags: Long)(createFn: Symbol => Type): MethodSymbol =
      newPolyMethod(1, owner, name, flags) { tparams => val t = createFn(tparams.head) ; (Some(List(t)), t) }
    def enterNewT1Method(owner: Symbol, name: TermName, flags: Long)(createFn: Symbol => Type): MethodSymbol =
      enterNewPolyMethod(1, owner, name, flags) { tparams => val t = createFn(tparams.head) ; (Some(List(t)), t) }

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
      JavaCloneableClass,
      SerializableClass
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
    def isPrimitiveValueClass(sym: Symbol) = ScalaValueClassesSet contains sym
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
    def isNumericValueType(tp: Type): Boolean = tp.widen match {
      case TypeRef(_, sym, _) => isNumericValueClass(sym)
      case _                  => false
    }

    lazy val ShowAsInfixAnnotationClass = rootMirror.getClassIfDefined("scala.annotation.showAsInfix")

    // todo: reconcile with javaSignature!!!
    def signature(tp: Type): String = {
      @tailrec def erasure(tp: Type): Type = tp match {
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
    def init(): Unit = {
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
      // The given symbol represents String.+
      // TODO: this misses Predef.any2stringadd
      def isStringAddition(sym: Symbol) = sym == String_+

      lazy val String_valueOf_Int = getMemberMethod(StringClass.companionModule, nme.valueOf).suchThat(
        x => x.paramss.head.length == 1 && x.firstParam.info.typeSymbol == IntClass)

      lazy val StringContext_f = getMemberMethod(StringContextClass, nme.f)
      lazy val StringContext_s = getMemberMethod(StringContextClass, nme.s)
      lazy val StringContext_raw = getMemberMethod(StringContextClass, nme.raw_)
      lazy val StringContext_apply = getMemberMethod(StringContextModule, nme.apply)

      lazy val ArrowAssocClass = getMemberClass(PredefModule, TypeName("ArrowAssoc")) // scala/bug#5731
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
      lazy val Option_isDefined: Symbol = getMemberMethod(OptionClass, TermName("isDefined"))
      lazy val Option_get: Symbol = getMemberMethod(OptionClass, TermName("get"))
      private lazy val List_apply = DefinitionsClass.this.List_apply
      private lazy val Seq_apply = {
        val result = getMemberMethod(DefinitionsClass.this.SeqModule, nme.apply)
        assert(result == getMemberMethod(DefinitionsClass.this.Collection_SeqModule, nme.apply), "Expected collection.Seq and immutable.Seq to have the same apply member")
        result
      }
      /* This is for translating uses of List() into Nil.
       *
       * 2.12 would see scala.collection.immutable.List.apply[Nothing]
       * 2.13 sees scala.`package`.List().apply or after typer scala.`package`.List().apply(scala.collection.immutable.Nil).$asInstanceOf[List]
       *
       * Conservative check to avoid cycles is restored.
       */
      final def isListApply(tree: Tree): Boolean =
        tree.symbol.isInitialized && ListModule.hasCompleteInfo && (tree.symbol == List_apply || tree.symbol.name == nme.apply) && cond(tree) {
          case treeInfo.Applied(Select(qual, _), _, _) =>
            treeInfo.isQualifierSafeToElide(qual) && (qual.symbol == ListModule || qual.symbol == ListModuleAlias /*|| isListAlias(qual.tpe)*/)
        }
      /*
      private def isListAlias(tpe: Type): Boolean = cond(tpe) {
        case SingleType(_, _) => tpe.widen.typeSymbol.companionSymbol == ListModule
      }
      */

      final def isSeqApply(tree: Tree): Boolean = isListApply(tree) || {
        /*
         * This is now also used for converting {Seq, List}.apply(a, b, c) to `a :: b :: c :: Nil` in CleanUp.
         */
        def isSeqFactory(sym: Symbol) = sym == SeqModule || sym == SeqModuleAlias || sym == Collection_SeqModule

        (tree.symbol == Seq_apply) && (tree match {
          case treeInfo.Applied(core @ Select(qual, _), _, _) =>
            treeInfo.isQualifierSafeToElide(qual) && isSeqFactory(qual.symbol)
          case _ => false
        })
      }

      def isPredefClassOf(sym: Symbol) = if (PredefModule.hasCompleteInfo) sym == Predef_classOf else isPredefMemberNamed(sym, nme.classOf)

      lazy val TagMaterializers = Map[Symbol, Symbol](
        ClassTagClass    -> materializeClassTag,
        WeakTypeTagClass -> materializeWeakTypeTag,
        TypeTagClass     -> materializeTypeTag
      )
      lazy val TagSymbols = TagMaterializers.keySet

      // Methods treated specially by implicit search
      lazy val Predef_conforms = getMemberIfDefined(PredefModule, nme.conforms)
      lazy val SubTypeModule   = requiredModule[scala.<:<[_,_]]
        lazy val SubType_refl  = getMemberMethod(SubTypeModule, nme.refl)

      lazy val Predef_classOf      = getMemberMethod(PredefModule, nme.classOf)

      lazy val Predef_double2Double = getMemberMethod(PredefModule, nme.double2Double)
      lazy val Predef_float2Float = getMemberMethod(PredefModule, nme.float2Float)
      lazy val Predef_byte2Byte = getMemberMethod(PredefModule, nme.byte2Byte)
      lazy val Predef_short2Short = getMemberMethod(PredefModule, nme.short2Short)
      lazy val Predef_char2Character = getMemberMethod(PredefModule, nme.char2Character)
      lazy val Predef_int2Integer = getMemberMethod(PredefModule, nme.int2Integer)
      lazy val Predef_long2Long = getMemberMethod(PredefModule, nme.long2Long)
      lazy val Predef_boolean2Boolean = getMemberMethod(PredefModule, nme.boolean2Boolean)

      lazy val PreDef_primitives2Primitives =
        Set[Symbol](Predef_double2Double, Predef_float2Float, Predef_byte2Byte, Predef_short2Short,
          Predef_char2Character, Predef_int2Integer, Predef_long2Long, Predef_boolean2Boolean)

      lazy val Predef_implicitly   = getMemberMethod(PredefModule, nme.implicitly)
      lazy val Predef_???          = DefinitionsClass.this.Predef_???
      lazy val Predef_any2stringaddMethod = getMemberMethod(PredefModule, nme.any2stringadd).suchThat(_.isMethod)

      lazy val arrayApplyMethod       = getMemberMethod(ScalaRunTimeModule, nme.array_apply)
      lazy val arrayUpdateMethod      = getMemberMethod(ScalaRunTimeModule, nme.array_update)
      lazy val arrayLengthMethod      = getMemberMethod(ScalaRunTimeModule, nme.array_length)
      lazy val arrayCloneMethod       = getMemberMethod(ScalaRunTimeModule, nme.array_clone)
      lazy val ensureAccessibleMethod = getMemberMethod(ScalaRunTimeModule, nme.ensureAccessible)
      lazy val arrayClassMethod       = getMemberMethod(ScalaRunTimeModule, nme.arrayClass)
      lazy val wrapVarargsRefArrayMethod = getMemberMethod(ScalaRunTimeModule, nme.wrapRefArray)
      lazy val genericWrapVarargsRefArrayMethod = getMemberMethod(ScalaRunTimeModule, nme.genericWrapArray)
      lazy val primitiveWrapArrayMethod = Seq[Symbol](
        getMemberMethod(ScalaRunTimeModule, nme.wrapBooleanArray),
        getMemberMethod(ScalaRunTimeModule, nme.wrapByteArray),
        getMemberMethod(ScalaRunTimeModule, nme.wrapCharArray),
        getMemberMethod(ScalaRunTimeModule, nme.wrapIntArray),
        getMemberMethod(ScalaRunTimeModule, nme.wrapDoubleArray),
        getMemberMethod(ScalaRunTimeModule, nme.wrapFloatArray),
        getMemberMethod(ScalaRunTimeModule, nme.wrapLongArray),
        getMemberMethod(ScalaRunTimeModule, nme.wrapShortArray),
        getMemberMethod(ScalaRunTimeModule, nme.wrapUnitArray)
      )


      lazy val RuntimeStatics_ioobe = getMemberMethod(RuntimeStaticsModule, nme.ioobe)

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

      @deprecated("scala.language.higherKinds no longer needs to be imported explicitly", "2.13.1")
      lazy val HigherKindsFeature         = getLanguageFeature("higherKinds")

      lazy val ExistentialsFeature        = getLanguageFeature("existentials")

      lazy val ApiUniverseReify = ApiUniverseClass.map(sym => getDeclIfDefined(sym, nme.reify))

      lazy val ReflectRuntimeUniverse      = DefinitionsClass.this.ReflectRuntimeUniverse
      lazy val ReflectRuntimeCurrentMirror = DefinitionsClass.this.ReflectRuntimeCurrentMirror

      lazy val TreesTreeType         = TreesClass.map(sym => getTypeMember(sym, tpnme.Tree))
      object TreeType { def unapply(tpe: Type): Boolean = tpe.typeSymbol.overrideChain contains TreesTreeType }
      object SubtreeType { def unapply(tpe: Type): Boolean = tpe.typeSymbol.overrideChain exists (_.tpe <:< TreesTreeType.tpe) }

      object ExprClassOf { def unapply(tp: Type): Option[Type] = elementExtractOption(ExprClass, tp) }

      lazy val PartialManifestClass  = getTypeMember(ReflectPackage, tpnme.ClassManifest)
      lazy val ManifestSymbols = Set[Symbol](PartialManifestClass, FullManifestClass, OptManifestClass)

      def isPolymorphicSignature(sym: Symbol) = sym != null && sym.isJavaDefined && {
        val owner = sym.safeOwner
        (owner == MethodHandleClass || owner == VarHandleClass) && {
          sym.hasAnnotation(NativeAttr) && (sym.paramss match {
            case List(List(p)) => definitions.isJavaRepeatedParamType(p.info)
            case _ => false
          })
        }
      }

      lazy val Scala_Java8_CompatPackage = rootMirror.getPackageIfDefined("scala.runtime.java8")

      lazy val Future_unit: Symbol = getMemberMethod(FutureClass.companionModule, TermName("unit"))
      lazy val Future_onComplete: Symbol = getMemberMethod(FutureClass, TermName("onComplete"))
      lazy val Future_value: Symbol = getMemberMethod(FutureClass, TermName("value"))
      lazy val Promise_complete: Symbol = getMemberMethod(PromiseClass, TermName("complete"))
    }
  }
}
