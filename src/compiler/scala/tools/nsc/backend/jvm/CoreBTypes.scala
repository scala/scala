/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package backend.jvm

import scala.tools.asm.{Handle, Opcodes}
import scala.tools.nsc.backend.jvm.BTypes.InternalName

abstract class CoreBTypes extends PerRunInit {
  val bTypes: BTypes
  import bTypes._

  def boxedClasses: Set[ClassBType]
  def boxedClassOfPrimitive: Map[PrimitiveBType, ClassBType]

  def srNothingRef              : ClassBType
  def srNullRef                 : ClassBType
  def ObjectRef                 : ClassBType
  def StringRef                 : ClassBType
  def PredefRef                 : ClassBType
  def jlCloneableRef            : ClassBType
  def jiSerializableRef         : ClassBType
  def jlIllegalArgExceptionRef  : ClassBType
  def juHashMapRef              : ClassBType
  def juMapRef                  : ClassBType
  def jliCallSiteRef            : ClassBType
  def jliLambdaMetafactoryRef   : ClassBType
  def jliMethodTypeRef          : ClassBType
  def jliSerializedLambdaRef    : ClassBType
  def jliMethodHandleRef        : ClassBType
  def jliMethodHandlesLookupRef : ClassBType
  def srBoxesRunTimeRef         : ClassBType
  def srBoxedUnitRef            : ClassBType

  def srBoxesRuntimeBoxToMethods   : Map[BType, MethodNameAndType]
  def srBoxesRuntimeUnboxToMethods : Map[BType, MethodNameAndType]

  def javaBoxMethods   : Map[InternalName, MethodNameAndType]
  def javaUnboxMethods : Map[InternalName, MethodNameAndType]

  def predefAutoBoxMethods   : Map[String, MethodBType]
  def predefAutoUnboxMethods : Map[String, MethodBType]

  def srRefCreateMethods : Map[InternalName, MethodNameAndType]
  def srRefZeroMethods   : Map[InternalName, MethodNameAndType]

  def primitiveBoxConstructors : Map[InternalName, MethodNameAndType]
  def srRefConstructors        : Map[InternalName, MethodNameAndType]
  def tupleClassConstructors   : Map[InternalName, MethodNameAndType]

  def lambdaMetaFactoryMetafactoryHandle    : Handle
  def lambdaMetaFactoryAltMetafactoryHandle : Handle
  def lambdaDeserializeBootstrapHandle      : Handle
}

abstract class CoreBTypesFromSymbols[G <: Global] extends CoreBTypes {
  val bTypes: BTypesFromSymbols[G]

  import bTypes._
  import global._
  import definitions._
  import rootMirror.{getClassIfDefined, getRequiredClass, requiredClass}

  /**
   * This method is used to lazily initialize the core BTypes. The computation is synchronized on
   * the frontendLock, as it reads Symbols. The BTypes are re-initialized in each compiler run as
   * the information in symbols may change.
   */
  private def runLazy[T](init: => T): LazyVar[T] = perRunLazy(this)(init)

  /**
   * Maps primitive types to their corresponding PrimitiveBType. The map is defined lexically above
   * the first use of `classBTypeFromSymbol` because that method looks at the map.
   */
  def primitiveTypeToBType: Map[Symbol, PrimitiveBType] = _primitiveTypeToBType.get
  private[this] lazy val _primitiveTypeToBType: LazyVar[Map[Symbol, PrimitiveBType]] = runLazy {
    Map(
      UnitClass    -> UNIT,
      BooleanClass -> BOOL,
      CharClass    -> CHAR,
      ByteClass    -> BYTE,
      ShortClass   -> SHORT,
      IntClass     -> INT,
      LongClass    -> LONG,
      FloatClass   -> FLOAT,
      DoubleClass  -> DOUBLE)
  }

  /**
   * Map from primitive types to their boxed class type. Useful when pushing class literals onto the
   * operand stack (ldc instruction taking a class literal), see genConstant.
   */
  def boxedClassOfPrimitive: Map[PrimitiveBType, ClassBType] = _boxedClassOfPrimitive.get
  private[this] lazy val _boxedClassOfPrimitive: LazyVar[Map[PrimitiveBType, ClassBType]] = runLazy {
    Map(
      UNIT   -> classBTypeFromSymbol(requiredClass[java.lang.Void]),
      BOOL   -> classBTypeFromSymbol(BoxedBooleanClass),
      BYTE   -> classBTypeFromSymbol(BoxedByteClass),
      SHORT  -> classBTypeFromSymbol(BoxedShortClass),
      CHAR   -> classBTypeFromSymbol(BoxedCharacterClass),
      INT    -> classBTypeFromSymbol(BoxedIntClass),
      LONG   -> classBTypeFromSymbol(BoxedLongClass),
      FLOAT  -> classBTypeFromSymbol(BoxedFloatClass),
      DOUBLE -> classBTypeFromSymbol(BoxedDoubleClass))
  }

  def boxedClasses: Set[ClassBType] = _boxedClasses.get
  private[this] lazy val _boxedClasses: LazyVar[Set[ClassBType]] = runLazy(boxedClassOfPrimitive.values.toSet)

  /**
   * Maps the method symbol for a box method to the boxed type of the result. For example, the
   * method symbol for `Byte.box()` is mapped to the ClassBType `java/lang/Byte`.
   */
  def boxResultType: Map[Symbol, ClassBType] = _boxResultType.get
  private[this] lazy val _boxResultType: LazyVar[Map[Symbol, ClassBType]] = runLazy {
    for ((valueClassSym, boxMethodSym) <- currentRun.runDefinitions.boxMethod)
    yield boxMethodSym -> boxedClassOfPrimitive(primitiveTypeToBType(valueClassSym))
  }

  /**
   * Maps the method symbol for an unbox method to the primitive type of the result.
   * For example, the method symbol for `Byte.unbox()`) is mapped to the PrimitiveBType BYTE. */
  def unboxResultType: Map[Symbol, PrimitiveBType] = _unboxResultType.get
  private[this] lazy val _unboxResultType: LazyVar[Map[Symbol, PrimitiveBType]] = runLazy {
    for ((valueClassSym, unboxMethodSym) <- currentRun.runDefinitions.unboxMethod)
    yield unboxMethodSym -> primitiveTypeToBType(valueClassSym)
  }

  /*
   * RT_NOTHING and RT_NULL exist at run-time only. They are the bytecode-level manifestation (in
   * method signatures only) of what shows up as NothingClass (scala.Nothing) resp. NullClass
   * (scala.Null) in Scala ASTs.
   *
   * Therefore, when RT_NOTHING or RT_NULL are to be emitted, a mapping is needed: the internal
   * names of NothingClass and NullClass can't be emitted as-is.
   */
  def                     srNothingRef              : ClassBType = _srNothingRef.get
  private[this] lazy val _srNothingRef              : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(requiredClass[scala.runtime.Nothing$]))

  def                     srNullRef                 : ClassBType = _srNullRef.get
  private[this] lazy val _srNullRef                 : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(requiredClass[scala.runtime.Null$]))

  def                     ObjectRef                 : ClassBType = _ObjectRef.get
  private[this] lazy val _ObjectRef                 : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(ObjectClass))

  def                     StringRef                 : ClassBType = _StringRef.get
  private[this] lazy val _StringRef                 : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(StringClass))

  def                     PredefRef                 : ClassBType = _PredefRef.get
  private[this] lazy val _PredefRef                 : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(PredefModule.moduleClass))

  def                     jlStringBuilderRef        : ClassBType = _jlStringBuilderRef.get
  private[this] lazy val _jlStringBuilderRef        : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(JavaStringBuilderClass))

  def                     jlStringBufferRef         : ClassBType = _jlStringBufferRef.get
  private[this] lazy val _jlStringBufferRef         : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(JavaStringBufferClass))

  def                     jlCharSequenceRef         : ClassBType = _jlCharSequenceRef.get
  private[this] lazy val _jlCharSequenceRef         : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(JavaCharSequenceClass))

  def                     jlThrowableRef            : ClassBType = _jlThrowableRef.get
  private[this] lazy val _jlThrowableRef            : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(ThrowableClass))

  def                     jlCloneableRef            : ClassBType = _jlCloneableRef.get
  private[this] lazy val _jlCloneableRef            : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(JavaCloneableClass))        // java/lang/Cloneable

  def                     jiSerializableRef         : ClassBType = _jiSerializableRef.get
  private[this] lazy val _jiSerializableRef         : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(SerializableClass))     // java/io/Serializable

  def                     jlClassCastExceptionRef   : ClassBType = _jlClassCastExceptionRef.get
  private[this] lazy val _jlClassCastExceptionRef   : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(ClassCastExceptionClass))   // java/lang/ClassCastException

  def                     jlIllegalArgExceptionRef  : ClassBType = _jlIllegalArgExceptionRef.get
  private[this] lazy val _jlIllegalArgExceptionRef  : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(IllegalArgExceptionClass))  // java/lang/IllegalArgumentException

  def                     juMapRef                  : ClassBType = _juMapRef.get
  private[this] lazy val _juMapRef                  : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(JavaUtilMap))               // java/util/Map

  def                     juHashMapRef              : ClassBType = _juHashMapRef.get
  private[this] lazy val _juHashMapRef              : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(JavaUtilHashMap))           // java/util/HashMap

  def                     jliSerializedLambdaRef    : ClassBType = _jliSerializedLambdaRef.get
  private[this] lazy val _jliSerializedLambdaRef    : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(requiredClass[java.lang.invoke.SerializedLambda]))

  def                     jliMethodHandleRef        : ClassBType = _jliMethodHandleRef.get
  private[this] lazy val _jliMethodHandleRef        : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(requiredClass[java.lang.invoke.MethodHandle]))

  def                     jliMethodHandlesRef       : ClassBType = _jliMethodHandlesRef.get
  private[this] lazy val _jliMethodHandlesRef       : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(requiredClass[java.lang.invoke.MethodHandles]))

  def                     jliMethodHandlesLookupRef : ClassBType = _jliMethodHandlesLookupRef.get
  private[this] lazy val _jliMethodHandlesLookupRef : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(exitingPickler(getRequiredClass("java.lang.invoke.MethodHandles.Lookup")))) // didn't find a reliable non-stringly-typed way that works for inner classes in the backend

  def                     jliMethodTypeRef          : ClassBType = _jliMethodTypeRef.get
  private[this] lazy val _jliMethodTypeRef          : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(requiredClass[java.lang.invoke.MethodType]))

  def                     jliCallSiteRef            : ClassBType = _jliCallSiteRef.get
  private[this] lazy val _jliCallSiteRef            : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(requiredClass[java.lang.invoke.CallSite]))

  def                     jliLambdaMetafactoryRef   : ClassBType = _jliLambdaMetafactoryRef.get
  private[this] lazy val _jliLambdaMetafactoryRef   : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(requiredClass[java.lang.invoke.LambdaMetafactory]))

  def                     jliStringConcatFactoryRef : ClassBType = _jliStringConcatFactoryRef.get
  private[this] lazy val _jliStringConcatFactoryRef : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(getRequiredClass("java.lang.invoke.StringConcatFactory")))

  def                     srBoxesRunTimeRef         : ClassBType = _srBoxesRunTimeRef.get
  private[this] lazy val _srBoxesRunTimeRef         : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(requiredClass[scala.runtime.BoxesRunTime]))

  def                     srSymbolLiteral           : ClassBType = _srSymbolLiteral.get
  private[this] lazy val _srSymbolLiteral           : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(requiredClass[scala.runtime.SymbolLiteral]))

  def                     srStructuralCallSite      : ClassBType = _srStructuralCallSite.get
  private[this] lazy val _srStructuralCallSite      : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(requiredClass[scala.runtime.StructuralCallSite]))

  def                     srLambdaDeserialize       : ClassBType = _srLambdaDeserialize.get
  private[this] lazy val _srLambdaDeserialize       : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(requiredClass[scala.runtime.LambdaDeserialize]))

  def                     srBoxedUnitRef            : ClassBType = _srBoxedUnitRef.get
  private[this] lazy val _srBoxedUnitRef            : LazyVar[ClassBType] = runLazy(classBTypeFromSymbol(requiredClass[scala.runtime.BoxedUnit]))

  private def methodNameAndType(cls: Symbol, name: Name, static: Boolean = false, filterOverload: Symbol => Boolean = _ => true): MethodNameAndType = {
    val holder = if (static) cls.companionModule.moduleClass else cls
    val method = holder.info.member(name).suchThat(filterOverload)
    assert(!method.isOverloaded, method)
    MethodNameAndType(name.toString, methodBTypeFromSymbol(method))
  }

  private def srBoxesRuntimeMethods(getName: (String, String) => String): Map[BType, MethodNameAndType] = {
    Map.from(ScalaValueClassesNoUnit.iterator.map(primitive => {
      val bType = primitiveTypeToBType(primitive)
      val name = newTermName(getName(primitive.name.toString, boxedClass(primitive).name.toString))
      (bType, methodNameAndType(BoxesRunTimeClass, name))
    }))
  }

  // Z -> MethodNameAndType(boxToBoolean,(Z)Ljava/lang/Boolean;)
  def srBoxesRuntimeBoxToMethods: Map[BType, MethodNameAndType] = _srBoxesRuntimeBoxToMethods.get
  private[this] lazy val _srBoxesRuntimeBoxToMethods: LazyVar[Map[BType, MethodNameAndType]] = runLazy(srBoxesRuntimeMethods((_, boxed) => "boxTo" + boxed))

  // Z -> MethodNameAndType(unboxToBoolean,(Ljava/lang/Object;)Z)
  def srBoxesRuntimeUnboxToMethods: Map[BType, MethodNameAndType] = _srBoxesRuntimeUnboxToMethods.get
  private[this] lazy val _srBoxesRuntimeUnboxToMethods: LazyVar[Map[BType, MethodNameAndType]] = runLazy(srBoxesRuntimeMethods((primitive, _) => "unboxTo" + primitive))

  private def singleParamOfClass(cls: Symbol) = (s: Symbol) => s.paramss match {
    case List(List(param)) => param.info.typeSymbol == cls
    case _ => false
  }

  // java/lang/Boolean -> MethodNameAndType(valueOf,(Z)Ljava/lang/Boolean;)
  def javaBoxMethods: Map[InternalName, MethodNameAndType] = _javaBoxMethods.get
  private[this] lazy val _javaBoxMethods: LazyVar[Map[InternalName, MethodNameAndType]] = runLazy {
    Map.from(ScalaValueClassesNoUnit.iterator.map(primitive => {
      val boxed = boxedClass(primitive)
      val method = methodNameAndType(boxed, newTermName("valueOf"), static = true, filterOverload = singleParamOfClass(primitive))
      (classBTypeFromSymbol(boxed).internalName, method)
    }))
  }

  // java/lang/Boolean -> MethodNameAndType(booleanValue,()Z)
  def javaUnboxMethods: Map[InternalName, MethodNameAndType] = _javaUnboxMethods.get
  private[this] lazy val _javaUnboxMethods: LazyVar[Map[InternalName, MethodNameAndType]] = runLazy {
    Map.from(ScalaValueClassesNoUnit.iterator.map(primitive => {
      val boxed = boxedClass(primitive)
      val name = primitive.name.toString.toLowerCase + "Value"
      (classBTypeFromSymbol(boxed).internalName, methodNameAndType(boxed, newTermName(name)))
    }))
  }

  private def predefBoxingMethods(getName: (String, String) => String): Map[String, MethodBType] = {
    Map.from(ScalaValueClassesNoUnit.iterator.map(primitive => {
      val boxed = boxedClass(primitive)
      val name = getName(primitive.name.toString, boxed.name.toString)
      (name, methodNameAndType(PredefModule.moduleClass, newTermName(name)).methodType)
    }))
  }

  // boolean2Boolean -> (Z)Ljava/lang/Boolean;
  def predefAutoBoxMethods: Map[String, MethodBType] = _predefAutoBoxMethods.get
  private[this] lazy val _predefAutoBoxMethods: LazyVar[Map[String, MethodBType]] = runLazy(predefBoxingMethods((primitive, boxed) => primitive.toLowerCase + "2" + boxed))

  // Boolean2boolean -> (Ljava/lang/Boolean;)Z
  def predefAutoUnboxMethods: Map[String, MethodBType] = _predefAutoUnboxMethods.get
  private[this] lazy val _predefAutoUnboxMethods: LazyVar[Map[String, MethodBType]] = runLazy(predefBoxingMethods((primitive, boxed) => boxed + "2" + primitive.toLowerCase))

  private def staticRefMethods(name: Name): Map[InternalName, MethodNameAndType] = {
    Map.from(allRefClasses.iterator.map(refClass =>
      (classBTypeFromSymbol(refClass).internalName, methodNameAndType(refClass, name, static = true))))
  }

  // scala/runtime/BooleanRef -> MethodNameAndType(create,(Z)Lscala/runtime/BooleanRef;)
  def srRefCreateMethods: Map[InternalName, MethodNameAndType] = _srRefCreateMethods.get
  private[this] lazy val _srRefCreateMethods: LazyVar[Map[InternalName, MethodNameAndType]] = runLazy(staticRefMethods(nme.create))

  // scala/runtime/BooleanRef -> MethodNameAndType(zero,()Lscala/runtime/BooleanRef;)
  def srRefZeroMethods: Map[InternalName, MethodNameAndType] = _srRefZeroMethods.get
  private[this] lazy val _srRefZeroMethods: LazyVar[Map[InternalName, MethodNameAndType]] = runLazy(staticRefMethods(nme.zero))

  // java/lang/Boolean -> MethodNameAndType(<init>,(Z)V)
  def primitiveBoxConstructors: Map[InternalName, MethodNameAndType] = _primitiveBoxConstructors.get
  private[this] lazy val _primitiveBoxConstructors: LazyVar[Map[InternalName, MethodNameAndType]] = runLazy {
    Map.from(ScalaValueClassesNoUnit.iterator.map(primitive => {
      val boxed = boxedClass(primitive)
      (classBTypeFromSymbol(boxed).internalName, methodNameAndType(boxed, nme.CONSTRUCTOR, filterOverload = singleParamOfClass(primitive)))
    }))
  }

  private def nonOverloadedConstructors(classes: Iterable[Symbol]): Map[InternalName, MethodNameAndType] = {
    Map.from(classes.iterator.map(cls => (classBTypeFromSymbol(cls).internalName, methodNameAndType(cls, nme.CONSTRUCTOR))))
  }

  // scala/runtime/BooleanRef -> MethodNameAndType(<init>,(Z)V)
  def srRefConstructors: Map[InternalName, MethodNameAndType] = _srRefConstructors.get
  private[this] lazy val _srRefConstructors: LazyVar[Map[InternalName, MethodNameAndType]] = runLazy(nonOverloadedConstructors(allRefClasses))

  private def specializedSubclasses(cls: Symbol): List[Symbol] = {
    exitingSpecialize(cls.info) // the `transformInfo` method of specialization adds specialized subclasses to the `specializedClass` map
    val map = specializeTypes.specializedClass.getOrNull(cls)
    if (map == null) Nil
    else map.values.toList
  }

  // scala/Tuple3 -> MethodNameAndType(<init>,(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)V)
  // scala/Tuple2$mcZC$sp -> MethodNameAndType(<init>,(ZC)V)
  def tupleClassConstructors: Map[InternalName, MethodNameAndType] = _tupleClassConstructors.get
  private[this] lazy val _tupleClassConstructors: LazyVar[Map[InternalName, MethodNameAndType]] = runLazy {
    val tupleClassSymbols = TupleClass.seq ++ specializedSubclasses(TupleClass(1)) ++ specializedSubclasses(TupleClass(2))
    nonOverloadedConstructors(tupleClassSymbols)
  }

  def typeOfArrayOp: Map[Int, BType] = _typeOfArrayOp.get
  private[this] lazy val _typeOfArrayOp: LazyVar[Map[Int, BType]] = runLazy {
    import scalaPrimitives._
    Map(
      (List(ZARRAY_LENGTH, ZARRAY_GET, ZARRAY_SET) map (_ -> BOOL))   ++
        (List(BARRAY_LENGTH, BARRAY_GET, BARRAY_SET) map (_ -> BYTE))   ++
        (List(SARRAY_LENGTH, SARRAY_GET, SARRAY_SET) map (_ -> SHORT))  ++
        (List(CARRAY_LENGTH, CARRAY_GET, CARRAY_SET) map (_ -> CHAR))   ++
        (List(IARRAY_LENGTH, IARRAY_GET, IARRAY_SET) map (_ -> INT))    ++
        (List(LARRAY_LENGTH, LARRAY_GET, LARRAY_SET) map (_ -> LONG))   ++
        (List(FARRAY_LENGTH, FARRAY_GET, FARRAY_SET) map (_ -> FLOAT))  ++
        (List(DARRAY_LENGTH, DARRAY_GET, DARRAY_SET) map (_ -> DOUBLE)) ++
        (List(OARRAY_LENGTH, OARRAY_GET, OARRAY_SET) map (_ -> ObjectRef)) : _*
    )
  }

  def hashMethodSym: Symbol = _hashMethodSym.get
  private[this] lazy val _hashMethodSym: LazyVar[Symbol] = runLazy(getMember(RuntimeStaticsModule, nme.anyHash))

  // TODO @lry avoiding going through through missingHook for every line in the REPL: https://github.com/scala/scala/commit/8d962ed4ddd310cc784121c426a2e3f56a112540
  def AndroidParcelableInterface: Symbol = _AndroidParcelableInterface.get
  private[this] lazy val _AndroidParcelableInterface: LazyVar[Symbol] = runLazy(getClassIfDefined("android.os.Parcelable"))

  def AndroidCreatorClass: Symbol = _AndroidCreatorClass.get
  private[this] lazy val _AndroidCreatorClass: LazyVar[Symbol] = runLazy(getClassIfDefined("android.os.Parcelable$Creator"))

  /* The Object => String overload. */
  def String_valueOf: Symbol = _String_valueOf.get
  private[this] lazy val _String_valueOf: LazyVar[Symbol] = runLazy {
    getMember(StringModule, nme.valueOf) filter (sym => sym.info.paramTypes match {
      case List(pt) => pt.typeSymbol == ObjectClass
      case _        => false
    })
  }

  def lambdaMetaFactoryMetafactoryHandle: Handle = _lambdaMetaFactoryMetafactoryHandle.get
  private[this] lazy val _lambdaMetaFactoryMetafactoryHandle: LazyVar[Handle] = runLazy {
    new Handle(Opcodes.H_INVOKESTATIC,
      coreBTypes.jliLambdaMetafactoryRef.internalName, sn.Metafactory.toString,
      MethodBType(
        Array(
          coreBTypes.jliMethodHandlesLookupRef,
          coreBTypes.StringRef,
          coreBTypes.jliMethodTypeRef,
          coreBTypes.jliMethodTypeRef,
          coreBTypes.jliMethodHandleRef,
          coreBTypes.jliMethodTypeRef),
        coreBTypes.jliCallSiteRef
      ).descriptor,
      /* itf = */ coreBTypes.jliLambdaMetafactoryRef.isInterface.get)
  }

  def lambdaMetaFactoryAltMetafactoryHandle: Handle = _lambdaMetaFactoryAltMetafactoryHandle.get
  private[this] lazy val _lambdaMetaFactoryAltMetafactoryHandle: LazyVar[Handle] = runLazy {
    new Handle(Opcodes.H_INVOKESTATIC,
      coreBTypes.jliLambdaMetafactoryRef.internalName, sn.AltMetafactory.toString,
      MethodBType(
        Array(
          coreBTypes.jliMethodHandlesLookupRef,
          coreBTypes.StringRef,
          coreBTypes.jliMethodTypeRef,
          ArrayBType(ObjectRef)),
        coreBTypes.jliCallSiteRef
      ).descriptor,
      /* itf = */ coreBTypes.jliLambdaMetafactoryRef.isInterface.get)
  }

  def lambdaDeserializeBootstrapHandle: Handle = _lambdaDeserializeBootstrapHandle.get
  private[this] lazy val _lambdaDeserializeBootstrapHandle: LazyVar[Handle] = runLazy {
    new Handle(Opcodes.H_INVOKESTATIC,
      coreBTypes.srLambdaDeserialize.internalName, sn.Bootstrap.toString,
      MethodBType(
        Array(
          coreBTypes.jliMethodHandlesLookupRef,
          coreBTypes.StringRef,
          coreBTypes.jliMethodTypeRef,
          ArrayBType(jliMethodHandleRef)
        ),
        coreBTypes.jliCallSiteRef
      ).descriptor,
      /* itf = */ coreBTypes.srLambdaDeserialize.isInterface.get)
  }
}
