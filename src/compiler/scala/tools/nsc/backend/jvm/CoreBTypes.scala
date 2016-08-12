package scala.tools.nsc
package backend.jvm

import scala.tools.asm
import scala.tools.nsc.backend.jvm.BTypes.InternalName

/**
 * Core BTypes and some other definitions. The initialization of these definitions requires access
 * to symbols / types (global).
 *
 * The symbols used to initialize the ClassBTypes may change from one compiler run to the next. To
 * make sure the definitions are consistent with the symbols in the current run, the
 * `initializeCoreBTypes` method in BTypesFromSymbols creates a new instance of CoreBTypes in each
 * compiler run.
 *
 * The class BTypesFromSymbols does not directly reference CoreBTypes, but CoreBTypesProxy. The
 * reason is that having a `var bTypes: CoreBTypes` would not allow `import bTypes._`. Instead, the
 * proxy class holds a `CoreBTypes` in a variable field and forwards to this instance.
 *
 * The definitions in `CoreBTypes` need to be lazy vals to break an initialization cycle. When
 * creating a new instance to assign to the proxy, the `classBTypeFromSymbol` invoked in the
 * constructor will actually go through the proxy. The lazy vals make sure the instance is assigned
 * in the proxy before the fields are initialized.
 *
 * Note: if we did not re-create the core BTypes on each compiler run, BType.classBTypeFromInternalNameMap
 * could not be a perRunCache anymore: the classes defined here need to be in that map, they are
 * added when the ClassBTypes are created. The per run cache removes them, so they would be missing
 * in the second run.
 */
class CoreBTypes[BTFS <: BTypesFromSymbols[_ <: Global]](val bTypes: BTFS) {
  import bTypes._
  import global._
  import rootMirror.{requiredClass, getRequiredClass, getClassIfDefined}
  import definitions._

  /**
   * Maps primitive types to their corresponding PrimitiveBType. The map is defined lexically above
   * the first use of `classBTypeFromSymbol` because that method looks at the map.
   */
  lazy val primitiveTypeToBType: Map[Symbol, PrimitiveBType] = Map(
    UnitClass    -> UNIT,
    BooleanClass -> BOOL,
    CharClass    -> CHAR,
    ByteClass    -> BYTE,
    ShortClass   -> SHORT,
    IntClass     -> INT,
    LongClass    -> LONG,
    FloatClass   -> FLOAT,
    DoubleClass  -> DOUBLE)

  /**
   * Map from primitive types to their boxed class type. Useful when pushing class literals onto the
   * operand stack (ldc instruction taking a class literal), see genConstant.
   */
  lazy val boxedClassOfPrimitive: Map[PrimitiveBType, ClassBType] = Map(
    UNIT   -> classBTypeFromSymbol(requiredClass[java.lang.Void]),
    BOOL   -> classBTypeFromSymbol(BoxedBooleanClass),
    BYTE   -> classBTypeFromSymbol(BoxedByteClass),
    SHORT  -> classBTypeFromSymbol(BoxedShortClass),
    CHAR   -> classBTypeFromSymbol(BoxedCharacterClass),
    INT    -> classBTypeFromSymbol(BoxedIntClass),
    LONG   -> classBTypeFromSymbol(BoxedLongClass),
    FLOAT  -> classBTypeFromSymbol(BoxedFloatClass),
    DOUBLE -> classBTypeFromSymbol(BoxedDoubleClass))

  lazy val boxedClasses: Set[ClassBType] = boxedClassOfPrimitive.values.toSet

  /**
   * Maps the method symbol for a box method to the boxed type of the result. For example, the
   * method symbol for `Byte.box()` is mapped to the ClassBType `java/lang/Byte`.
   */
  lazy val boxResultType: Map[Symbol, ClassBType] = {
    for ((valueClassSym, boxMethodSym) <- currentRun.runDefinitions.boxMethod)
    yield boxMethodSym -> boxedClassOfPrimitive(primitiveTypeToBType(valueClassSym))
  }

  /**
   * Maps the method symbol for an unbox method to the primitive type of the result.
   * For example, the method symbol for `Byte.unbox()`) is mapped to the PrimitiveBType BYTE. */
  lazy val unboxResultType: Map[Symbol, PrimitiveBType] = {
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
  lazy val srNothingRef : ClassBType = classBTypeFromSymbol(requiredClass[scala.runtime.Nothing$])
  lazy val srNullRef    : ClassBType = classBTypeFromSymbol(requiredClass[scala.runtime.Null$])

  lazy val ObjectRef                 : ClassBType = classBTypeFromSymbol(ObjectClass)
  lazy val StringRef                 : ClassBType = classBTypeFromSymbol(StringClass)
  lazy val PredefRef                 : ClassBType = classBTypeFromSymbol(PredefModule.moduleClass)
  lazy val jlStringBuilderRef        : ClassBType = classBTypeFromSymbol(JavaStringBuilderClass)
  lazy val jlStringBufferRef         : ClassBType = classBTypeFromSymbol(JavaStringBufferClass)
  lazy val jlCharSequenceRef         : ClassBType = classBTypeFromSymbol(JavaCharSequenceClass)
  lazy val jlThrowableRef            : ClassBType = classBTypeFromSymbol(ThrowableClass)
  lazy val jlCloneableRef            : ClassBType = classBTypeFromSymbol(JavaCloneableClass)        // java/lang/Cloneable
  lazy val jiSerializableRef         : ClassBType = classBTypeFromSymbol(JavaSerializableClass)     // java/io/Serializable
  lazy val jlClassCastExceptionRef   : ClassBType = classBTypeFromSymbol(ClassCastExceptionClass)   // java/lang/ClassCastException
  lazy val juMapRef                  : ClassBType = classBTypeFromSymbol(JavaUtilMap)               // java/util/Map
  lazy val juHashMapRef              : ClassBType = classBTypeFromSymbol(JavaUtilHashMap)           // java/util/HashMap
  lazy val sbScalaBeanInfoRef        : ClassBType = classBTypeFromSymbol(requiredClass[scala.beans.ScalaBeanInfo])
  lazy val jliSerializedLambdaRef    : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.invoke.SerializedLambda])
  lazy val jliMethodHandleRef        : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.invoke.MethodHandle])
  lazy val jliMethodHandlesRef       : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.invoke.MethodHandles])
  lazy val jliMethodHandlesLookupRef : ClassBType = classBTypeFromSymbol(exitingPickler(getRequiredClass("java.lang.invoke.MethodHandles.Lookup"))) // didn't find a reliable non-stringly-typed way that works for inner classes in the backend
  lazy val jliMethodTypeRef          : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.invoke.MethodType])
  lazy val jliCallSiteRef            : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.invoke.CallSite])
  lazy val jliLambdaMetafactoryRef   : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.invoke.LambdaMetafactory])
  lazy val srBoxesRunTimeRef         : ClassBType = classBTypeFromSymbol(requiredClass[scala.runtime.BoxesRunTime])
  lazy val srSymbolLiteral           : ClassBType = classBTypeFromSymbol(requiredClass[scala.runtime.SymbolLiteral])
  lazy val srStructuralCallSite      : ClassBType = classBTypeFromSymbol(requiredClass[scala.runtime.StructuralCallSite])
  lazy val srLambdaDeserialize       : ClassBType = classBTypeFromSymbol(requiredClass[scala.runtime.LambdaDeserialize])
  lazy val srBoxedUnitRef            : ClassBType = classBTypeFromSymbol(requiredClass[scala.runtime.BoxedUnit])

  private def methodNameAndType(cls: Symbol, name: Name, static: Boolean = false, filterOverload: Symbol => Boolean = _ => true): MethodNameAndType = {
    val holder = if (static) cls.companionModule.moduleClass else cls
    val method = holder.info.member(name).suchThat(filterOverload)
    assert(!method.isOverloaded, method)
    MethodNameAndType(name.toString, methodBTypeFromSymbol(method))
  }

  private def srBoxesRuntimeMethods(getName: (String, String) => String): Map[BType, MethodNameAndType] = {
    ScalaValueClassesNoUnit.map(primitive => {
      val bType = primitiveTypeToBType(primitive)
      val name = newTermName(getName(primitive.name.toString, boxedClass(primitive).name.toString))
      (bType, methodNameAndType(BoxesRunTimeClass, name))
    })(collection.breakOut)
  }

  // Z -> MethodNameAndType(boxToBoolean,(Z)Ljava/lang/Boolean;)
  lazy val srBoxesRuntimeBoxToMethods: Map[BType, MethodNameAndType] = srBoxesRuntimeMethods((primitive, boxed) => "boxTo" + boxed)

  // Z -> MethodNameAndType(unboxToBoolean,(Ljava/lang/Object;)Z)
  lazy val srBoxesRuntimeUnboxToMethods: Map[BType, MethodNameAndType] = srBoxesRuntimeMethods((primitive, boxed) => "unboxTo" + primitive)

  def singleParamOfClass(cls: Symbol) = (s: Symbol) => s.paramss match {
    case List(List(param)) => param.info.typeSymbol == cls
    case _ => false
  }

  // java/lang/Boolean -> MethodNameAndType(valueOf,(Z)Ljava/lang/Boolean;)
  lazy val javaBoxMethods: Map[InternalName, MethodNameAndType] = {
    ScalaValueClassesNoUnit.map(primitive => {
      val boxed = boxedClass(primitive)
      val method = methodNameAndType(boxed, newTermName("valueOf"), static = true, filterOverload = singleParamOfClass(primitive))
      (classBTypeFromSymbol(boxed).internalName, method)
    })(collection.breakOut)
  }

  // java/lang/Boolean -> MethodNameAndType(booleanValue,()Z)
  lazy val javaUnboxMethods: Map[InternalName, MethodNameAndType] = {
    ScalaValueClassesNoUnit.map(primitive => {
      val boxed = boxedClass(primitive)
      val name = primitive.name.toString.toLowerCase + "Value"
      (classBTypeFromSymbol(boxed).internalName, methodNameAndType(boxed, newTermName(name)))
    })(collection.breakOut)
  }

  private def predefBoxingMethods(getName: (String, String) => String): Map[String, MethodBType] = {
    ScalaValueClassesNoUnit.map(primitive => {
      val boxed = boxedClass(primitive)
      val name = getName(primitive.name.toString, boxed.name.toString)
      (name, methodNameAndType(PredefModule.moduleClass, newTermName(name)).methodType)
    })(collection.breakOut)
  }

  // boolean2Boolean -> (Z)Ljava/lang/Boolean;
  lazy val predefAutoBoxMethods: Map[String, MethodBType] = predefBoxingMethods((primitive, boxed) => primitive.toLowerCase + "2" + boxed)

  // Boolean2boolean -> (Ljava/lang/Boolean;)Z
  lazy val predefAutoUnboxMethods: Map[String, MethodBType] = predefBoxingMethods((primitive, boxed) => boxed + "2" + primitive.toLowerCase)

  private def staticRefMethods(name: Name): Map[InternalName, MethodNameAndType] = {
    allRefClasses.map(refClass =>
      (classBTypeFromSymbol(refClass).internalName, methodNameAndType(refClass, name, static = true)))(collection.breakOut)
  }

  // scala/runtime/BooleanRef -> MethodNameAndType(create,(Z)Lscala/runtime/BooleanRef;)
  lazy val srRefCreateMethods: Map[InternalName, MethodNameAndType] = staticRefMethods(nme.create)

  // scala/runtime/BooleanRef -> MethodNameAndType(zero,()Lscala/runtime/BooleanRef;)
  lazy val srRefZeroMethods: Map[InternalName, MethodNameAndType] = staticRefMethods(nme.zero)

  // java/lang/Boolean -> MethodNameAndType(<init>,(Z)V)
  lazy val primitiveBoxConstructors: Map[InternalName, MethodNameAndType] = {
    ScalaValueClassesNoUnit.map(primitive => {
      val boxed = boxedClass(primitive)
      (classBTypeFromSymbol(boxed).internalName, methodNameAndType(boxed, nme.CONSTRUCTOR, filterOverload = singleParamOfClass(primitive)))
    })(collection.breakOut)
  }

  private def nonOverloadedConstructors(classes: Iterable[Symbol]): Map[InternalName, MethodNameAndType] = {
    classes.map(cls => (classBTypeFromSymbol(cls).internalName, methodNameAndType(cls, nme.CONSTRUCTOR)))(collection.breakOut)
  }

  // scala/runtime/BooleanRef -> MethodNameAndType(<init>,(Z)V)
  lazy val srRefConstructors: Map[InternalName, MethodNameAndType] = nonOverloadedConstructors(allRefClasses)

  private def specializedSubclasses(cls: Symbol): List[Symbol] = {
    exitingSpecialize(cls.info) // the `transformInfo` method of specialization adds specialized subclasses to the `specializedClass` map
    specializeTypes.specializedClass.collect({
      case ((`cls`, _), specCls) => specCls
    }).toList
  }

  // scala/Tuple3 -> MethodNameAndType(<init>,(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)V)
  // scala/Tuple2$mcZC$sp -> MethodNameAndType(<init>,(ZC)V)
  lazy val tupleClassConstructors: Map[InternalName, MethodNameAndType] = {
    val tupleClassSymbols = TupleClass.seq ++ specializedSubclasses(TupleClass(1)) ++ specializedSubclasses(TupleClass(2))
    nonOverloadedConstructors(tupleClassSymbols)
  }

  lazy val typeOfArrayOp: Map[Int, BType] = {
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

  lazy val hashMethodSym: Symbol = getMember(RuntimeStaticsModule, nme.anyHash)

  // TODO @lry avoiding going through through missingHook for every line in the REPL: https://github.com/scala/scala/commit/8d962ed4ddd310cc784121c426a2e3f56a112540
  lazy val AndroidParcelableInterface : Symbol = getClassIfDefined("android.os.Parcelable")
  lazy val AndroidCreatorClass        : Symbol = getClassIfDefined("android.os.Parcelable$Creator")

  lazy val BeanInfoAttr: Symbol = requiredClass[scala.beans.BeanInfo]

  /* The Object => String overload. */
  lazy val String_valueOf: Symbol = {
    getMember(StringModule, nme.valueOf) filter (sym => sym.info.paramTypes match {
      case List(pt) => pt.typeSymbol == ObjectClass
      case _        => false
    })
  }

  lazy val lambdaMetaFactoryMetafactoryHandle =
    new asm.Handle(asm.Opcodes.H_INVOKESTATIC,
      coreBTypes.jliLambdaMetafactoryRef.internalName, sn.Metafactory.toString,
      MethodBType(
        List(
          coreBTypes.jliMethodHandlesLookupRef,
          coreBTypes.StringRef,
          coreBTypes.jliMethodTypeRef,
          coreBTypes.jliMethodTypeRef,
          coreBTypes.jliMethodHandleRef,
          coreBTypes.jliMethodTypeRef),
        coreBTypes.jliCallSiteRef
      ).descriptor,
      /* itf = */ coreBTypes.jliLambdaMetafactoryRef.isInterface.get)

  lazy val lambdaMetaFactoryAltMetafactoryHandle =
    new asm.Handle(asm.Opcodes.H_INVOKESTATIC,
      coreBTypes.jliLambdaMetafactoryRef.internalName, sn.AltMetafactory.toString,
      MethodBType(
        List(
          coreBTypes.jliMethodHandlesLookupRef,
          coreBTypes.StringRef,
          coreBTypes.jliMethodTypeRef,
          ArrayBType(ObjectRef)),
        coreBTypes.jliCallSiteRef
      ).descriptor,
      /* itf = */ coreBTypes.jliLambdaMetafactoryRef.isInterface.get)

  lazy val lambdaDeserializeBootstrapHandle =
    new scala.tools.asm.Handle(scala.tools.asm.Opcodes.H_INVOKESTATIC,
      coreBTypes.srLambdaDeserialize.internalName, sn.Bootstrap.toString,
      MethodBType(
        List(
          coreBTypes.jliMethodHandlesLookupRef,
          coreBTypes.StringRef,
          coreBTypes.jliMethodTypeRef,
          ArrayBType(jliMethodHandleRef)
        ),
        coreBTypes.jliCallSiteRef
      ).descriptor,
      /* itf = */ coreBTypes.srLambdaDeserialize.isInterface.get)
}

/**
 * This trait make some core BTypes available that don't depend on a Global instance. Some core
 * BTypes are required to be accessible in the BTypes trait, which does not have access to Global.
 *
 * BTypes cannot refer to CoreBTypesProxy because some of its members depend on global, for example
 * the type Symbol in
 *   def primitiveTypeMap: Map[Symbol, PrimitiveBType]
 */
trait CoreBTypesProxyGlobalIndependent[BTS <: BTypes] {
  val bTypes: BTS
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

  def lambdaMetaFactoryMetafactoryHandle    : asm.Handle
  def lambdaMetaFactoryAltMetafactoryHandle : asm.Handle
  def lambdaDeserializeBootstrapHandle      : asm.Handle
}

/**
 * See comment in class [[CoreBTypes]].
 */
final class CoreBTypesProxy[BTFS <: BTypesFromSymbols[_ <: Global]](val bTypes: BTFS) extends CoreBTypesProxyGlobalIndependent[BTFS] {
  import bTypes._
  import global._

  private[this] var _coreBTypes: CoreBTypes[bTypes.type] = _
  def setBTypes(coreBTypes: CoreBTypes[BTFS]): Unit = {
    _coreBTypes = coreBTypes.asInstanceOf[CoreBTypes[bTypes.type]]
  }

  def primitiveTypeToBType: Map[Symbol, PrimitiveBType] = _coreBTypes.primitiveTypeToBType

  def boxedClasses: Set[ClassBType] = _coreBTypes.boxedClasses
  def boxedClassOfPrimitive: Map[PrimitiveBType, ClassBType] = _coreBTypes.boxedClassOfPrimitive

  def boxResultType: Map[Symbol, ClassBType] = _coreBTypes.boxResultType
  def unboxResultType: Map[Symbol, PrimitiveBType] = _coreBTypes.unboxResultType

  def srNothingRef              : ClassBType = _coreBTypes.srNothingRef
  def srNullRef                 : ClassBType = _coreBTypes.srNullRef

  def ObjectRef                 : ClassBType = _coreBTypes.ObjectRef
  def StringRef                 : ClassBType = _coreBTypes.StringRef
  def PredefRef                 : ClassBType = _coreBTypes.PredefRef
  def jlStringBuilderRef        : ClassBType = _coreBTypes.jlStringBuilderRef
  def jlStringBufferRef         : ClassBType = _coreBTypes.jlStringBufferRef
  def jlCharSequenceRef         : ClassBType = _coreBTypes.jlCharSequenceRef
  def jlThrowableRef            : ClassBType = _coreBTypes.jlThrowableRef
  def jlCloneableRef            : ClassBType = _coreBTypes.jlCloneableRef
  def jiSerializableRef         : ClassBType = _coreBTypes.jiSerializableRef
  def jlClassCastExceptionRef   : ClassBType = _coreBTypes.jlClassCastExceptionRef
  def juMapRef                  : ClassBType = _coreBTypes.juMapRef
  def juHashMapRef              : ClassBType = _coreBTypes.juHashMapRef
  def sbScalaBeanInfoRef        : ClassBType = _coreBTypes.sbScalaBeanInfoRef
  def jliSerializedLambdaRef    : ClassBType = _coreBTypes.jliSerializedLambdaRef
  def jliMethodHandleRef        : ClassBType = _coreBTypes.jliMethodHandleRef
  def jliMethodHandlesRef       : ClassBType = _coreBTypes.jliMethodHandlesRef
  def jliMethodHandlesLookupRef : ClassBType = _coreBTypes.jliMethodHandlesLookupRef
  def jliMethodTypeRef          : ClassBType = _coreBTypes.jliMethodTypeRef
  def jliCallSiteRef            : ClassBType = _coreBTypes.jliCallSiteRef
  def jliLambdaMetafactoryRef   : ClassBType = _coreBTypes.jliLambdaMetafactoryRef
  def srBoxesRunTimeRef         : ClassBType = _coreBTypes.srBoxesRunTimeRef
  def srBoxedUnitRef            : ClassBType = _coreBTypes.srBoxedUnitRef

  def srBoxesRuntimeBoxToMethods   : Map[BType, MethodNameAndType] = _coreBTypes.srBoxesRuntimeBoxToMethods
  def srBoxesRuntimeUnboxToMethods : Map[BType, MethodNameAndType] = _coreBTypes.srBoxesRuntimeUnboxToMethods

  def javaBoxMethods   : Map[InternalName, MethodNameAndType] = _coreBTypes.javaBoxMethods
  def javaUnboxMethods : Map[InternalName, MethodNameAndType] = _coreBTypes.javaUnboxMethods

  def predefAutoBoxMethods   : Map[String, MethodBType] = _coreBTypes.predefAutoBoxMethods
  def predefAutoUnboxMethods : Map[String, MethodBType] = _coreBTypes.predefAutoUnboxMethods

  def srRefCreateMethods : Map[InternalName, MethodNameAndType] = _coreBTypes.srRefCreateMethods
  def srRefZeroMethods   : Map[InternalName, MethodNameAndType] = _coreBTypes.srRefZeroMethods

  def primitiveBoxConstructors : Map[InternalName, MethodNameAndType] = _coreBTypes.primitiveBoxConstructors
  def srRefConstructors        : Map[InternalName, MethodNameAndType] = _coreBTypes.srRefConstructors
  def tupleClassConstructors   : Map[InternalName, MethodNameAndType] = _coreBTypes.tupleClassConstructors

  def srSymbolLiteral           : ClassBType = _coreBTypes.srSymbolLiteral
  def srStructuralCallSite      : ClassBType = _coreBTypes.srStructuralCallSite
  def srLambdaDeserialize       : ClassBType = _coreBTypes.srLambdaDeserialize

  def typeOfArrayOp: Map[Int, BType] = _coreBTypes.typeOfArrayOp

  // Some symbols. These references should probably be moved to Definitions.

  def hashMethodSym: Symbol = _coreBTypes.hashMethodSym

  def AndroidParcelableInterface : Symbol = _coreBTypes.AndroidParcelableInterface
  def AndroidCreatorClass        : Symbol = _coreBTypes.AndroidCreatorClass

  def BeanInfoAttr: Symbol = _coreBTypes.BeanInfoAttr

  def String_valueOf: Symbol = _coreBTypes.String_valueOf

  def lambdaMetaFactoryMetafactoryHandle    : asm.Handle = _coreBTypes.lambdaMetaFactoryMetafactoryHandle
  def lambdaMetaFactoryAltMetafactoryHandle : asm.Handle = _coreBTypes.lambdaMetaFactoryAltMetafactoryHandle
  def lambdaDeserializeBootstrapHandle      : asm.Handle = _coreBTypes.lambdaDeserializeBootstrapHandle
}
