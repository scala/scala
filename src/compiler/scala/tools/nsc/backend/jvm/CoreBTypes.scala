package scala.tools.nsc
package backend.jvm

import scala.annotation.switch

/**
 * Core BTypes and some other definitions. The initialization of these definitions requies access
 * to symbols / types (global).
 *
 * There is a reason for using vars instead of lazy vals. The symbols used to initialize the
 * ClassBTypes may change from one compiler run to the next. To make sure the definitions are
 * consistent with the symbols in the current run, the `intializeCoreBTypes` (which assigns the
 * vars) is executed once for every compiler run.
 *
 * Note: if they were lazy vals, BType.classBTypeFromInternalNameMap could not be a perRunCache
 * anymore: the classes defeined here need to be in that map, they are added when the ClassBTypes
 * are created. The per run cache removes them, so they would be missing in the second run.
 */
trait CoreBTypes[G <: Global] { self: BTypesFromSymbols[G] =>
  import global._
  import rootMirror.{requiredClass, getClassIfDefined}
  import definitions._

  /**
   * Maps primitive types to their corresponding PrimitiveBType. The map is defined lexically above
   * the first use of `classBTypeFromSymbol` because that method looks at the map.
   */
  var primitiveTypeMap: Map[Symbol, PrimitiveBType] = _

  var BOXED_UNIT    : ClassBType = _
  var BOXED_BOOLEAN : ClassBType = _
  var BOXED_BYTE    : ClassBType = _
  var BOXED_SHORT   : ClassBType = _
  var BOXED_CHAR    : ClassBType = _
  var BOXED_INT     : ClassBType = _
  var BOXED_LONG    : ClassBType = _
  var BOXED_FLOAT   : ClassBType = _
  var BOXED_DOUBLE  : ClassBType = _

  var boxedClasses: Set[ClassBType] = _

  /**
   * Map from primitive types to their boxed class type. Useful when pushing class literals onto the
   * operand stack (ldc instruction taking a class literal), see genConstant.
   */
  var boxedClassOfPrimitive: Map[PrimitiveBType, ClassBType] = _

  /**
   * Maps the method symbol for a box method to the boxed type of the result. For example, the
   * method symbol for `Byte.box()` is mapped to the ClassBType `java/lang/Byte`.
   */
  var boxResultType: Map[Symbol, ClassBType] = _

  /**
   * Maps the method symbol for an unbox method to the primitive type of the result.
   * For example, the method symbol for `Byte.unbox()`) is mapped to the PrimitiveBType BYTE. */
  var unboxResultType: Map[Symbol, PrimitiveBType] = _

  /*
   * RT_NOTHING and RT_NULL exist at run-time only. They are the bytecode-level manifestation (in
   * method signatures only) of what shows up as NothingClass resp. NullClass in Scala ASTs.
   *
   * Therefore, when RT_NOTHING or RT_NULL are to be emitted, a mapping is needed: the internal
   * names of NothingClass and NullClass can't be emitted as-is.
   */
  var RT_NOTHING : ClassBType = _
  var RT_NULL    : ClassBType = _

  var ObjectReference   : ClassBType = _
  var objArrayReference : ArrayBType = _

  var StringReference             : ClassBType = _
  var StringBuilderReference      : ClassBType = _
  var ThrowableReference          : ClassBType = _
  var jlCloneableReference        : ClassBType = _
  var jlNPEReference              : ClassBType = _
  var jioSerializableReference    : ClassBType = _
  var scalaSerializableReference  : ClassBType = _
  var classCastExceptionReference : ClassBType = _

  var srBooleanRef : ClassBType = _
  var srByteRef    : ClassBType = _
  var srCharRef    : ClassBType = _
  var srIntRef     : ClassBType = _
  var srLongRef    : ClassBType = _
  var srFloatRef   : ClassBType = _
  var srDoubleRef  : ClassBType = _

  var hashMethodSym: Symbol = _

  var AndroidParcelableInterface : Symbol = _
  var AndroidCreatorClass        : Symbol = _

  var BeanInfoAttr: Symbol = _

  /* The Object => String overload. */
  var String_valueOf: Symbol = _

  // scala.FunctionX and scala.runtim.AbstractFunctionX
  var FunctionReference         : Vector[ClassBType]   = _
  var AbstractFunctionReference : Vector[ClassBType]   = _
  var AbstractFunctionArityMap  : Map[ClassBType, Int] = _

  var PartialFunctionReference         : ClassBType = _
  var AbstractPartialFunctionReference : ClassBType = _

  var BoxesRunTime: ClassBType = _

  case class MethodNameAndType(name: String, methodType: MethodBType)

  /**
   * Methods in scala.runtime.BoxesRuntime
   */
  var asmBoxTo  : Map[BType, MethodNameAndType] = _
  var asmUnboxTo: Map[BType, MethodNameAndType] = _

  var typeOfArrayOp: Map[Int, BType] = _

  /**
   * Initialize core ClassBTypes computed from symbols. Invoked once for each compiler run.
   */
  def intializeCoreBTypes(): Unit = {
    primitiveTypeMap = Map(
      UnitClass    -> UNIT,
      BooleanClass -> BOOL,
      CharClass    -> CHAR,
      ByteClass    -> BYTE,
      ShortClass   -> SHORT,
      IntClass     -> INT,
      LongClass    -> LONG,
      FloatClass   -> FLOAT,
      DoubleClass  -> DOUBLE
    )

    BOXED_UNIT    = classBTypeFromSymbol(requiredClass[java.lang.Void])
    BOXED_BOOLEAN = classBTypeFromSymbol(BoxedBooleanClass)
    BOXED_BYTE    = classBTypeFromSymbol(BoxedByteClass)
    BOXED_SHORT   = classBTypeFromSymbol(BoxedShortClass)
    BOXED_CHAR    = classBTypeFromSymbol(BoxedCharacterClass)
    BOXED_INT     = classBTypeFromSymbol(BoxedIntClass)
    BOXED_LONG    = classBTypeFromSymbol(BoxedLongClass)
    BOXED_FLOAT   = classBTypeFromSymbol(BoxedFloatClass)
    BOXED_DOUBLE  = classBTypeFromSymbol(BoxedDoubleClass)

    boxedClassOfPrimitive = Map(
      UNIT   -> BOXED_UNIT,
      BOOL   -> BOXED_BOOLEAN,
      BYTE   -> BOXED_BYTE,
      SHORT  -> BOXED_SHORT,
      CHAR   -> BOXED_CHAR,
      INT    -> BOXED_INT,
      LONG   -> BOXED_LONG,
      FLOAT  -> BOXED_FLOAT,
      DOUBLE -> BOXED_DOUBLE
    )

    boxedClasses = boxedClassOfPrimitive.values.toSet

    boxResultType = {
      for ((valueClassSym, boxMethodSym) <- currentRun.runDefinitions.boxMethod)
      yield boxMethodSym -> boxedClassOfPrimitive(primitiveTypeMap(valueClassSym))
    }

    unboxResultType = {
      for ((valueClassSym, unboxMethodSym) <- currentRun.runDefinitions.unboxMethod)
      yield unboxMethodSym -> primitiveTypeMap(valueClassSym)
    }

    // TODO @lry Once there's a 2.11.2 starr, use the commented argument list. The current starr crashes on the type literal `scala.runtime.Nothing$`
    RT_NOTHING = classBTypeFromSymbol(rootMirror.getRequiredClass("scala.runtime.Nothing$")) // (requiredClass[scala.runtime.Nothing$])
    RT_NULL    = classBTypeFromSymbol(rootMirror.getRequiredClass("scala.runtime.Null$"))    // (requiredClass[scala.runtime.Null$])

    ObjectReference   = classBTypeFromSymbol(ObjectClass)
    objArrayReference = ArrayBType(ObjectReference)

    StringReference             = classBTypeFromSymbol(StringClass)
    StringBuilderReference      = classBTypeFromSymbol(StringBuilderClass)
    ThrowableReference          = classBTypeFromSymbol(ThrowableClass)
    jlCloneableReference        = classBTypeFromSymbol(JavaCloneableClass)        // java/lang/Cloneable
    jlNPEReference              = classBTypeFromSymbol(NullPointerExceptionClass) // java/lang/NullPointerException
    jioSerializableReference    = classBTypeFromSymbol(JavaSerializableClass)     // java/io/Serializable
    scalaSerializableReference  = classBTypeFromSymbol(SerializableClass)         // scala/Serializable
    classCastExceptionReference = classBTypeFromSymbol(ClassCastExceptionClass)   // java/lang/ClassCastException

    srBooleanRef = classBTypeFromSymbol(requiredClass[scala.runtime.BooleanRef])
    srByteRef    = classBTypeFromSymbol(requiredClass[scala.runtime.ByteRef])
    srCharRef    = classBTypeFromSymbol(requiredClass[scala.runtime.CharRef])
    srIntRef     = classBTypeFromSymbol(requiredClass[scala.runtime.IntRef])
    srLongRef    = classBTypeFromSymbol(requiredClass[scala.runtime.LongRef])
    srFloatRef   = classBTypeFromSymbol(requiredClass[scala.runtime.FloatRef])
    srDoubleRef  = classBTypeFromSymbol(requiredClass[scala.runtime.DoubleRef])

    hashMethodSym = getMember(ScalaRunTimeModule, nme.hash_)

    // TODO @lry avoiding going through through missingHook for every line in the REPL: https://github.com/scala/scala/commit/8d962ed4ddd310cc784121c426a2e3f56a112540
    AndroidParcelableInterface = getClassIfDefined("android.os.Parcelable")
    AndroidCreatorClass = getClassIfDefined("android.os.Parcelable$Creator")

    BeanInfoAttr = requiredClass[scala.beans.BeanInfo]

    String_valueOf = {
      getMember(StringModule, nme.valueOf) filter (sym => sym.info.paramTypes match {
        case List(pt) => pt.typeSymbol == ObjectClass
        case _        => false
      })
    }

    // scala.FunctionX and scala.runtim.AbstractFunctionX
    FunctionReference = (0 to MaxFunctionArity).map(i => classBTypeFromSymbol(FunctionClass(i)))(collection.breakOut)

    AbstractFunctionReference = (0 to MaxFunctionArity).map(i => classBTypeFromSymbol(AbstractFunctionClass(i)))(collection.breakOut)

    AbstractFunctionArityMap = AbstractFunctionReference.zipWithIndex.toMap

    PartialFunctionReference         = classBTypeFromSymbol(PartialFunctionClass)
    AbstractPartialFunctionReference = classBTypeFromSymbol(AbstractPartialFunctionClass)

    BoxesRunTime = classBTypeFromSymbol(requiredClass[scala.runtime.BoxesRunTime])

    asmBoxTo = Map(
      BOOL   -> MethodNameAndType("boxToBoolean",   MethodBType(List(BOOL),   BOXED_BOOLEAN)),
      BYTE   -> MethodNameAndType("boxToByte",      MethodBType(List(BYTE),   BOXED_BYTE)),
      CHAR   -> MethodNameAndType("boxToCharacter", MethodBType(List(CHAR),   BOXED_CHAR)),
      SHORT  -> MethodNameAndType("boxToShort",     MethodBType(List(SHORT),  BOXED_SHORT)),
      INT    -> MethodNameAndType("boxToInteger",   MethodBType(List(INT),    BOXED_INT)),
      LONG   -> MethodNameAndType("boxToLong",      MethodBType(List(LONG),   BOXED_LONG)),
      FLOAT  -> MethodNameAndType("boxToFloat",     MethodBType(List(FLOAT),  BOXED_FLOAT)),
      DOUBLE -> MethodNameAndType("boxToDouble",    MethodBType(List(DOUBLE), BOXED_DOUBLE))
    )

    asmUnboxTo = Map(
      BOOL   -> MethodNameAndType("unboxToBoolean", MethodBType(List(ObjectReference), BOOL)),
      BYTE   -> MethodNameAndType("unboxToByte",    MethodBType(List(ObjectReference), BYTE)),
      CHAR   -> MethodNameAndType("unboxToChar",    MethodBType(List(ObjectReference), CHAR)),
      SHORT  -> MethodNameAndType("unboxToShort",   MethodBType(List(ObjectReference), SHORT)),
      INT    -> MethodNameAndType("unboxToInt",     MethodBType(List(ObjectReference), INT)),
      LONG   -> MethodNameAndType("unboxToLong",    MethodBType(List(ObjectReference), LONG)),
      FLOAT  -> MethodNameAndType("unboxToFloat",   MethodBType(List(ObjectReference), FLOAT)),
      DOUBLE -> MethodNameAndType("unboxToDouble",  MethodBType(List(ObjectReference), DOUBLE))
    )

    typeOfArrayOp = {
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
          (List(OARRAY_LENGTH, OARRAY_GET, OARRAY_SET) map (_ -> ObjectReference)) : _*
      )
    }
  }
}
