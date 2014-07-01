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

    primitiveTypeMap =
      Map(
        UnitClass     -> UNIT,
        BooleanClass  -> BOOL,
        CharClass     -> CHAR,
        ByteClass     -> BYTE,
        ShortClass    -> SHORT,
        IntClass      -> INT,
        LongClass     -> LONG,
        FloatClass    -> FLOAT,
        DoubleClass   -> DOUBLE
      )

    phantomTypeMap =
      Map(
        NothingClass -> RT_NOTHING,
        NullClass    -> RT_NULL,
        NothingClass -> RT_NOTHING, // we map on purpose to RT_NOTHING, getting rid of the distinction compile-time vs. runtime for NullClass.
        NullClass    -> RT_NULL     // ditto.
      )

  val BOXED_UNIT    = ClassBType("java/lang/Void")
  val BOXED_BOOLEAN = ClassBType("java/lang/Boolean")
  val BOXED_BYTE    = ClassBType("java/lang/Byte")
  val BOXED_SHORT   = ClassBType("java/lang/Short")
  val BOXED_CHAR    = ClassBType("java/lang/Character")
  val BOXED_INT     = ClassBType("java/lang/Integer")
  val BOXED_LONG    = ClassBType("java/lang/Long")
  val BOXED_FLOAT   = ClassBType("java/lang/Float")
  val BOXED_DOUBLE  = ClassBType("java/lang/Double")

  /**
   * Map from type kinds to the Java reference types.
   * Useful when pushing class literals onto the operand stack (ldc instruction taking a class
   * literal).
   * @see Predef.classOf
   * @see genConstant()
   *
   * TODO @lry rename to "boxedClassOfPrimitive" or so, check usages
   */
  val classLiteral = immutable.Map[BType, ClassBType](
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

    boxResultType =
      for((csym, msym) <- currentRun.runDefinitions.boxMethod)
      yield (msym -> classLiteral(primitiveTypeMap(csym)))

    unboxResultType =
      for((csym, msym) <- currentRun.runDefinitions.unboxMethod)
      yield (msym -> primitiveTypeMap(csym))

  /*
   * RT_NOTHING and RT_NULL exist at run-time only. They are the bytecode-level manifestation (in
   * method signatures only) of what shows up as NothingClass resp. NullClass in Scala ASTs.
   *
   * Therefore, when RT_NOTHING or RT_NULL are to be emitted, a mapping is needed: the internal
   * names of NothingClass and NullClass can't be emitted as-is.
   */
  val RT_NOTHING = ClassBType("scala/runtime/Nothing$")
  val RT_NULL    = ClassBType("scala/runtime/Null$")
  val CT_NOTHING = ClassBType("scala/Nothing")
  val CT_NULL    = ClassBType("scala/Null")

  val ObjectReference   = ClassBType("java/lang/Object")
  val AnyRefReference   = ObjectReference
  val objArrayReference = ArrayBType(ObjectReference)

    StringReference             = exemplar(StringClass).c
    StringBuilderReference      = exemplar(StringBuilderClass).c
    ThrowableReference          = exemplar(ThrowableClass).c
    jlCloneableReference        = exemplar(JavaCloneableClass).c
    jlNPEReference              = exemplar(NullPointerExceptionClass).c
    jioSerializableReference    = exemplar(JavaSerializableClass).c
    scalaSerializableReference  = exemplar(SerializableClass).c
    classCastExceptionReference = exemplar(ClassCastExceptionClass).c

  val srBooleanRef = ClassBType("scala/runtime/BooleanRef")
  val srByteRef    = ClassBType("scala/runtime/ByteRef")
  val srCharRef    = ClassBType("scala/runtime/CharRef")
  val srIntRef     = ClassBType("scala/runtime/IntRef")
  val srLongRef    = ClassBType("scala/runtime/LongRef")
  val srFloatRef   = ClassBType("scala/runtime/FloatRef")
  val srDoubleRef  = ClassBType("scala/runtime/DoubleRef")

    hashMethodSym = getMember(ScalaRunTimeModule, nme.hash_)

    // TODO avoiding going through through missingHook for every line in the REPL: https://github.com/scala/scala/commit/8d962ed4ddd310cc784121c426a2e3f56a112540
    AndroidParcelableInterface = rootMirror.getClassIfDefined("android.os.Parcelable")
    AndroidCreatorClass        = rootMirror.getClassIfDefined("android.os.Parcelable$Creator")

    // the following couldn't be an eager vals in Phase constructors:
    // that might cause cycles before Global has finished initialization.
    BeanInfoAttr = rootMirror.getRequiredClass("scala.beans.BeanInfo")

    String_valueOf = {
      getMember(StringModule, nme.valueOf) filter (sym =>
        sym.info.paramTypes match {
          case List(pt) => pt.typeSymbol == ObjectClass
          case _        => false
        }
      )
    }

    PartialFunctionReference    = exemplar(PartialFunctionClass).c
    for(idx <- 0 to definitions.MaxFunctionArity) {
      FunctionReference(idx)           = exemplar(FunctionClass(idx))
      AbstractFunctionReference(idx)   = exemplar(AbstractFunctionClass(idx))
      abstractFunctionArityMap        += (AbstractFunctionReference(idx).c -> idx)
      AbstractPartialFunctionReference = exemplar(AbstractPartialFunctionClass).c
    }


    BoxesRunTime = ClassBType("scala/runtime/BoxesRunTime")

  case class MethodNameAndType(name: String, descriptor: String)

  val asmBoxTo: immutable.Map[BType, MethodNameAndType] = {
    Map(
      BOOL   -> MethodNameAndType("boxToBoolean",   "(Z)Ljava/lang/Boolean;"  ) ,
      BYTE   -> MethodNameAndType("boxToByte",      "(B)Ljava/lang/Byte;"     ) ,
      CHAR   -> MethodNameAndType("boxToCharacter", "(C)Ljava/lang/Character;") ,
      SHORT  -> MethodNameAndType("boxToShort",     "(S)Ljava/lang/Short;"    ) ,
      INT    -> MethodNameAndType("boxToInteger",   "(I)Ljava/lang/Integer;"  ) ,
      LONG   -> MethodNameAndType("boxToLong",      "(J)Ljava/lang/Long;"     ) ,
      FLOAT  -> MethodNameAndType("boxToFloat",     "(F)Ljava/lang/Float;"    ) ,
      DOUBLE -> MethodNameAndType("boxToDouble",    "(D)Ljava/lang/Double;"   )
    )
  }

  val asmUnboxTo: immutable.Map[BType, MethodNameAndType] = {
    Map(
      BOOL   -> MethodNameAndType("unboxToBoolean", "(Ljava/lang/Object;)Z") ,
      BYTE   -> MethodNameAndType("unboxToByte",    "(Ljava/lang/Object;)B") ,
      CHAR   -> MethodNameAndType("unboxToChar",    "(Ljava/lang/Object;)C") ,
      SHORT  -> MethodNameAndType("unboxToShort",   "(Ljava/lang/Object;)S") ,
      INT    -> MethodNameAndType("unboxToInt",     "(Ljava/lang/Object;)I") ,
      LONG   -> MethodNameAndType("unboxToLong",    "(Ljava/lang/Object;)J") ,
      FLOAT  -> MethodNameAndType("unboxToFloat",   "(Ljava/lang/Object;)F") ,
      DOUBLE -> MethodNameAndType("unboxToDouble",  "(Ljava/lang/Object;)D")
    )
  }

  final val typeOfArrayOp: Map[Int, BType] = {
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
