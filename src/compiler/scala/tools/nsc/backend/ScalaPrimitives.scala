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

package scala
package tools.nsc
package backend

import scala.collection.mutable

/** Scala primitive operations are represented as methods in `Any` and
 *  `AnyVal` subclasses. Here we demultiplex them by providing a mapping
 *  from their symbols to integers. Different methods exist for
 *  different value types, but with the same meaning (like plus, minus,
 *  etc.). They will all be mapped to the same int.
 *
 *  Note: The three equal methods have the following semantics:
 *  - `"=="` checks for `null`, and if non-null, calls
 *    `java.lang.Object.equals`
 *    `(class: Any; modifier: final)`. Primitive: `EQ`
 *  - `"eq"` usual reference comparison
 *    `(class: AnyRef; modifier: final)`. Primitive: `ID`
 *  - `"equals"` user-defined equality (Java semantics)
 *    `(class: Object; modifier: none)`. Primitive: `EQUALS`
 *
 * Inspired from the `scalac` compiler.
 */
abstract class ScalaPrimitives {
  val global: Global

  import global._
  import definitions._

  // Arithmetic unary operations
  final val POS = 1                            // +x
  final val NEG = 2                            // -x
  final val NOT = 3                            // ~x

  // Arithmetic binary operations
  final val ADD = 10                           // x + y
  final val SUB = 11                           // x - y
  final val MUL = 12                           // x * y
  final val DIV = 13                           // x / y
  final val MOD = 14                           // x % y

  // Bitwise operations
  final val OR  = 20                           // x | y
  final val XOR = 21                           // x ^ y
  final val AND = 22                           // x & y

  // Shift operations
  final val LSL = 30                           // x << y
  final val LSR = 31                           // x >>> y
  final val ASR = 32                           // x >> y

  // Comparison operations
  final val ID = 40                            // x eq y
  final val NI = 41                            // x ne y
  final val EQ = 42                            // x == y
  final val NE = 43                            // x != y
  final val LT = 44                            // x < y
  final val LE = 45                            // x <= y
  final val GT = 46                            // x > y
  final val GE = 47                            // x >= y

  // Boolean unary operations
  final val ZNOT = 50                          // !x

  // Boolean binary operations
  final val ZOR = 60                           // x || y
  final val ZAND = 61                          // x && y

  // Array operations
  final val LENGTH = 70                        // x.length
  final val APPLY  = 71                        // x(y)
  final val UPDATE = 72                        // x(y) = z

  // Any operations
  final val IS = 80                            // x.is[y]
  final val AS = 81                            // x.as[y]
  final val HASH = 87                          // x.##

  // AnyRef operations
  final val SYNCHRONIZED = 90                  // x.synchronized(y)

  // String operations
  final val CONCAT = 100                       // String.valueOf(x)+String.valueOf(y)

  // coercions
  final val COERCE = 101

  // RunTime operations
  final val BOX = 110                          // RunTime.box_<X>(x)
  final val UNBOX = 111                        // RunTime.unbox_<X>(x)
  final val NEW_ZARRAY = 112                   // RunTime.zarray(x)
  final val NEW_BARRAY = 113                   // RunTime.barray(x)
  final val NEW_SARRAY = 114                   // RunTime.sarray(x)
  final val NEW_CARRAY = 115                   // RunTime.carray(x)
  final val NEW_IARRAY = 116                   // RunTime.iarray(x)
  final val NEW_LARRAY = 117                   // RunTime.larray(x)
  final val NEW_FARRAY = 118                   // RunTime.farray(x)
  final val NEW_DARRAY = 119                   // RunTime.darray(x)
  final val NEW_OARRAY = 120                   // RunTime.oarray(x)

  final val ZARRAY_LENGTH = 131                // RunTime.zarray_length(x)
  final val BARRAY_LENGTH = 132                // RunTime.barray_length(x)
  final val SARRAY_LENGTH = 133                // RunTime.sarray_length(x)
  final val CARRAY_LENGTH = 134                // RunTime.carray_length(x)
  final val IARRAY_LENGTH = 135                // RunTime.iarray_length(x)
  final val LARRAY_LENGTH = 136                // RunTime.larray_length(x)
  final val FARRAY_LENGTH = 137                // RunTime.farray_length(x)
  final val DARRAY_LENGTH = 138                // RunTime.darray_length(x)
  final val OARRAY_LENGTH = 139                // RunTime.oarray_length(x)

  final val ZARRAY_GET = 140                   // RunTime.zarray_get(x,y)
  final val BARRAY_GET = 141                   // RunTime.barray_get(x,y)
  final val SARRAY_GET = 142                   // RunTime.sarray_get(x,y)
  final val CARRAY_GET = 143                   // RunTime.carray_get(x,y)
  final val IARRAY_GET = 144                   // RunTime.iarray_get(x,y)
  final val LARRAY_GET = 145                   // RunTime.larray_get(x,y)
  final val FARRAY_GET = 146                   // RunTime.farray_get(x,y)
  final val DARRAY_GET = 147                   // RunTime.darray_get(x,y)
  final val OARRAY_GET = 148                   // RunTime.oarray_get(x,y)

  final val ZARRAY_SET = 150                   // RunTime.zarray(x,y,z)
  final val BARRAY_SET = 151                   // RunTime.barray(x,y,z)
  final val SARRAY_SET = 152                   // RunTime.sarray(x,y,z)
  final val CARRAY_SET = 153                   // RunTime.carray(x,y,z)
  final val IARRAY_SET = 154                   // RunTime.iarray(x,y,z)
  final val LARRAY_SET = 155                   // RunTime.larray(x,y,z)
  final val FARRAY_SET = 156                   // RunTime.farray(x,y,z)
  final val DARRAY_SET = 157                   // RunTime.darray(x,y,z)
  final val OARRAY_SET = 158                   // RunTime.oarray(x,y,z)

  final val B2B = 200                          // RunTime.b2b(x)
  final val B2S = 201                          // RunTime.b2s(x)
  final val B2C = 202                          // RunTime.b2c(x)
  final val B2I = 203                          // RunTime.b2i(x)
  final val B2L = 204                          // RunTime.b2l(x)
  final val B2F = 205                          // RunTime.b2f(x)
  final val B2D = 206                          // RunTime.b2d(x)

  final val S2B = 210                          // RunTime.s2b(x)
  final val S2S = 211                          // RunTime.s2s(x)
  final val S2C = 212                          // RunTime.s2c(x)
  final val S2I = 213                          // RunTime.s2i(x)
  final val S2L = 214                          // RunTime.s2l(x)
  final val S2F = 215                          // RunTime.s2f(x)
  final val S2D = 216                          // RunTime.s2d(x)

  final val C2B = 220                          // RunTime.c2b(x)
  final val C2S = 221                          // RunTime.c2s(x)
  final val C2C = 222                          // RunTime.c2c(x)
  final val C2I = 223                          // RunTime.c2i(x)
  final val C2L = 224                          // RunTime.c2l(x)
  final val C2F = 225                          // RunTime.c2f(x)
  final val C2D = 226                          // RunTime.c2d(x)

  final val I2B = 230                          // RunTime.i2b(x)
  final val I2S = 231                          // RunTime.i2s(x)
  final val I2C = 232                          // RunTime.i2c(x)
  final val I2I = 233                          // RunTime.i2i(x)
  final val I2L = 234                          // RunTime.i2l(x)
  final val I2F = 235                          // RunTime.i2f(x)
  final val I2D = 236                          // RunTime.i2d(x)

  final val L2B = 240                          // RunTime.l2b(x)
  final val L2S = 241                          // RunTime.l2s(x)
  final val L2C = 242                          // RunTime.l2c(x)
  final val L2I = 243                          // RunTime.l2i(x)
  final val L2L = 244                          // RunTime.l2l(x)
  final val L2F = 245                          // RunTime.l2f(x)
  final val L2D = 246                          // RunTime.l2d(x)

  final val F2B = 250                          // RunTime.f2b(x)
  final val F2S = 251                          // RunTime.f2s(x)
  final val F2C = 252                          // RunTime.f2c(x)
  final val F2I = 253                          // RunTime.f2i(x)
  final val F2L = 254                          // RunTime.f2l(x)
  final val F2F = 255                          // RunTime.f2f(x)
  final val F2D = 256                          // RunTime.f2d(x)

  final val D2B = 260                          // RunTime.d2b(x)
  final val D2S = 261                          // RunTime.d2s(x)
  final val D2C = 262                          // RunTime.d2c(x)
  final val D2I = 263                          // RunTime.d2i(x)
  final val D2L = 264                          // RunTime.d2l(x)
  final val D2F = 265                          // RunTime.d2f(x)
  final val D2D = 266                          // RunTime.d2d(x)

  private val primitives: mutable.Map[Symbol, Int] = new mutable.HashMap()

  /** Initialize the primitive map */
  def init(): Unit = {
    primitives.clear()
    // scala.Any
    addPrimitive(Any_==, EQ)
    addPrimitive(Any_!=, NE)
    addPrimitive(Any_isInstanceOf, IS)
    addPrimitive(Any_asInstanceOf, AS)
    addPrimitive(Any_##, HASH)

    // java.lang.Object
    addPrimitive(Object_eq, ID)
    addPrimitive(Object_ne, NI)
    addPrimitive(Object_==, EQ)
    addPrimitive(Object_!=, NE)
    addPrimitive(Object_synchronized, SYNCHRONIZED)
    addPrimitive(Object_isInstanceOf, IS)
    addPrimitive(Object_asInstanceOf, AS)

    // java.lang.String
    addPrimitive(String_+, CONCAT)

    // scala.Array
    addPrimitives(ArrayClass, nme.length, LENGTH)
    addPrimitives(ArrayClass, nme.apply, APPLY)
    addPrimitives(ArrayClass, nme.update, UPDATE)

    // scala.Boolean
    addPrimitives(BooleanClass, nme.EQ, EQ)
    addPrimitives(BooleanClass, nme.NE, NE)
    addPrimitives(BooleanClass, nme.UNARY_!, ZNOT)
    addPrimitives(BooleanClass, nme.ZOR, ZOR)
    addPrimitives(BooleanClass, nme.ZAND, ZAND)
    addPrimitives(BooleanClass, nme.OR, OR)
    addPrimitives(BooleanClass, nme.AND, AND)
    addPrimitives(BooleanClass, nme.XOR, XOR)

    // scala.Byte
    addPrimitives(ByteClass, nme.EQ, EQ)
    addPrimitives(ByteClass, nme.NE, NE)
    addPrimitives(ByteClass, nme.ADD, ADD)
    addPrimitives(ByteClass, nme.SUB, SUB)
    addPrimitives(ByteClass, nme.MUL, MUL)
    addPrimitives(ByteClass, nme.DIV, DIV)
    addPrimitives(ByteClass, nme.MOD, MOD)
    addPrimitives(ByteClass, nme.LT, LT)
    addPrimitives(ByteClass, nme.LE, LE)
    addPrimitives(ByteClass, nme.GT, GT)
    addPrimitives(ByteClass, nme.GE, GE)
    addPrimitives(ByteClass, nme.XOR, XOR)
    addPrimitives(ByteClass, nme.OR, OR)
    addPrimitives(ByteClass, nme.AND, AND)
    addPrimitives(ByteClass, nme.LSL, LSL)
    addPrimitives(ByteClass, nme.LSR, LSR)
    addPrimitives(ByteClass, nme.ASR, ASR)
      // conversions
    addPrimitives(ByteClass, nme.toByte,   B2B)
    addPrimitives(ByteClass, nme.toShort,  B2S)
    addPrimitives(ByteClass, nme.toChar,   B2C)
    addPrimitives(ByteClass, nme.toInt,    B2I)
    addPrimitives(ByteClass, nme.toLong,   B2L)
    // unary methods
    addPrimitives(ByteClass, nme.UNARY_+, POS)
    addPrimitives(ByteClass, nme.UNARY_-, NEG)
    addPrimitives(ByteClass, nme.UNARY_~, NOT)

    addPrimitives(ByteClass, nme.toFloat,  B2F)
    addPrimitives(ByteClass, nme.toDouble, B2D)

    // scala.Short
    addPrimitives(ShortClass, nme.EQ, EQ)
    addPrimitives(ShortClass, nme.NE, NE)
    addPrimitives(ShortClass, nme.ADD, ADD)
    addPrimitives(ShortClass, nme.SUB, SUB)
    addPrimitives(ShortClass, nme.MUL, MUL)
    addPrimitives(ShortClass, nme.DIV, DIV)
    addPrimitives(ShortClass, nme.MOD, MOD)
    addPrimitives(ShortClass, nme.LT, LT)
    addPrimitives(ShortClass, nme.LE, LE)
    addPrimitives(ShortClass, nme.GT, GT)
    addPrimitives(ShortClass, nme.GE, GE)
    addPrimitives(ShortClass, nme.XOR, XOR)
    addPrimitives(ShortClass, nme.OR, OR)
    addPrimitives(ShortClass, nme.AND, AND)
    addPrimitives(ShortClass, nme.LSL, LSL)
    addPrimitives(ShortClass, nme.LSR, LSR)
    addPrimitives(ShortClass, nme.ASR, ASR)
      // conversions
    addPrimitives(ShortClass, nme.toByte,   S2B)
    addPrimitives(ShortClass, nme.toShort,  S2S)
    addPrimitives(ShortClass, nme.toChar,   S2C)
    addPrimitives(ShortClass, nme.toInt,    S2I)
    addPrimitives(ShortClass, nme.toLong,   S2L)
    // unary methods
    addPrimitives(ShortClass, nme.UNARY_+, POS)
    addPrimitives(ShortClass, nme.UNARY_-, NEG)
    addPrimitives(ShortClass, nme.UNARY_~, NOT)

    addPrimitives(ShortClass, nme.toFloat,  S2F)
    addPrimitives(ShortClass, nme.toDouble, S2D)

    // scala.Char
    addPrimitives(CharClass, nme.EQ, EQ)
    addPrimitives(CharClass, nme.NE, NE)
    addPrimitives(CharClass, nme.ADD, ADD)
    addPrimitives(CharClass, nme.SUB, SUB)
    addPrimitives(CharClass, nme.MUL, MUL)
    addPrimitives(CharClass, nme.DIV, DIV)
    addPrimitives(CharClass, nme.MOD, MOD)
    addPrimitives(CharClass, nme.LT, LT)
    addPrimitives(CharClass, nme.LE, LE)
    addPrimitives(CharClass, nme.GT, GT)
    addPrimitives(CharClass, nme.GE, GE)
    addPrimitives(CharClass, nme.XOR, XOR)
    addPrimitives(CharClass, nme.OR, OR)
    addPrimitives(CharClass, nme.AND, AND)
    addPrimitives(CharClass, nme.LSL, LSL)
    addPrimitives(CharClass, nme.LSR, LSR)
    addPrimitives(CharClass, nme.ASR, ASR)
      // conversions
    addPrimitives(CharClass, nme.toByte,   C2B)
    addPrimitives(CharClass, nme.toShort,  C2S)
    addPrimitives(CharClass, nme.toChar,   C2C)
    addPrimitives(CharClass, nme.toInt,    C2I)
    addPrimitives(CharClass, nme.toLong,   C2L)
    // unary methods
    addPrimitives(CharClass, nme.UNARY_+, POS)
    addPrimitives(CharClass, nme.UNARY_-, NEG)
    addPrimitives(CharClass, nme.UNARY_~, NOT)
    addPrimitives(CharClass, nme.toFloat,  C2F)
    addPrimitives(CharClass, nme.toDouble, C2D)

    // scala.Int
    addPrimitives(IntClass, nme.EQ, EQ)
    addPrimitives(IntClass, nme.NE, NE)
    addPrimitives(IntClass, nme.ADD, ADD)
    addPrimitives(IntClass, nme.SUB, SUB)
    addPrimitives(IntClass, nme.MUL, MUL)
    addPrimitives(IntClass, nme.DIV, DIV)
    addPrimitives(IntClass, nme.MOD, MOD)
    addPrimitives(IntClass, nme.LT, LT)
    addPrimitives(IntClass, nme.LE, LE)
    addPrimitives(IntClass, nme.GT, GT)
    addPrimitives(IntClass, nme.GE, GE)
    addPrimitives(IntClass, nme.XOR, XOR)
    addPrimitives(IntClass, nme.OR, OR)
    addPrimitives(IntClass, nme.AND, AND)
    addPrimitives(IntClass, nme.LSL, LSL)
    addPrimitives(IntClass, nme.LSR, LSR)
    addPrimitives(IntClass, nme.ASR, ASR)
      // conversions
    addPrimitives(IntClass, nme.toByte,   I2B)
    addPrimitives(IntClass, nme.toShort,  I2S)
    addPrimitives(IntClass, nme.toChar,   I2C)
    addPrimitives(IntClass, nme.toInt,    I2I)
    addPrimitives(IntClass, nme.toLong,   I2L)
    // unary methods
    addPrimitives(IntClass, nme.UNARY_+, POS)
    addPrimitives(IntClass, nme.UNARY_-, NEG)
    addPrimitives(IntClass, nme.UNARY_~, NOT)
    addPrimitives(IntClass, nme.toFloat,  I2F)
    addPrimitives(IntClass, nme.toDouble, I2D)

    // scala.Long
    addPrimitives(LongClass, nme.EQ, EQ)
    addPrimitives(LongClass, nme.NE, NE)
    addPrimitives(LongClass, nme.ADD, ADD)
    addPrimitives(LongClass, nme.SUB, SUB)
    addPrimitives(LongClass, nme.MUL, MUL)
    addPrimitives(LongClass, nme.DIV, DIV)
    addPrimitives(LongClass, nme.MOD, MOD)
    addPrimitives(LongClass, nme.LT, LT)
    addPrimitives(LongClass, nme.LE, LE)
    addPrimitives(LongClass, nme.GT, GT)
    addPrimitives(LongClass, nme.GE, GE)
    addPrimitives(LongClass, nme.XOR, XOR)
    addPrimitives(LongClass, nme.OR, OR)
    addPrimitives(LongClass, nme.AND, AND)
    addPrimitives(LongClass, nme.LSL, LSL)
    addPrimitives(LongClass, nme.LSR, LSR)
    addPrimitives(LongClass, nme.ASR, ASR)
      // conversions
    addPrimitives(LongClass, nme.toByte,   L2B)
    addPrimitives(LongClass, nme.toShort,  L2S)
    addPrimitives(LongClass, nme.toChar,   L2C)
    addPrimitives(LongClass, nme.toInt,    L2I)
    addPrimitives(LongClass, nme.toLong,   L2L)
    // unary methods
    addPrimitives(LongClass, nme.UNARY_+, POS)
    addPrimitives(LongClass, nme.UNARY_-, NEG)
    addPrimitives(LongClass, nme.UNARY_~, NOT)
    addPrimitives(LongClass, nme.toFloat,  L2F)
    addPrimitives(LongClass, nme.toDouble, L2D)

    // scala.Float
    addPrimitives(FloatClass, nme.EQ, EQ)
    addPrimitives(FloatClass, nme.NE, NE)
    addPrimitives(FloatClass, nme.ADD, ADD)
    addPrimitives(FloatClass, nme.SUB, SUB)
    addPrimitives(FloatClass, nme.MUL, MUL)
    addPrimitives(FloatClass, nme.DIV, DIV)
    addPrimitives(FloatClass, nme.MOD, MOD)
    addPrimitives(FloatClass, nme.LT, LT)
    addPrimitives(FloatClass, nme.LE, LE)
    addPrimitives(FloatClass, nme.GT, GT)
    addPrimitives(FloatClass, nme.GE, GE)
    // conversions
    addPrimitives(FloatClass, nme.toByte,   F2B)
    addPrimitives(FloatClass, nme.toShort,  F2S)
    addPrimitives(FloatClass, nme.toChar,   F2C)
    addPrimitives(FloatClass, nme.toInt,    F2I)
    addPrimitives(FloatClass, nme.toLong,   F2L)
    addPrimitives(FloatClass, nme.toFloat,  F2F)
    addPrimitives(FloatClass, nme.toDouble, F2D)
    // unary methods
    addPrimitives(FloatClass, nme.UNARY_+, POS)
    addPrimitives(FloatClass, nme.UNARY_-, NEG)

    // scala.Double
    addPrimitives(DoubleClass, nme.EQ, EQ)
    addPrimitives(DoubleClass, nme.NE, NE)
    addPrimitives(DoubleClass, nme.ADD, ADD)
    addPrimitives(DoubleClass, nme.SUB, SUB)
    addPrimitives(DoubleClass, nme.MUL, MUL)
    addPrimitives(DoubleClass, nme.DIV, DIV)
    addPrimitives(DoubleClass, nme.MOD, MOD)
    addPrimitives(DoubleClass, nme.LT, LT)
    addPrimitives(DoubleClass, nme.LE, LE)
    addPrimitives(DoubleClass, nme.GT, GT)
    addPrimitives(DoubleClass, nme.GE, GE)
    // conversions
    addPrimitives(DoubleClass, nme.toByte,   D2B)
    addPrimitives(DoubleClass, nme.toShort,  D2S)
    addPrimitives(DoubleClass, nme.toChar,   D2C)
    addPrimitives(DoubleClass, nme.toInt,    D2I)
    addPrimitives(DoubleClass, nme.toLong,   D2L)
    addPrimitives(DoubleClass, nme.toFloat,  D2F)
    addPrimitives(DoubleClass, nme.toDouble, D2D)
    // unary methods
    addPrimitives(DoubleClass, nme.UNARY_+, POS)
    addPrimitives(DoubleClass, nme.UNARY_-, NEG)
  }

  /** Add a primitive operation to the map */
  def addPrimitive(s: Symbol, code: Int): Unit = {
    assert(!(primitives contains s), "Duplicate primitive " + s)
    primitives(s) = code
  }

  def addPrimitives(cls: Symbol, method: Name, code: Int): Unit = {
    val alts = (cls.info member method).alternatives
    if (alts.isEmpty)
      inform(s"Unknown primitive method $cls.$method")
    else alts foreach (s =>
      addPrimitive(s,
        if (code != ADD) code
        else exitingTyper(s.info).paramTypes match {
          case tp :: _ if tp =:= StringTpe => CONCAT
          case _                           => code
        }
      )
    )
  }

  def isCoercion(code: Int): Boolean = (code >= B2B) && (code <= D2D)

  /** Check whether the given operation code is an array operation. */
  def isArrayOp(code: Int): Boolean =
    isArrayNew(code) | isArrayLength(code) | isArrayGet(code) | isArraySet(code)

  def isArrayNew(code: Int): Boolean = code match {
    case NEW_ZARRAY | NEW_BARRAY | NEW_SARRAY | NEW_CARRAY |
         NEW_IARRAY | NEW_LARRAY | NEW_FARRAY | NEW_DARRAY |
         NEW_OARRAY => true
    case _ => false
  }

  def isArrayLength(code: Int): Boolean = code match {
    case ZARRAY_LENGTH | BARRAY_LENGTH | SARRAY_LENGTH | CARRAY_LENGTH |
         IARRAY_LENGTH | LARRAY_LENGTH | FARRAY_LENGTH | DARRAY_LENGTH |
         OARRAY_LENGTH | LENGTH => true
    case _ => false
  }

  def isArrayGet(code: Int): Boolean = code match {
    case ZARRAY_GET | BARRAY_GET | SARRAY_GET | CARRAY_GET |
         IARRAY_GET | LARRAY_GET | FARRAY_GET | DARRAY_GET |
         OARRAY_GET | APPLY => true
    case _ => false
  }

  def isArraySet(code: Int): Boolean = code match {
    case ZARRAY_SET | BARRAY_SET | SARRAY_SET | CARRAY_SET |
         IARRAY_SET | LARRAY_SET | FARRAY_SET | DARRAY_SET |
         OARRAY_SET | UPDATE => true
    case _ => false
  }

  /** Check whether the given code is a comparison operator */
  def isComparisonOp(code: Int): Boolean = code match {
    case ID | NI | EQ | NE |
         LT | LE | GT | GE => true

    case _ => false
  }
  def isUniversalEqualityOp(code: Int): Boolean = (code == EQ) || (code == NE)
  def isReferenceEqualityOp(code: Int): Boolean = (code == ID) || (code == NI)

  def isArithmeticOp(code: Int): Boolean = code match {
    case POS | NEG | NOT => true; // unary
    case ADD | SUB | MUL |
         DIV | MOD       => true; // binary
    case OR  | XOR | AND |
         LSL | LSR | ASR => true; // bitwise
    case _ => false
  }

  def isLogicalOp(code: Int): Boolean = code match {
    case ZNOT | ZAND | ZOR => true
    case _ => false
  }

  def isShiftOp(code: Int): Boolean = code match {
    case LSL | LSR | ASR => true
    case _ => false
  }

  def isBitwiseOp(code: Int): Boolean = code match {
    case OR | XOR | AND => true
    case _ => false
  }

  def isPrimitive(sym: Symbol): Boolean = primitives contains sym

  /** Return the code for the given symbol. */
  def getPrimitive(sym: Symbol): Int =
    primitives.getOrElse(sym, throw new AssertionError(s"Unknown primitive $sym"))

  /**
   * Return the primitive code of the given operation. If the
   * operation is an array get/set, we inspect the type of the receiver
   * to demux the operation.
   *
   * @param fun The method symbol
   * @param tpe The type of the receiver object. It is used only for array
   *            operations
   */
  def getPrimitive(fun: Symbol, tpe: Type): Int = {
    import definitions._
    import genBCode.bTypes._
    val code = getPrimitive(fun)

    def elementType = enteringTyper {
      val arrayParent = tpe :: tpe.parents collectFirst {
        case TypeRef(_, ArrayClass, elem :: Nil) => elem
      }
      arrayParent getOrElse abort(fun.fullName + " : " + (tpe :: tpe.baseTypeSeq.toList).mkString(", "))
    }

    code match {

      case APPLY =>
        typeToBType(elementType) match {
          case BOOL    => ZARRAY_GET
          case BYTE    => BARRAY_GET
          case SHORT   => SARRAY_GET
          case CHAR    => CARRAY_GET
          case INT     => IARRAY_GET
          case LONG    => LARRAY_GET
          case FLOAT   => FARRAY_GET
          case DOUBLE  => DARRAY_GET
          case _: ClassBType | _: ArrayBType => OARRAY_GET
          case _ =>
            abort("Unexpected array element type: " + elementType)
        }

      case UPDATE =>
        typeToBType(elementType) match {
          case BOOL    => ZARRAY_SET
          case BYTE    => BARRAY_SET
          case SHORT   => SARRAY_SET
          case CHAR    => CARRAY_SET
          case INT     => IARRAY_SET
          case LONG    => LARRAY_SET
          case FLOAT   => FARRAY_SET
          case DOUBLE  => DARRAY_SET
          case _: ClassBType | _: ArrayBType => OARRAY_SET
          case _ =>
            abort("Unexpected array element type: " + elementType)
        }

      case LENGTH =>
        typeToBType(elementType) match {
          case BOOL    => ZARRAY_LENGTH
          case BYTE    => BARRAY_LENGTH
          case SHORT   => SARRAY_LENGTH
          case CHAR    => CARRAY_LENGTH
          case INT     => IARRAY_LENGTH
          case LONG    => LARRAY_LENGTH
          case FLOAT   => FARRAY_LENGTH
          case DOUBLE  => DARRAY_LENGTH
          case _: ClassBType | _: ArrayBType => OARRAY_LENGTH
          case _ =>
            abort("Unexpected array element type: " + elementType)
        }

      case _ =>
        code
    }
  }

}
