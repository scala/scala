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

package scala.reflect
package api

trait StandardLiftables { self: Universe =>
  import internal._
  import reificationSupport.{SyntacticTuple, ScalaDot}

  trait StandardLiftableInstances {
    private def lift[T: Liftable](value: T): Tree            = implicitly[Liftable[T]].apply(value)
    private def selectScala(names: Name*)                    = names.tail.foldLeft(ScalaDot(names.head)) { Select(_, _) }
    private def callScala(names: Name*)(args: List[Tree])    = Apply(selectScala(names: _*), args)
    private def callCollection(name: Name)(args: List[Tree]) = callScala(stdnme.collection, stdnme.immutable, name)(args)
    private def liftAsLiteral[T]: Liftable[T]                = Liftable { v => Literal(Constant(v)) }

    implicit def liftByte[T <: Byte]: Liftable[T]       = liftAsLiteral[T]
    implicit def liftShort[T <: Short]: Liftable[T]     = liftAsLiteral[T]
    implicit def liftChar[T <: Char]: Liftable[T]       = liftAsLiteral[T]
    implicit def liftInt[T <: Int]: Liftable[T]         = liftAsLiteral[T]
    implicit def liftLong[T <: Long]: Liftable[T]       = liftAsLiteral[T]
    implicit def liftFloat[T <: Float]: Liftable[T]     = liftAsLiteral[T]
    implicit def liftDouble[T <: Double]: Liftable[T]   = liftAsLiteral[T]
    implicit def liftBoolean[T <: Boolean]: Liftable[T] = liftAsLiteral[T]
    implicit def liftUnit: Liftable[Unit]               = liftAsLiteral[Unit]
    implicit def liftString[T <: String]: Liftable[T]   = liftAsLiteral[T]

    implicit def liftScalaSymbol: Liftable[scala.Symbol] = Liftable { v =>
      callScala(stdnme.Symbol)(Literal(Constant(v.name)) :: Nil)
    }

    implicit def liftTree[T <: Tree]: Liftable[T]              = Liftable { identity }
    implicit def liftName[T <: Name]: Liftable[T]              = Liftable { name => Ident(name) }
    implicit def liftExpr[T <: Expr[_]]: Liftable[T]           = Liftable { expr => expr.tree }
    implicit def liftType[T <: Type]: Liftable[T]              = Liftable { tpe => TypeTree(tpe) }
    implicit def liftTypeTag[T <: WeakTypeTag[_]]: Liftable[T] = Liftable { ttag => TypeTree(ttag.tpe) }
    implicit def liftConstant[T <: Constant]: Liftable[T]      = Liftable { const => Literal(const) }

    implicit def liftArray[T: Liftable]: Liftable[Array[T]]             = Liftable { arr => callScala(stdnme.Array)(arr.map(lift(_)).toList) }
    implicit def liftVector[T: Liftable]: Liftable[Vector[T]]           = Liftable { vect => callCollection(stdnme.Vector)(vect.map(lift(_)).toList) }
    implicit def liftList[T: Liftable]: Liftable[List[T]]               = Liftable { lst => callCollection(stdnme.List)(lst.map(lift(_))) }
    implicit def liftNil: Liftable[Nil.type]                            = Liftable { _ => selectScala(stdnme.collection, stdnme.immutable, stdnme.Nil) }
    implicit def liftMap[K: Liftable, V: Liftable]: Liftable[Map[K, V]] = Liftable { m => callCollection(stdnme.Map)(m.toList.map(lift(_))) }
    implicit def liftSet[T: Liftable]: Liftable[Set[T]]                 = Liftable { s => callCollection(stdnme.Set)(s.toList.map(lift(_))) }

    implicit def liftSome[T: Liftable]: Liftable[Some[T]]     = Liftable { case Some(v) => callScala(stdnme.Some)(lift(v) :: Nil) }
    implicit def liftNone: Liftable[None.type]                = Liftable { _ => selectScala(stdnme.None) }
    implicit def liftOption[T: Liftable]: Liftable[Option[T]] = Liftable {
      case some: Some[T]   => lift(some)
      case none: None.type => lift(none)
    }

    implicit def liftLeft[L: Liftable, R]: Liftable[Left[L, R]]               = Liftable { case Left(v)  => callScala(stdnme.util, stdnme.Left)(lift(v) :: Nil) }
    implicit def liftRight[L, R: Liftable]: Liftable[Right[L, R]]             = Liftable { case Right(v) => callScala(stdnme.util, stdnme.Right)(lift(v) :: Nil) }
    implicit def liftEither[L: Liftable, R: Liftable]: Liftable[Either[L, R]] = Liftable {
      case left: Left[L, R]   => lift(left)
      case right: Right[L, R] => lift(right)
    }

    implicit def liftTuple2[T1, T2](implicit liftT1: Liftable[T1], liftT2: Liftable[T2]): Liftable[Tuple2[T1, T2]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: Nil)
    }
    implicit def liftTuple3[T1, T2, T3](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3]): Liftable[Tuple3[T1, T2, T3]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: Nil)
    }
    implicit def liftTuple4[T1, T2, T3, T4](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4]): Liftable[Tuple4[T1, T2, T3, T4]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: Nil)
    }
    implicit def liftTuple5[T1, T2, T3, T4, T5](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5]): Liftable[Tuple5[T1, T2, T3, T4, T5]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: Nil)
    }
    implicit def liftTuple6[T1, T2, T3, T4, T5, T6](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6]): Liftable[Tuple6[T1, T2, T3, T4, T5, T6]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: Nil)
    }
    implicit def liftTuple7[T1, T2, T3, T4, T5, T6, T7](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7]): Liftable[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: Nil)
    }
    implicit def liftTuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8]): Liftable[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: Nil)
    }
    implicit def liftTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9]): Liftable[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: Nil)
    }
    implicit def liftTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10]): Liftable[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: Nil)
    }
    implicit def liftTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11]): Liftable[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: Nil)
    }
    implicit def liftTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12]): Liftable[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: Nil)
    }
    implicit def liftTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13]): Liftable[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: Nil)
    }
    implicit def liftTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13], liftT14: Liftable[T14]): Liftable[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: Nil)
    }
    implicit def liftTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13], liftT14: Liftable[T14], liftT15: Liftable[T15]): Liftable[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: Nil)
    }
    implicit def liftTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13], liftT14: Liftable[T14], liftT15: Liftable[T15], liftT16: Liftable[T16]): Liftable[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: Nil)
    }
    implicit def liftTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13], liftT14: Liftable[T14], liftT15: Liftable[T15], liftT16: Liftable[T16], liftT17: Liftable[T17]): Liftable[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: liftT17(t._17) :: Nil)
    }
    implicit def liftTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13], liftT14: Liftable[T14], liftT15: Liftable[T15], liftT16: Liftable[T16], liftT17: Liftable[T17], liftT18: Liftable[T18]): Liftable[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: liftT17(t._17) :: liftT18(t._18) :: Nil)
    }
    implicit def liftTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13], liftT14: Liftable[T14], liftT15: Liftable[T15], liftT16: Liftable[T16], liftT17: Liftable[T17], liftT18: Liftable[T18], liftT19: Liftable[T19]): Liftable[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: liftT17(t._17) :: liftT18(t._18) :: liftT19(t._19) :: Nil)
    }
    implicit def liftTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13], liftT14: Liftable[T14], liftT15: Liftable[T15], liftT16: Liftable[T16], liftT17: Liftable[T17], liftT18: Liftable[T18], liftT19: Liftable[T19], liftT20: Liftable[T20]): Liftable[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: liftT17(t._17) :: liftT18(t._18) :: liftT19(t._19) :: liftT20(t._20) :: Nil)
    }
    implicit def liftTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13], liftT14: Liftable[T14], liftT15: Liftable[T15], liftT16: Liftable[T16], liftT17: Liftable[T17], liftT18: Liftable[T18], liftT19: Liftable[T19], liftT20: Liftable[T20], liftT21: Liftable[T21]): Liftable[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: liftT17(t._17) :: liftT18(t._18) :: liftT19(t._19) :: liftT20(t._20) :: liftT21(t._21) :: Nil)
    }
    implicit def liftTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](implicit liftT1: Liftable[T1], liftT2: Liftable[T2], liftT3: Liftable[T3], liftT4: Liftable[T4], liftT5: Liftable[T5], liftT6: Liftable[T6], liftT7: Liftable[T7], liftT8: Liftable[T8], liftT9: Liftable[T9], liftT10: Liftable[T10], liftT11: Liftable[T11], liftT12: Liftable[T12], liftT13: Liftable[T13], liftT14: Liftable[T14], liftT15: Liftable[T15], liftT16: Liftable[T16], liftT17: Liftable[T17], liftT18: Liftable[T18], liftT19: Liftable[T19], liftT20: Liftable[T20], liftT21: Liftable[T21], liftT22: Liftable[T22]): Liftable[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: liftT2(t._2) :: liftT3(t._3) :: liftT4(t._4) :: liftT5(t._5) :: liftT6(t._6) :: liftT7(t._7) :: liftT8(t._8) :: liftT9(t._9) :: liftT10(t._10) :: liftT11(t._11) :: liftT12(t._12) :: liftT13(t._13) :: liftT14(t._14) :: liftT15(t._15) :: liftT16(t._16) :: liftT17(t._17) :: liftT18(t._18) :: liftT19(t._19) :: liftT20(t._20) :: liftT21(t._21) :: liftT22(t._22) :: Nil)
    }
  }

  trait StandardUnliftableInstances {
    private def unliftPrimitive[Unboxed: ClassTag, Boxed: ClassTag] = Unliftable[Unboxed] {
      case Literal(Constant(value))
           if value.getClass == implicitly[ClassTag[Boxed]].runtimeClass
           || value.getClass == implicitly[ClassTag[Unboxed]].runtimeClass =>
        value.asInstanceOf[Unboxed]
    }
    implicit def unliftByte: Unliftable[Byte]       = unliftPrimitive[Byte, java.lang.Byte]
    implicit def unliftShort: Unliftable[Short]     = unliftPrimitive[Short, java.lang.Short]
    implicit def unliftChar: Unliftable[Char]       = unliftPrimitive[Char, java.lang.Character]
    implicit def unliftInt: Unliftable[Int]         = unliftPrimitive[Int, java.lang.Integer]
    implicit def unliftLong: Unliftable[Long]       = unliftPrimitive[Long, java.lang.Long]
    implicit def unliftFloat: Unliftable[Float]     = unliftPrimitive[Float, java.lang.Float]
    implicit def unliftDouble: Unliftable[Double]   = unliftPrimitive[Double, java.lang.Double]
    implicit def unliftBoolean: Unliftable[Boolean] = unliftPrimitive[Boolean, java.lang.Boolean]
    implicit def unliftUnit: Unliftable[Unit]       = unliftPrimitive[Unit, scala.runtime.BoxedUnit]
    implicit def unliftString: Unliftable[String]   = Unliftable { case Literal(Constant(s: String)) => s }

    implicit def unliftScalaSymbol: Unliftable[scala.Symbol] = Unliftable {
      case Apply(ScalaDot(stdnme.Symbol), List(Literal(Constant(name: String)))) => scala.Symbol(name)
    }

    implicit def unliftName[T <: Name : ClassTag]: Unliftable[T] = Unliftable[T] {
      case Ident(name: T)                        => name
      case Bind(name: T, Ident(stdnme.WILDCARD)) => name
    }
    implicit def unliftType: Unliftable[Type]                    = Unliftable[Type] { case tt: TypeTree if tt.tpe != null => tt.tpe }
    implicit def unliftConstant: Unliftable[Constant]            = Unliftable[Constant] { case Literal(const) => const }

    // GENERATED CODE:
    // scala> def m(n: Int) = s"    implicit def unliftTuple$n[${(1 to n).map("T" + _).mkString(", ")}](implicit ${(1 to n).map(i => "UnliftT" + i + ": Unliftable[T" + i + "]").mkString(", ")}): Unliftable[(${(1 to n).map("T" + _).mkString(", ")})] =\n      new TupleUnlifter[(${(1 to n).map("T" + _).mkString(", ")})](Array(${(1 to n).map("UnliftT" + _).mkString(", ")})) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple$n(${(1 to n).map(i => "rs(" + (i - 1) + ")").mkString(", ")})}"; println((2 to 22).map(m).mkString("\n\n"))
    implicit def unliftTuple2[T1, T2](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2]): Unliftable[(T1, T2)] =
      new TupleUnlifter[(T1, T2)](Array(UnliftT1, UnliftT2)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple2(rs(0), rs(1))}

    implicit def unliftTuple3[T1, T2, T3](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3]): Unliftable[(T1, T2, T3)] =
      new TupleUnlifter[(T1, T2, T3)](Array(UnliftT1, UnliftT2, UnliftT3)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple3(rs(0), rs(1), rs(2))}

    implicit def unliftTuple4[T1, T2, T3, T4](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4]): Unliftable[(T1, T2, T3, T4)] =
      new TupleUnlifter[(T1, T2, T3, T4)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple4(rs(0), rs(1), rs(2), rs(3))}

    implicit def unliftTuple5[T1, T2, T3, T4, T5](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5]): Unliftable[(T1, T2, T3, T4, T5)] =
      new TupleUnlifter[(T1, T2, T3, T4, T5)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4, UnliftT5)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple5(rs(0), rs(1), rs(2), rs(3), rs(4))}

    implicit def unliftTuple6[T1, T2, T3, T4, T5, T6](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6]): Unliftable[(T1, T2, T3, T4, T5, T6)] =
      new TupleUnlifter[(T1, T2, T3, T4, T5, T6)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4, UnliftT5, UnliftT6)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple6(rs(0), rs(1), rs(2), rs(3), rs(4), rs(5))}

    implicit def unliftTuple7[T1, T2, T3, T4, T5, T6, T7](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7]): Unliftable[(T1, T2, T3, T4, T5, T6, T7)] =
      new TupleUnlifter[(T1, T2, T3, T4, T5, T6, T7)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4, UnliftT5, UnliftT6, UnliftT7)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple7(rs(0), rs(1), rs(2), rs(3), rs(4), rs(5), rs(6))}

    implicit def unliftTuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8]): Unliftable[(T1, T2, T3, T4, T5, T6, T7, T8)] =
      new TupleUnlifter[(T1, T2, T3, T4, T5, T6, T7, T8)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4, UnliftT5, UnliftT6, UnliftT7, UnliftT8)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple8(rs(0), rs(1), rs(2), rs(3), rs(4), rs(5), rs(6), rs(7))}

    implicit def unliftTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9]): Unliftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
      new TupleUnlifter[(T1, T2, T3, T4, T5, T6, T7, T8, T9)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4, UnliftT5, UnliftT6, UnliftT7, UnliftT8, UnliftT9)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple9(rs(0), rs(1), rs(2), rs(3), rs(4), rs(5), rs(6), rs(7), rs(8))}

    implicit def unliftTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10]): Unliftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] =
      new TupleUnlifter[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4, UnliftT5, UnliftT6, UnliftT7, UnliftT8, UnliftT9, UnliftT10)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple10(rs(0), rs(1), rs(2), rs(3), rs(4), rs(5), rs(6), rs(7), rs(8), rs(9))}

    implicit def unliftTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11]): Unliftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] =
      new TupleUnlifter[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4, UnliftT5, UnliftT6, UnliftT7, UnliftT8, UnliftT9, UnliftT10, UnliftT11)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple11(rs(0), rs(1), rs(2), rs(3), rs(4), rs(5), rs(6), rs(7), rs(8), rs(9), rs(10))}

    implicit def unliftTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12]): Unliftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] =
      new TupleUnlifter[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4, UnliftT5, UnliftT6, UnliftT7, UnliftT8, UnliftT9, UnliftT10, UnliftT11, UnliftT12)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple12(rs(0), rs(1), rs(2), rs(3), rs(4), rs(5), rs(6), rs(7), rs(8), rs(9), rs(10), rs(11))}

    implicit def unliftTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13]): Unliftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] =
      new TupleUnlifter[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4, UnliftT5, UnliftT6, UnliftT7, UnliftT8, UnliftT9, UnliftT10, UnliftT11, UnliftT12, UnliftT13)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple13(rs(0), rs(1), rs(2), rs(3), rs(4), rs(5), rs(6), rs(7), rs(8), rs(9), rs(10), rs(11), rs(12))}

    implicit def unliftTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13], UnliftT14: Unliftable[T14]): Unliftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] =
      new TupleUnlifter[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4, UnliftT5, UnliftT6, UnliftT7, UnliftT8, UnliftT9, UnliftT10, UnliftT11, UnliftT12, UnliftT13, UnliftT14)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple14(rs(0), rs(1), rs(2), rs(3), rs(4), rs(5), rs(6), rs(7), rs(8), rs(9), rs(10), rs(11), rs(12), rs(13))}

    implicit def unliftTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13], UnliftT14: Unliftable[T14], UnliftT15: Unliftable[T15]): Unliftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] =
      new TupleUnlifter[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4, UnliftT5, UnliftT6, UnliftT7, UnliftT8, UnliftT9, UnliftT10, UnliftT11, UnliftT12, UnliftT13, UnliftT14, UnliftT15)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple15(rs(0), rs(1), rs(2), rs(3), rs(4), rs(5), rs(6), rs(7), rs(8), rs(9), rs(10), rs(11), rs(12), rs(13), rs(14))}

    implicit def unliftTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13], UnliftT14: Unliftable[T14], UnliftT15: Unliftable[T15], UnliftT16: Unliftable[T16]): Unliftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] =
      new TupleUnlifter[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4, UnliftT5, UnliftT6, UnliftT7, UnliftT8, UnliftT9, UnliftT10, UnliftT11, UnliftT12, UnliftT13, UnliftT14, UnliftT15, UnliftT16)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple16(rs(0), rs(1), rs(2), rs(3), rs(4), rs(5), rs(6), rs(7), rs(8), rs(9), rs(10), rs(11), rs(12), rs(13), rs(14), rs(15))}

    implicit def unliftTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13], UnliftT14: Unliftable[T14], UnliftT15: Unliftable[T15], UnliftT16: Unliftable[T16], UnliftT17: Unliftable[T17]): Unliftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] =
      new TupleUnlifter[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4, UnliftT5, UnliftT6, UnliftT7, UnliftT8, UnliftT9, UnliftT10, UnliftT11, UnliftT12, UnliftT13, UnliftT14, UnliftT15, UnliftT16, UnliftT17)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple17(rs(0), rs(1), rs(2), rs(3), rs(4), rs(5), rs(6), rs(7), rs(8), rs(9), rs(10), rs(11), rs(12), rs(13), rs(14), rs(15), rs(16))}

    implicit def unliftTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13], UnliftT14: Unliftable[T14], UnliftT15: Unliftable[T15], UnliftT16: Unliftable[T16], UnliftT17: Unliftable[T17], UnliftT18: Unliftable[T18]): Unliftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] =
      new TupleUnlifter[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4, UnliftT5, UnliftT6, UnliftT7, UnliftT8, UnliftT9, UnliftT10, UnliftT11, UnliftT12, UnliftT13, UnliftT14, UnliftT15, UnliftT16, UnliftT17, UnliftT18)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple18(rs(0), rs(1), rs(2), rs(3), rs(4), rs(5), rs(6), rs(7), rs(8), rs(9), rs(10), rs(11), rs(12), rs(13), rs(14), rs(15), rs(16), rs(17))}

    implicit def unliftTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13], UnliftT14: Unliftable[T14], UnliftT15: Unliftable[T15], UnliftT16: Unliftable[T16], UnliftT17: Unliftable[T17], UnliftT18: Unliftable[T18], UnliftT19: Unliftable[T19]): Unliftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] =
      new TupleUnlifter[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4, UnliftT5, UnliftT6, UnliftT7, UnliftT8, UnliftT9, UnliftT10, UnliftT11, UnliftT12, UnliftT13, UnliftT14, UnliftT15, UnliftT16, UnliftT17, UnliftT18, UnliftT19)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple19(rs(0), rs(1), rs(2), rs(3), rs(4), rs(5), rs(6), rs(7), rs(8), rs(9), rs(10), rs(11), rs(12), rs(13), rs(14), rs(15), rs(16), rs(17), rs(18))}

    implicit def unliftTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13], UnliftT14: Unliftable[T14], UnliftT15: Unliftable[T15], UnliftT16: Unliftable[T16], UnliftT17: Unliftable[T17], UnliftT18: Unliftable[T18], UnliftT19: Unliftable[T19], UnliftT20: Unliftable[T20]): Unliftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] =
      new TupleUnlifter[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4, UnliftT5, UnliftT6, UnliftT7, UnliftT8, UnliftT9, UnliftT10, UnliftT11, UnliftT12, UnliftT13, UnliftT14, UnliftT15, UnliftT16, UnliftT17, UnliftT18, UnliftT19, UnliftT20)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple20(rs(0), rs(1), rs(2), rs(3), rs(4), rs(5), rs(6), rs(7), rs(8), rs(9), rs(10), rs(11), rs(12), rs(13), rs(14), rs(15), rs(16), rs(17), rs(18), rs(19))}

    implicit def unliftTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13], UnliftT14: Unliftable[T14], UnliftT15: Unliftable[T15], UnliftT16: Unliftable[T16], UnliftT17: Unliftable[T17], UnliftT18: Unliftable[T18], UnliftT19: Unliftable[T19], UnliftT20: Unliftable[T20], UnliftT21: Unliftable[T21]): Unliftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] =
      new TupleUnlifter[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4, UnliftT5, UnliftT6, UnliftT7, UnliftT8, UnliftT9, UnliftT10, UnliftT11, UnliftT12, UnliftT13, UnliftT14, UnliftT15, UnliftT16, UnliftT17, UnliftT18, UnliftT19, UnliftT20, UnliftT21)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple21(rs(0), rs(1), rs(2), rs(3), rs(4), rs(5), rs(6), rs(7), rs(8), rs(9), rs(10), rs(11), rs(12), rs(13), rs(14), rs(15), rs(16), rs(17), rs(18), rs(19), rs(20))}

    implicit def unliftTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](implicit UnliftT1: Unliftable[T1], UnliftT2: Unliftable[T2], UnliftT3: Unliftable[T3], UnliftT4: Unliftable[T4], UnliftT5: Unliftable[T5], UnliftT6: Unliftable[T6], UnliftT7: Unliftable[T7], UnliftT8: Unliftable[T8], UnliftT9: Unliftable[T9], UnliftT10: Unliftable[T10], UnliftT11: Unliftable[T11], UnliftT12: Unliftable[T12], UnliftT13: Unliftable[T13], UnliftT14: Unliftable[T14], UnliftT15: Unliftable[T15], UnliftT16: Unliftable[T16], UnliftT17: Unliftable[T17], UnliftT18: Unliftable[T18], UnliftT19: Unliftable[T19], UnliftT20: Unliftable[T20], UnliftT21: Unliftable[T21], UnliftT22: Unliftable[T22]): Unliftable[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)] =
      new TupleUnlifter[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)](Array(UnliftT1, UnliftT2, UnliftT3, UnliftT4, UnliftT5, UnliftT6, UnliftT7, UnliftT8, UnliftT9, UnliftT10, UnliftT11, UnliftT12, UnliftT13, UnliftT14, UnliftT15, UnliftT16, UnliftT17, UnliftT18, UnliftT19, UnliftT20, UnliftT21, UnliftT22)) { override protected def buildTuple(rs: Array[AnyRef]) = Tuple22(rs(0), rs(1), rs(2), rs(3), rs(4), rs(5), rs(6), rs(7), rs(8), rs(9), rs(10), rs(11), rs(12), rs(13), rs(14), rs(15), rs(16), rs(17), rs(18), rs(19), rs(20), rs(21))}
    // END GENERATED CODE

    private abstract class TupleUnlifter[T](unlifts: Array[Unliftable[_]]) extends Unliftable[T] {
      override def unapply(tree: Tree): Option[T] = SyntacticTuple.unapply(tree) match {
        case Some(values) =>
          val arity = unlifts.length
          val iterator = values.iterator
          val rs = new Array[AnyRef](arity)
          var i = 0
          while (i < arity) {
            if (!iterator.hasNext) return None
            unlifts(i).unapply(iterator.next()) match {
              case Some(x) =>
                rs(i) = x.asInstanceOf[AnyRef]
              case None => return None
            }
            i += 1
          }
          if (iterator.hasNext) None
          else Some(buildTuple(rs).asInstanceOf[T])
        case None => None
      }
      protected def buildTuple(rs: Array[AnyRef]): AnyRef
    }
  }

  // names used internally by implementations of standard liftables and unliftables
  // can't be `private object nme` because of https://groups.google.com/forum/#!topic/scala-internals/b-Full9WZeE
  // can't be `private[this] object nme` because then STARR has problems prioritizing this.nme over self.nme
  // therefore I'm essentially forced to give this object a non-standard name
  private object stdnme {
    val Array      = TermName("Array")
    val collection = TermName("collection")
    val immutable  = TermName("immutable")
    val Left       = TermName("Left")
    val List       = TermName("List")
    val Map        = TermName("Map")
    val None       = TermName("None")
    val Nil        = TermName("Nil")
    val Right      = TermName("Right")
    val Set        = TermName("Set")
    val Some       = TermName("Some")
    val Symbol     = TermName("Symbol")
    val util       = TermName("util")
    val Vector     = TermName("Vector")
    val WILDCARD   = self.termNames.WILDCARD
  }
}
