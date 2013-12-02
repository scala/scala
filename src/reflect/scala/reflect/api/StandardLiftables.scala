package scala.reflect
package api

trait StandardLiftables { self: Universe =>
  import build.{SyntacticTuple, ScalaDot}

  trait Liftable[T] {
    def apply(value: T): Tree
  }

  object Liftable {
    def apply[T](f: T => Tree): Liftable[T] =
      new Liftable[T] { def apply(value: T): Tree = f(value) }

    private def lift[T: Liftable](value: T): Tree            = implicitly[Liftable[T]].apply(value)
    private def selectScala(names: Name*)                    = names.tail.foldLeft(ScalaDot(names.head)) { Select(_, _) }
    private def callScala(names: Name*)(args: List[Tree])    = Apply(selectScala(names: _*), args)
    private def callCollection(name: Name)(args: List[Tree]) = callScala(nme.collection, nme.immutable, name)(args)
    private def liftAsLiteral[T]: Liftable[T]                = Liftable { v => Literal(Constant(v)) }

    implicit def liftByte[T <: Byte]: Liftable[T]     = liftAsLiteral[T]
    implicit def liftShort[T <: Short]: Liftable[T]   = liftAsLiteral[T]
    implicit def liftChar[T <: Char]: Liftable[T]     = liftAsLiteral[T]
    implicit def liftInt[T <: Int]: Liftable[T]       = liftAsLiteral[T]
    implicit def liftLong[T <: Long]: Liftable[T]     = liftAsLiteral[T]
    implicit def liftFloat[T <: Float]: Liftable[T]   = liftAsLiteral[T]
    implicit def liftDouble[T <: Double]: Liftable[T] = liftAsLiteral[T]
    implicit def liftBoolean: Liftable[Boolean]       = liftAsLiteral[Boolean]
    implicit def liftUnit: Liftable[Unit]             = liftAsLiteral[Unit]
    implicit def liftString: Liftable[String]         = liftAsLiteral[String]

    implicit def liftScalaSymbol: Liftable[scala.Symbol] = Liftable { v =>
      callScala(nme.Symbol)(Literal(Constant(v.name)) :: Nil)
    }

    implicit def liftName[T <: Name]: Liftable[T]              = Liftable { name => Ident(name) }
    implicit def liftExpr[T <: Expr[_]]: Liftable[T]           = Liftable { expr => expr.tree }
    implicit def liftType[T <: Type]: Liftable[T]              = Liftable { tpe => TypeTree(tpe) }
    implicit def liftTypeTag[T <: WeakTypeTag[_]]: Liftable[T] = Liftable { ttag => TypeTree(ttag.tpe) }
    implicit def liftConstant[T <: Constant]: Liftable[T]      = Liftable { const => Literal(const) }

    implicit def liftArray[T: Liftable]: Liftable[Array[T]]             = Liftable { arr => callScala(nme.Array)(arr.map(lift(_)).toList) }
    implicit def liftVector[T: Liftable]: Liftable[Vector[T]]           = Liftable { vect => callCollection(nme.Vector)(vect.map(lift(_)).toList) }
    implicit def liftList[T: Liftable]: Liftable[List[T]]               = Liftable { lst => callCollection(nme.List)(lst.map(lift(_))) }
    implicit def liftMap[K: Liftable, V: Liftable]: Liftable[Map[K, V]] = Liftable { m => callCollection(nme.Map)(m.toList.map(lift(_))) }
    implicit def liftSet[T: Liftable]: Liftable[Set[T]]                 = Liftable { s => callCollection(nme.Set)(s.toList.map(lift(_))) }

    implicit def liftOption[T: Liftable]: Liftable[Option[T]] = Liftable {
      case Some(v) => callScala(nme.Some)(lift(v) :: Nil)
      case None    => selectScala(nme.None)
    }
    implicit def liftEither[L: Liftable, R: Liftable]: Liftable[Either[L, R]] = Liftable {
      case Left(l)  => callScala(nme.util, nme.Left)(lift(l) :: Nil)
      case Right(r) => callScala(nme.util, nme.Right)(lift(r) :: Nil)
    }

    implicit def liftTuple1[T1](implicit liftT1: Liftable[T1]): Liftable[Tuple1[T1]] = Liftable { t =>
      SyntacticTuple(liftT1(t._1) :: Nil)
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
}
