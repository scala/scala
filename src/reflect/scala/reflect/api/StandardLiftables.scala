package scala.reflect
package api

trait StandardLiftables { self: Universe =>

  trait Liftable[T] {
    def apply(value: T): Tree
  }

  object Liftable {
    def apply[T](f: T => Tree): Liftable[T] =
      new Liftable[T] { def apply(value: T): Tree = f(value) }
  }

  trait Unliftable[T] {
    def unapply(tree: Tree): Option[T]
  }

  object Unliftable {
    def apply[T](pf: PartialFunction[Tree, T]): Unliftable[T] = new Unliftable[T] {
      def unapply(value: Tree): Option[T] = pf.lift(value)
    }
  }

  // --------- Isomorphisms ---------

  implicit def isoByte: Liftable[Byte] with Unliftable[Byte]
  implicit def isoShort: Liftable[Short] with Unliftable[Short]
  implicit def isoChar: Liftable[Char] with Unliftable[Char]
  implicit def isoInt: Liftable[Int] with Unliftable[Int]
  implicit def isoLong: Liftable[Long] with Unliftable[Long]
  implicit def isoFloat: Liftable[Float] with Unliftable[Float]
  implicit def isoDouble: Liftable[Double] with Unliftable[Double]
  implicit def isoBoolean: Liftable[Boolean] with Unliftable[Boolean]
  implicit def isoUnit: Liftable[Unit] with Unliftable[Unit]
  implicit def isoString: Liftable[String] with Unliftable[String]
  implicit def isoScalaSymbol: Liftable[scala.Symbol] with Unliftable[scala.Symbol]

  // --------- Liftables ---------

  implicit def liftName[T <: Name]: Liftable[T]
  implicit def liftExpr[T <: Expr[_]]: Liftable[T]
  implicit def liftType[T <: Type]: Liftable[T]
  implicit def liftTypeTag[T <: WeakTypeTag[_]]: Liftable[T]
  implicit def liftConstant[T <: Constant]: Liftable[T]

  implicit def liftArray[T: Liftable]: Liftable[Array[T]]
  implicit def liftVector[T: Liftable]: Liftable[Vector[T]]
  implicit def liftList[T: Liftable]: Liftable[List[T]]
  implicit def liftMap[K: Liftable, V: Liftable]: Liftable[Map[K, V]]
  implicit def liftSet[T: Liftable]: Liftable[Set[T]]
  implicit def liftOption[T: Liftable]: Liftable[Option[T]]
  implicit def liftEither[L: Liftable, R: Liftable]: Liftable[Either[L, R]]

  implicit def liftTuple1[T1: Liftable]: Liftable[Tuple1[T1]]
  implicit def liftTuple2[T1: Liftable, T2: Liftable]: Liftable[Tuple2[T1, T2]]
  implicit def liftTuple3[T1: Liftable, T2: Liftable, T3: Liftable]: Liftable[Tuple3[T1, T2, T3]]
  implicit def liftTuple4[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable]: Liftable[Tuple4[T1, T2, T3, T4]]
  implicit def liftTuple5[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable]: Liftable[Tuple5[T1, T2, T3, T4, T5]]
  implicit def liftTuple6[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable]: Liftable[Tuple6[T1, T2, T3, T4, T5, T6]]
  implicit def liftTuple7[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable]: Liftable[Tuple7[T1, T2, T3, T4, T5, T6, T7]]
  implicit def liftTuple8[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable]: Liftable[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]
  implicit def liftTuple9[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable]: Liftable[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]
  implicit def liftTuple10[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable]: Liftable[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]
  implicit def liftTuple11[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable]: Liftable[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]
  implicit def liftTuple12[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable]: Liftable[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]
  implicit def liftTuple13[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable]: Liftable[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]
  implicit def liftTuple14[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable, T14: Liftable]: Liftable[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]
  implicit def liftTuple15[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable, T14: Liftable, T15: Liftable]: Liftable[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]
  implicit def liftTuple16[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable, T14: Liftable, T15: Liftable, T16: Liftable]: Liftable[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]
  implicit def liftTuple17[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable, T14: Liftable, T15: Liftable, T16: Liftable, T17: Liftable]: Liftable[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]
  implicit def liftTuple18[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable, T14: Liftable, T15: Liftable, T16: Liftable, T17: Liftable, T18: Liftable]: Liftable[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]
  implicit def liftTuple19[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable, T14: Liftable, T15: Liftable, T16: Liftable, T17: Liftable, T18: Liftable, T19: Liftable]: Liftable[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]
  implicit def liftTuple20[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable, T14: Liftable, T15: Liftable, T16: Liftable, T17: Liftable, T18: Liftable, T19: Liftable, T20: Liftable]: Liftable[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]
  implicit def liftTuple21[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable, T14: Liftable, T15: Liftable, T16: Liftable, T17: Liftable, T18: Liftable, T19: Liftable, T20: Liftable, T21: Liftable]: Liftable[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]
  implicit def liftTuple22[T1: Liftable, T2: Liftable, T3: Liftable, T4: Liftable, T5: Liftable, T6: Liftable, T7: Liftable, T8: Liftable, T9: Liftable, T10: Liftable, T11: Liftable, T12: Liftable, T13: Liftable, T14: Liftable, T15: Liftable, T16: Liftable, T17: Liftable, T18: Liftable, T19: Liftable, T20: Liftable, T21: Liftable, T22: Liftable]: Liftable[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]

  // --------- Unliftables ---------

  implicit def unliftName[T <: Name : ClassTag]: Unliftable[T]
  implicit def unliftType: Unliftable[Type]
  implicit def unliftConstant: Unliftable[Constant]

  implicit def unliftTuple1[T1: Unliftable]: Unliftable[Tuple1[T1]]
  implicit def unliftTuple2[T1: Unliftable, T2: Unliftable]: Unliftable[Tuple2[T1, T2]]
  implicit def unliftTuple3[T1: Unliftable, T2: Unliftable, T3: Unliftable]: Unliftable[Tuple3[T1, T2, T3]]
  implicit def unliftTuple4[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable]: Unliftable[Tuple4[T1, T2, T3, T4]]
  implicit def unliftTuple5[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable, T5: Unliftable]: Unliftable[Tuple5[T1, T2, T3, T4, T5]]
  implicit def unliftTuple6[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable, T5: Unliftable, T6: Unliftable]: Unliftable[Tuple6[T1, T2, T3, T4, T5, T6]]
  implicit def unliftTuple7[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable, T5: Unliftable, T6: Unliftable, T7: Unliftable]: Unliftable[Tuple7[T1, T2, T3, T4, T5, T6, T7]]
  implicit def unliftTuple8[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable, T5: Unliftable, T6: Unliftable, T7: Unliftable, T8: Unliftable]: Unliftable[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]
  implicit def unliftTuple9[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable, T5: Unliftable, T6: Unliftable, T7: Unliftable, T8: Unliftable, T9: Unliftable]: Unliftable[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]
  implicit def unliftTuple10[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable, T5: Unliftable, T6: Unliftable, T7: Unliftable, T8: Unliftable, T9: Unliftable, T10: Unliftable]: Unliftable[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]
  implicit def unliftTuple11[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable, T5: Unliftable, T6: Unliftable, T7: Unliftable, T8: Unliftable, T9: Unliftable, T10: Unliftable, T11: Unliftable]: Unliftable[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]
  implicit def unliftTuple12[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable, T5: Unliftable, T6: Unliftable, T7: Unliftable, T8: Unliftable, T9: Unliftable, T10: Unliftable, T11: Unliftable, T12: Unliftable]: Unliftable[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]
  implicit def unliftTuple13[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable, T5: Unliftable, T6: Unliftable, T7: Unliftable, T8: Unliftable, T9: Unliftable, T10: Unliftable, T11: Unliftable, T12: Unliftable, T13: Unliftable]: Unliftable[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]
  implicit def unliftTuple14[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable, T5: Unliftable, T6: Unliftable, T7: Unliftable, T8: Unliftable, T9: Unliftable, T10: Unliftable, T11: Unliftable, T12: Unliftable, T13: Unliftable, T14: Unliftable]: Unliftable[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]
  implicit def unliftTuple15[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable, T5: Unliftable, T6: Unliftable, T7: Unliftable, T8: Unliftable, T9: Unliftable, T10: Unliftable, T11: Unliftable, T12: Unliftable, T13: Unliftable, T14: Unliftable, T15: Unliftable]: Unliftable[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]
  implicit def unliftTuple16[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable, T5: Unliftable, T6: Unliftable, T7: Unliftable, T8: Unliftable, T9: Unliftable, T10: Unliftable, T11: Unliftable, T12: Unliftable, T13: Unliftable, T14: Unliftable, T15: Unliftable, T16: Unliftable]: Unliftable[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]
  implicit def unliftTuple17[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable, T5: Unliftable, T6: Unliftable, T7: Unliftable, T8: Unliftable, T9: Unliftable, T10: Unliftable, T11: Unliftable, T12: Unliftable, T13: Unliftable, T14: Unliftable, T15: Unliftable, T16: Unliftable, T17: Unliftable]: Unliftable[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]
  implicit def unliftTuple18[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable, T5: Unliftable, T6: Unliftable, T7: Unliftable, T8: Unliftable, T9: Unliftable, T10: Unliftable, T11: Unliftable, T12: Unliftable, T13: Unliftable, T14: Unliftable, T15: Unliftable, T16: Unliftable, T17: Unliftable, T18: Unliftable]: Unliftable[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]
  implicit def unliftTuple19[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable, T5: Unliftable, T6: Unliftable, T7: Unliftable, T8: Unliftable, T9: Unliftable, T10: Unliftable, T11: Unliftable, T12: Unliftable, T13: Unliftable, T14: Unliftable, T15: Unliftable, T16: Unliftable, T17: Unliftable, T18: Unliftable, T19: Unliftable]: Unliftable[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]
  implicit def unliftTuple20[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable, T5: Unliftable, T6: Unliftable, T7: Unliftable, T8: Unliftable, T9: Unliftable, T10: Unliftable, T11: Unliftable, T12: Unliftable, T13: Unliftable, T14: Unliftable, T15: Unliftable, T16: Unliftable, T17: Unliftable, T18: Unliftable, T19: Unliftable, T20: Unliftable]: Unliftable[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]
  implicit def unliftTuple21[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable, T5: Unliftable, T6: Unliftable, T7: Unliftable, T8: Unliftable, T9: Unliftable, T10: Unliftable, T11: Unliftable, T12: Unliftable, T13: Unliftable, T14: Unliftable, T15: Unliftable, T16: Unliftable, T17: Unliftable, T18: Unliftable, T19: Unliftable, T20: Unliftable, T21: Unliftable]: Unliftable[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]
  implicit def unliftTuple22[T1: Unliftable, T2: Unliftable, T3: Unliftable, T4: Unliftable, T5: Unliftable, T6: Unliftable, T7: Unliftable, T8: Unliftable, T9: Unliftable, T10: Unliftable, T11: Unliftable, T12: Unliftable, T13: Unliftable, T14: Unliftable, T15: Unliftable, T16: Unliftable, T17: Unliftable, T18: Unliftable, T19: Unliftable, T20: Unliftable, T21: Unliftable, T22: Unliftable]: Unliftable[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]
}
