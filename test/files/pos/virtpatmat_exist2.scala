class ParseResult[+T]
case class MemoEntry[+T](var r: Either[Nothing,ParseResult[_]])

object Test {
  def grow[T]: ParseResult[T] = (null: MemoEntry[T]) match {
    case MemoEntry(Right(x: ParseResult[_])) => x.asInstanceOf[ParseResult[T]]
  }

  // what's the _$1 doing there?
  // def grow[T >: Nothing <: Any]: ParseResult[T] = {
  //   import OptionMatching._
  //   runOrElse[MemoEntry[T], ParseResult[T]]((null: MemoEntry[T]))(((x1: MemoEntry[T]) =>
  //     (MemoEntry.unapply[T](x1).flatMap[ParseResult[T]](((x4: Either[Nothing,ParseResult[_]]) =>
  //       guard[Right[Nothing,ParseResult[_]]](x4.isInstanceOf[Right[Nothing,ParseResult[_]]], x4.asInstanceOf[Right[Nothing,ParseResult[_]]]).flatMap[ParseResult[T]](((cp3: Right[Nothing,ParseResult[_]]) =>
  //         scala.Right.unapply[Nothing, ParseResult[_]](cp3).flatMap[ParseResult[T]](((x5: ParseResult[_]) =>
  //           guard[ParseResult[_$1]](x5.ne(null), x5.asInstanceOf[ParseResult[_]]).flatMap[ParseResult[T]](((x6: ParseResult[_]) =>
  //             one[ParseResult[T]](x6.asInstanceOf[ParseResult[T]]))))))))): Option[ParseResult[T]]
  //     ).orElse[ParseResult[T]]((zero: Option[ParseResult[T]]))))
  // }
}