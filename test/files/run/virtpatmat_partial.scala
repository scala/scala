object Test extends App {
  val a = Map("a" -> Some(1), "b" -> None)
  println(a)

  val res = a collect {case (p, Some(a)) => (p, a)}

  final val GT = 79
  final val GTGT = 93
  final val GTGTGT = 94
  final val GTEQ = 81
  final val GTGTEQ = 113
  final val GTGTGTEQ = 114
  final val ASSIGN = 75

  def acceptClosingAngle(in: Int) {
    val closers: PartialFunction[Int, Int] = {
      case GTGTGTEQ => GTGTEQ
      case GTGTGT   => GTGT
      case GTGTEQ   => GTEQ
      case GTGT     => GT
      case GTEQ     => ASSIGN
    }
    if (closers isDefinedAt in) println(closers(in))
    else println("undefined")
  }

  acceptClosingAngle(GTGT)
  acceptClosingAngle(ASSIGN)

  // should uncurry to:
  // val res: Map[String,Int] = a.collect[(String, Int), Map[String,Int]](
  //   new PartialFunction[(String, Option[Int]),(String, Int)]  {
  //     def apply(x0_1: (String, Option[Int])): (String, Int) = MatchingStrategy.OptionMatchingStrategy.runOrElse[(String, Option[Int]), (String, Int)](x0_1)(
  //       (x1: (String, Option[Int])) => {
  //           val o9: Option[(String, Int)] = ({
  //             val o8: Option[(String, Option[Int])] = Tuple2.unapply[String, Option[Int]](x1);
  //             if (o8.isEmpty)
  //               MatchingStrategy.OptionMatchingStrategy.zero
  //             else
  //               {
  //                 val o7: Option[Some[Int]] = if (o8.get._2.isInstanceOf[Some[Int]])
  //                   MatchingStrategy.OptionMatchingStrategy.one[Some[Int]](o8.get._2.asInstanceOf[Some[Int]])
  //                 else
  //                   MatchingStrategy.OptionMatchingStrategy.zero;
  //                 if (o7.isEmpty)
  //                   MatchingStrategy.OptionMatchingStrategy.zero
  //                 else
  //                   {
  //                     val o6: Option[Int] = Some.unapply[Int](o7.get);
  //                     if (o6.isEmpty)
  //                       MatchingStrategy.OptionMatchingStrategy.zero
  //                     else
  //                       MatchingStrategy.OptionMatchingStrategy.one[(String, Int)]((o8.get._1, o6.get).asInstanceOf[(String, Int)])
  //                   }
  //               }
  //           }: Option[(String, Int)]);
  //           if (o9.isEmpty)
  //             (MatchingStrategy.OptionMatchingStrategy.zero: Option[(String, Int)])
  //           else
  //             o9
  //         })
  //     
  //       def isDefinedAt(x_1: (String, Option[Int])): Boolean = MatchingStrategy.OptionMatchingStrategy.isSuccess[(String, Option[Int]), (String, Int)](x_1)(
  //         (x1: (String, Option[Int])) => {
  //           val o9: Option[(String, Int)] = ({
  //             val o8: Option[(String, Option[Int])] = scala.Tuple2.unapply[String, Option[Int]](x1);
  //             if (o8.isEmpty)
  //               MatchingStrategy.OptionMatchingStrategy.zero
  //             else
  //               {
  //                 val o7: Option[Some[Int]] = if (o8.get._2.isInstanceOf[Some[Int]])
  //                   MatchingStrategy.OptionMatchingStrategy.one[Some[Int]](o8.get._2.asInstanceOf[Some[Int]]) // XXX
  //                 else
  //                   MatchingStrategy.OptionMatchingStrategy.zero;
  //                 if (o7.isEmpty)
  //                   MatchingStrategy.OptionMatchingStrategy.zero
  //                 else
  //                   {
  //                     val o6: Option[Int] = scala.Some.unapply[Int](o7.get);
  //                     if (o6.isEmpty)
  //                       MatchingStrategy.OptionMatchingStrategy.zero
  //                     else
  //                       MatchingStrategy.OptionMatchingStrategy.one[(String, Int)](null.asInstanceOf[(String, Int)])
  //                   }
  //               }
  //           }: Option[(String, Int)]);
  //           if (o9.isEmpty)
  //             (MatchingStrategy.OptionMatchingStrategy.zero: Option[(String, Int)])
  //           else
  //             o9
  //     })
  //   }
  // )

  println(res)
}
