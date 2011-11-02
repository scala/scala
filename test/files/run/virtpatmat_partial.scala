object Test extends App {
  val a = Map("a" -> Some(1), "b" -> None)
  println(a)

  val res = a collect {case (p, Some(a)) => (p, a)}

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
