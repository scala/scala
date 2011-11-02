class IntList(val hd: Int, val tl: IntList)
object NilIL extends IntList(0, null)
object IntList {
  def unapply(il: IntList): Option[(Int, IntList)] = if(il eq NilIL) None else Some(il.hd, il.tl)
  def apply(x: Int, xs: IntList) = new IntList(x, xs)
}

object Test extends App {
  IntList(1, IntList(2, NilIL)) match { 
    case IntList(a1, IntList(a2, IntList(a3, y))) => println(a1 + a2 + a3)
    case IntList(x, y) => println(x)
  }

  IntList(1, IntList(2, IntList(3, NilIL))) match { 
    case IntList(a1, IntList(a2, IntList(a3, y))) => println(a1 + a2 + a3)
    case IntList(x, y) => println(x)
  }
}

// ((x1: IntList) => IntList.unapply(x1).flatMap(((x4: (Int, IntList)) => IntList.unapply(x4._2).flatMap(((x5: (Int, IntList)) => IntList.unapply(x5._2).flatMap(((x6: (Int, IntList)) => implicitly[Predef.MatchingStrategy[Option]].success(Predef.println(x4._1.+(x5._1).+(x6._1))))))))).orElse(IntList.unapply(x1).flatMap(((x7: (Int, IntList)) => implicitly[scala.Predef.MatchingStrategy[Option]].success(Predef.println(x7._1))))).orElse(implicitly[scala.Predef.MatchingStrategy[Option]].fail))(IntList.apply(1, IntList.apply(2, IntList.apply(3, null))))

/*
  ((x1: IntList) => 
    IntList.this.unapply(x1).flatMap[Int](((x4: (Int, IntList)) => 
      IntList.this.unapply(x4._2).flatMap[Int](((x5: (Int, IntList)) => 
        IntList.this.unapply(x5._2).flatMap[Int](((x6: (Int, IntList)) => 
          Predef.this.implicitly[scala.Predef.MatchingStrategy[Option]](scala.this.Predef.OptionMatching).success[Int](x6._1))))))).orElse[Int](
    IntList.this.unapply(x1).flatMap[Int](((x7: (Int, IntList)) => 
      Predef.this.implicitly[scala.Predef.MatchingStrategy[Option]](scala.this.Predef.OptionMatching).success[Int](x7._1)))).orElse[Int](
    Predef.this.implicitly[scala.Predef.MatchingStrategy[Option]](scala.this.Predef.OptionMatching).fail)
  ).apply(IntList.apply(1, null))
*/