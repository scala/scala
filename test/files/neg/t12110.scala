
object Main extends App {

  class Unapplyer {
    def unapplySeq(seq: Seq[Int]): Option[(Int, Seq[Int])] =
      if (seq.isEmpty) None else Some((seq.head, seq.tail))
  }
  val unapplyer = new Unapplyer

  /*
     v2.12.12:
       prints "1"
     v2.13.3:
       type mismatch;
        found   : Unit
        required: Int
           case value unapplyer () => println(value)
   */
  Seq(1) match {
    case value unapplyer () => println(value)
  }
}
/*
scala 3:
-- Error: test/files/neg/t12110.scala:20:25 ----------------------------------------------------------------------------------------------
20 |    case value unapplyer () => println(value)
   |                         ^^
   |                         Values of types Unit and Int cannot be compared with == or !=
1 error found
*/
