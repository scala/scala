package scala {

  module Predef {

    val True = Boolean.True;
    val False = Boolean.False;

    def List[a](xs: Nil[a]): List[a] = [];
    def List[a](xs: [a]): List[a] = xs._1 :: [];
    def List[a](xs: [a, a]): List[a] = xs._1 :: xs._2 :: [];
    def List[a](xs: [a, a, a]): List[a] = xs._1 :: xs._2 :: xs._3 :: [];
    def List[a](xs: [a, a, a, a]): List[a] = xs._1 :: xs._2 :: xs._3 :: xs._4 :: [];
    def List[a](xs: [a, a, a, a, a]): List[a] = xs._1 :: xs._2 :: xs._3 :: xs._4 :: xs._5 :: [];
    def List[a](xs: [a, a, a, a, a, a]): List[a] = xs._1 :: xs._2 :: xs._3 :: xs._4 :: xs._5 :: xs._6 :: [];
    def List[a](xs: [a, a, a, a, a, a, a]): List[a] = xs._1 :: xs._2 :: xs._3 :: xs._4 :: xs._5 :: xs._6 :: xs._7 :: [];
    def List[a](xs: [a, a, a, a, a, a, a, a]): List[a] = xs._1 :: xs._2 :: xs._3 :: xs._4 :: xs._5 :: xs._6 :: xs._7 :: xs._8 :: [];
    def List[a](xs: [a, a, a, a, a, a, a, a, a]): List[a] = xs._1 :: xs._2 :: xs._3 :: xs._4 :: xs._5 :: xs._6 :: xs._7 :: xs._8 :: xs._9 :: [];

    def error[a](x: String):a = (new java.lang.RuntimeException(x)).throw;

    def ConsStream[a](hd: a, def tl: Stream[a]): Stream[a] =
      new ConsStreamClass(hd, () => tl);

    def range(lo: Int, hi: Int): List[Int] =
      if (lo > hi) [] else lo :: range(lo + 1, hi);

    def while(def condition: Boolean)(def command: Unit): Unit =
      if (condition) {
	command; while(condition)(command)
      } else {
      }

    trait Until {
      def until(def condition: Boolean): Unit
    }

    def repeat(def command: Unit): Until =
      new Until {
	def until(def condition: Boolean): Unit = {
	  command ;
	  if (condition) {}
	  else until(condition)
	}
      }
  }
}


