package scala {

  module Predef {

    val True = Boolean.True;
    val False = Boolean.False;

    val List = scala.List;

    def List[a](x: a*): List[a] = {
      def mkList(elems: Iterator[a]): List[a] =
        if (elems.hasNext) elems.next :: mkList(elems)
	else Nil();
      mkList(x.elements);
    }

    def error[a](x: String):a = (new java.lang.RuntimeException(x)).throw;

    def ConsStream[a](hd: a, def tl: Stream[a]): Stream[a] =
      new ConsStreamClass(hd, () => tl);

    def range(lo: Int, hi: Int): List[Int] =
      if (lo > hi) List() else lo :: range(lo + 1, hi);

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

    type Pair[a, b] = Tuple2[a, b];
    def Pair[a, b](x: a, y: b) = Tuple2(x, y);

    type Triple[a, b, c] = Tuple3[a, b, c];
    def Triple[a, b, c](x: a, y: b, z: c) = Tuple3(x, y, z);
  }
}


