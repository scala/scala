//############################################################################
// Programmation IV - 2002 - Week 05
//############################################################################

object M0 {
  def partition[a](xs: List[a], pred: a => Boolean): Tuple2[List[a], List[a]] = {
    if (xs.isEmpty)
      (List(),List())
    else {
      val tailPartition = partition(xs.tail, pred);
      if (pred(xs.head))
        (xs.head :: tailPartition._1, tailPartition._2)
      else
        (tailPartition._1, xs.head :: tailPartition._2)
    }
  }

  def quicksort[a] (less : (a,a) => Boolean) (xs : List[a]) : List[a] = {
    if (xs.isEmpty)
      xs
    else {
      val pivot = xs.head;
      val sub = partition(xs.tail, { elem : a => less(elem, pivot) });
      quicksort(less)(sub._1) ::: List(pivot) ::: quicksort(less)(sub._2)
    }
  }

  def test = {
    Console.println(partition[Int](List(1,2,3,4,5,6,7,8), (x => x < 0)));
    Console.println(partition[Int](List(1,2,3,4,5,6,7,8), (x => x < 5)));
    Console.println(partition[Int](List(1,2,3,4,5,6,7,8), (x => x < 9)));
    Console.println;

    Console.println(partition[Int](List(8,7,6,5,4,3,2,1), (x => x < 0)));
    Console.println(partition[Int](List(8,7,6,5,4,3,2,1), (x => x < 5)));
    Console.println(partition[Int](List(8,7,6,5,4,3,2,1), (x => x < 9)));
    Console.println;

    Console.println(partition[Int](List(7,2,1,5,4,3,8,6), (x => x < 0)));
    Console.println(partition[Int](List(7,2,1,5,4,3,8,6), (x => x < 5)));
    Console.println(partition[Int](List(7,2,1,5,4,3,8,6), (x => x < 9)));
    Console.println;

    Console.println(quicksort[Int]((x,y) => x < y)(List(7,2,1,5,4,3,8,6)));
    Console.println;
  }
}

//############################################################################

object M1 {
  def partition[a](xs: List[a], pred: a => Boolean): Tuple2[List[a], List[a]] = {
    xs.foldRight[Tuple2[List[a], List[a]]]((List(), List())) {
      (x, p) => if (pred (x)) (x :: p._1, p._2) else (p._1, x :: p._2)
    }
  }

  def quicksort[a] (less : (a,a) => Boolean) (xs : List[a]) : List[a] = {
    if (xs.isEmpty)
      xs
    else {
      val pivot = xs.head;
      val sub = partition(xs.tail, (elem : a) => less(elem, pivot));
      quicksort(less)(sub._1) ::: List(pivot) ::: quicksort(less)(sub._2)
    }
  }

  def test = {
    Console.println(partition[Int](List(1,2,3,4,5,6,7,8), (x => x < 0)));
    Console.println(partition[Int](List(1,2,3,4,5,6,7,8), (x => x < 5)));
    Console.println(partition[Int](List(1,2,3,4,5,6,7,8), (x => x < 9)));
    Console.println;

    Console.println(partition[Int](List(8,7,6,5,4,3,2,1), (x => x < 0)));
    Console.println(partition[Int](List(8,7,6,5,4,3,2,1), (x => x < 5)));
    Console.println(partition[Int](List(8,7,6,5,4,3,2,1), (x => x < 9)));
    Console.println;

    Console.println(partition[Int](List(7,2,1,5,4,3,8,6), (x => x < 0)));
    Console.println(partition[Int](List(7,2,1,5,4,3,8,6), (x => x < 5)));
    Console.println(partition[Int](List(7,2,1,5,4,3,8,6), (x => x < 9)));
    Console.println;

    Console.println(quicksort[Int]((x,y) => x < y)(List(7,2,1,5,4,3,8,6)));
    Console.println;
  }
}

//############################################################################

object M2 {

  def powerset[a] (s: List[a]): List[List[a]] = {
    if (s.isEmpty)
      List(List())
    else {
      val x = s.head;
      val withoutX = powerset(s.tail);
      withoutX ::: withoutX.map { s1 : List[a] => x::s1 }
    }
  }

  def test = {
    Console.println(powerset(List()));
    Console.println(powerset(List(1)));
    Console.println(powerset(List(1,2)));
    Console.println(powerset(List(1,2,3)));
    Console.println(powerset(List(1,2,3,4)));
    Console.println;
  }
}

//############################################################################

object M3 {

  def abs(x: Int) = if (x < 0) 0 - x else x;

  def range(lo: Int, hi: Int): List[Int] =
    if (lo > hi) List()
    else lo :: range(lo + 1, hi);

  type Placement = List[(Int, Int)];

  def queens(n: Int): List[Placement] = {
    def placeQueens(row: Int): List[Placement] = {
      if (row == 0)
        List(List())
      else {
        def isSafe(column: Int, placement: Placement): Boolean =
          placement forall {
            pos => (pos._2 != column &&
              abs(pos._2 - column) != row - pos._1)
          }

        def adjoinRow(placement: Placement): List[Placement] =
          range(1, n)
            .filter (column => isSafe(column, placement))
            .map (column => (row, column) :: placement);

        placeQueens(row - 1) flatMap adjoinRow
      }
    }
    placeQueens(n)
  }

  def test {
    Console.println("queens(1) = " + queens(1));
    Console.println("queens(2) = " + queens(2));
    Console.println("queens(3) = " + queens(3));
    Console.println("queens(4) = " + queens(4));
    Console.println;
  }
}

//############################################################################

object M4 {

  def abs(x: Int) = if (x < 0) 0 - x else x;

  def range(lo: Int, hi: Int): List[Int] =
    if (lo > hi) List()
    else lo :: range(lo + 1, hi);

  type Placement = List[Int];

  def queens(n: Int): List[Placement] = {
    val columns = range(1, n);
    def placeQueens(row: Int): List[Placement] = {
      if (row == 0)
        List(List())
      else {
        def isSafe(col: Int, p: Placement, delta: Int): Boolean =
          (p.isEmpty ||
           (col != p.head &&
            abs(col - p.head) != delta &&
            isSafe(col, p.tail, delta + 1)));

        for (
          placement <- placeQueens(row - 1);
          col <- columns;
          if isSafe(col, placement, 1)
        ) yield {
          col :: placement
        }
      }
    }
    placeQueens(n);
  }

  def test {
    Console.println("queens(1) = " + queens(1));
    Console.println("queens(2) = " + queens(2));
    Console.println("queens(3) = " + queens(3));
    Console.println("queens(4) = " + queens(4));
    Console.println;
  }
}

//############################################################################

object Test {
  def main(args: Array[String]) {
    M0.test;
    M1.test;
    M2.test;
    M3.test;
    M4.test;
    ()
  }
}

//############################################################################
