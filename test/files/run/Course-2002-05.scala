//############################################################################
// Programmation IV - 2002 - Week 05
//############################################################################
// $Id$

module M0 {
  def partition[a](xs: List[a], pred: a => boolean): Pair[List[a], List[a]] = {
    if (xs.isEmpty)
      Pair(List(),List())
    else {
      val tailPartition = partition(xs.tail, pred);
      if (pred(xs.head))
        Pair(xs.head :: tailPartition._1, tailPartition._2)
      else
        Pair(tailPartition._1, xs.head :: tailPartition._2)
    }
  }

  def quicksort[a] (less : (a,a) => boolean) (xs : List[a]) : List[a] = {
    if (xs.isEmpty)
      xs
    else {
      val pivot = xs.head;
      val sub = partition(xs.tail, (elem : a => less(elem, pivot)));
      quicksort(less)(sub._1) ::: List(pivot) ::: quicksort(less)(sub._2)
    }
  }

  def test = {
    System.out.println(partition[int](List(1,2,3,4,5,6,7,8), (x => x < 0)));
    System.out.println(partition[int](List(1,2,3,4,5,6,7,8), (x => x < 5)));
    System.out.println(partition[int](List(1,2,3,4,5,6,7,8), (x => x < 9)));
    System.out.println();

    System.out.println(partition[int](List(8,7,6,5,4,3,2,1), (x => x < 0)));
    System.out.println(partition[int](List(8,7,6,5,4,3,2,1), (x => x < 5)));
    System.out.println(partition[int](List(8,7,6,5,4,3,2,1), (x => x < 9)));
    System.out.println();

    System.out.println(partition[int](List(7,2,1,5,4,3,8,6), (x => x < 0)));
    System.out.println(partition[int](List(7,2,1,5,4,3,8,6), (x => x < 5)));
    System.out.println(partition[int](List(7,2,1,5,4,3,8,6), (x => x < 9)));
    System.out.println();

    System.out.println(quicksort[int]((x,y) => x < y)(List(7,2,1,5,4,3,8,6)));
    System.out.println();
  }
}

//############################################################################

module M1 {
  def partition[a](xs: List[a], pred: a => boolean): Pair[List[a], List[a]] = {
    xs.foldRight[Pair[List[a], List[a]]](Pair(List(), List())) {
      (x, p) => if (pred (x)) Pair(x :: p._1, p._2) else Pair(p._1, x :: p._2)
    }
  }

  def quicksort[a] (less : (a,a) => boolean) (xs : List[a]) : List[a] = {
    if (xs.isEmpty)
      xs
    else {
      val pivot = xs.head;
      val sub = partition(xs.tail, (elem : a => less(elem, pivot)));
      quicksort(less)(sub._1) ::: List(pivot) ::: quicksort(less)(sub._2)
    }
  }

  def test = {
    System.out.println(partition[int](List(1,2,3,4,5,6,7,8), (x => x < 0)));
    System.out.println(partition[int](List(1,2,3,4,5,6,7,8), (x => x < 5)));
    System.out.println(partition[int](List(1,2,3,4,5,6,7,8), (x => x < 9)));
    System.out.println();

    System.out.println(partition[int](List(8,7,6,5,4,3,2,1), (x => x < 0)));
    System.out.println(partition[int](List(8,7,6,5,4,3,2,1), (x => x < 5)));
    System.out.println(partition[int](List(8,7,6,5,4,3,2,1), (x => x < 9)));
    System.out.println();

    System.out.println(partition[int](List(7,2,1,5,4,3,8,6), (x => x < 0)));
    System.out.println(partition[int](List(7,2,1,5,4,3,8,6), (x => x < 5)));
    System.out.println(partition[int](List(7,2,1,5,4,3,8,6), (x => x < 9)));
    System.out.println();

    System.out.println(quicksort[int]((x,y) => x < y)(List(7,2,1,5,4,3,8,6)));
    System.out.println();
  }
}

//############################################################################

module M2 {

  def powerset[a] (s : List[a]) : List[List[a]] = {
    if (s.isEmpty)
      List(List())
    else {
      val x = s.head;
      val withoutX = powerset(s.tail);
      withoutX ::: withoutX.map(s1 : List[a] => x::s1)
    }
  }

  def test = {
    System.out.println(powerset(List()));
    System.out.println(powerset(List(1)));
    System.out.println(powerset(List(1,2)));
    System.out.println(powerset(List(1,2,3)));
    System.out.println(powerset(List(1,2,3,4)));
    System.out.println();
  }
}

//############################################################################

module M3 {

  type Placement = List[Pair[int,int]];

  def queens(n: int): List[Placement] = {
    def placeQueens(row: int): List[Placement] = {
      if (row == 0)
        List(List())
      else {
        def isSafe(column: int, placement: Placement): boolean =
          placement forall {
            pos => pos._2 != column
              && abs(pos._2 - column) != row - pos._1
          }

        def adjoinRow(placement: Placement): List[Placement] =
          range(1, n)
            filter (column => isSafe(column, placement))
            map (column => Pair(row, column) :: placement);

        placeQueens(row - 1) flatMap adjoinRow
      }
    }
    placeQueens(n)
  }

  def range(lo: int, hi: int): List[int] =
    if (lo > hi) List()
    else lo :: range(lo + 1, hi);

  def abs(x: int) = if (x < 0) 0 - x else x;

  def test = {
    System.out.println("queens(1) = " + queens(1));
    System.out.println("queens(2) = " + queens(2));
    System.out.println("queens(3) = " + queens(3));
    System.out.println("queens(4) = " + queens(4));
    System.out.println();
  }
}

//############################################################################

module Test {
  def main(args: Array[String]): unit = {
    M0.test;
    M1.test;
    M2.test;
    M3.test;
    ()
  }
}

//############################################################################
