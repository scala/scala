// $Id$

object M0 {
  type Placement = List[Int];

  def range(lo: Int, hi: Int): List[Int] =
    if (lo > hi) List()
    else lo :: range(lo + 1, hi);

  def abs(x: Int) = if (x < 0) 0 - x else x;

  def queens(n: Int): List[Placement] = {
    val columns = range(1, n);
    def placeQueens(row: Int): List[Placement] = {
      if (row == 0)
        List(List())
      else {
        def isSafe(col: Int, p: Placement, delta: Int): Boolean =
          p.isEmpty ||
          (col != p.head &&
           abs(col - p.head) != delta &&
           isSafe(col, p.tail, delta + 1));

        for {
          val placement <- placeQueens(row - 1);
          val col <- columns;
          isSafe(col, placement, 1)
        } yield {
          col :: placement
        }
      }
    }
    placeQueens(n);
  }

  def test = {
    System.out.println("Solutions to 1 queens: " + queens(1));
    System.out.println("Solutions to 2 queens: " + queens(2));
    System.out.println("Solutions to 3 queens: " + queens(3));
    System.out.println("Solutions to 4 queens: " + queens(4));
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    M0.test;
    ()
  }
}
