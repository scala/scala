//############################################################################
// Programmation IV - 2002 - Week 04
//############################################################################
// $Id$

module M0 {

  def quicksort[a] (less : (a,a) => Boolean) (xs : List[a]) : List[a] = {
    if (xs.isEmpty)
      xs
    else {
      val pivot : a = xs.head;
      val smaller : List[a] =
        quicksort(less)(xs.tail.filter(elem => less(elem, pivot)));
      val greaterOrEqual : List[a] =
        quicksort(less)(xs.tail.filter(elem => !less(elem, pivot)));
      smaller ::: List(pivot) ::: greaterOrEqual
    }
  }

  def test = {
    val isort = quicksort[Int]((x,y) => x < y);
    val list0 = List(6,3,1,8,7,1,2,5,8,4,3,4,8);
    val list1 = quicksort[Int]((x,y) => x < y)(list0);
    val list2 = quicksort[Int]((x,y) => x < y)(list1);
    val list3 = isort(list0);
    val list4 = isort(list1);
    val list5 = quicksort[Int]((x,y) => x >= y)(list0);
    val list6 = quicksort[Int]((x,y) => x >= y)(list1);

    System.out.println("list0 = " + list0);
    System.out.println("list1 = " + list1);
    System.out.println("list2 = " + list2);
    System.out.println("list3 = " + list3);
    System.out.println("list4 = " + list4);
    System.out.println("list5 = " + list5);
    System.out.println("list6 = " + list6);
    System.out.println();
  }
}

//############################################################################

module M1 {

  def horner (x : Double, coefs : List[Double]) : Double = {
    if (coefs.isEmpty)
      0
    else
      horner(x, coefs.tail) * x + coefs.head
  }

  def test = {
    val poly = List(9.0,5.0,7.0,5.0);
    System.out.println("f(x) = 5x^3+7x^2+5x+9");
    System.out.println("f(0) = " + horner(0, poly));
    System.out.println("f(1) = " + horner(1, poly));
    System.out.println("f(2) = " + horner(2, poly));
    System.out.println("f(3) = " + horner(3, poly));
    System.out.println();
  }
}

//############################################################################

module M2 {

  def dotproduct (v : List[Double], w : List[Double]) : Double = {
    if (v.isEmpty)
      0
    else
      (v.head * w.head) + dotproduct(v.tail, w.tail)
  }

  def matrixTimesVector (m : List[List[Double]], v : List[Double])
                        : List[Double] = {
    m.map(row => dotproduct(row, v))
  }

  def transpose(m : List[List[Double]]) : List[List[Double]] = {
    if (m.isEmpty || m.head.isEmpty)
      List()
    else
      m.map(row => row.head) :: transpose (m.map (row => row.tail))
  }

  def matrixTimesMatrix(m1 : List[List[Double]], m2 : List[List[Double]])
                       : List[List[Double]] = {
    val columns = transpose(m2);
    m1.map(row => matrixTimesVector(columns, row))
  }

  def test = {
    val v1 = List(2.0,3.0,4.0);
    val v2 = List(6.0,7.0,8.0);
    def id = List(List(1.0,0.0,0.0),List(0.0,1.0,0.0),List(0.0,0.0,1.0));
    def m1 = List(List(2.0,0.0,0.0),List(0.0,2.0,0.0),List(0.0,0.0,2.0));
    def m2 = List(List(1.0,2.0,3.0),List(4.0,5.0,6.0),List(7.0,8.0,9.0));

    def v = List(2.0,3.0,4.0);

    System.out.println("v1        = " + v1);
    System.out.println("v2        = " + v2);
    System.out.println();

    System.out.println("id        = " + id);
    System.out.println("m1        = " + m1);
    System.out.println("m2        = " + m2);
    System.out.println();

    System.out.println("v1 * v1   = " + dotproduct(v1,v1));
    System.out.println("v1 * v2   = " + dotproduct(v1,v2));
    System.out.println("v2 * v1   = " + dotproduct(v2,v1));
    System.out.println("v1 * v2   = " + dotproduct(v1,v2));
    System.out.println();

    System.out.println("id * v1   = " + matrixTimesVector(id,v1));
    System.out.println("m1 * v1   = " + matrixTimesVector(m1,v1));
    System.out.println("m2 * v1   = " + matrixTimesVector(m2,v1));
    System.out.println();

    System.out.println("trn(id)   = " + transpose(id));
    System.out.println("trn(m1)   = " + transpose(m1));
    System.out.println("trn(m2)   = " + transpose(m2));
    System.out.println();

    System.out.println("[v1] * id = " + matrixTimesMatrix(List(v1),id));
    System.out.println("[v1] * m1 = " + matrixTimesMatrix(List(v1),m1));
    System.out.println("[v1] * m2 = " + matrixTimesMatrix(List(v1),m2));
    System.out.println();

    System.out.println("id * [v1] = " + matrixTimesMatrix(id,List(v1)));
    System.out.println("m1 * [v1] = " + matrixTimesMatrix(m1,List(v1)));
    System.out.println("m2 * [v1] = " + matrixTimesMatrix(m2,List(v1)));
    System.out.println();

    System.out.println("id * id   = " + matrixTimesMatrix(id,id));
    System.out.println("id * m1   = " + matrixTimesMatrix(id,m1));
    System.out.println("m1 * id   = " + matrixTimesMatrix(m1,id));
    System.out.println("m1 * m1   = " + matrixTimesMatrix(m1,m1));
    System.out.println("id * m2   = " + matrixTimesMatrix(id,m2));
    System.out.println("m2 * id   = " + matrixTimesMatrix(m2,id));
    System.out.println("m1 * m2   = " + matrixTimesMatrix(m1,m2));
    System.out.println("m2 * m1   = " + matrixTimesMatrix(m2,m1));
    System.out.println("m2 * m2   = " + matrixTimesMatrix(m2,m2));
    System.out.println();
  }
}

//############################################################################

module Test {
  def main(args: Array[String]): Unit = {
    M0.test;
    M1.test;
    M2.test;
    ()
  }
}

//############################################################################
