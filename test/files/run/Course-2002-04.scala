//############################################################################
// Programmation IV - 2002 - Week 04
//############################################################################
// $Id$

import java.lang.System; // to avoid name clash with .NET's library

object M0 {

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

object M1 {

  def mergesort[a] (less : (a,a) => Boolean) (xs: Array[a]): Unit = {

    def While(def c: Boolean)(def b: Unit): Unit =
      if (c) { b ; While(c)(b) } else ();

    def swap(i: Int, j: Int): Unit = {
      val t = xs(i);
      val u = xs(j);
      xs(i) = u;
      xs(j) = t;
    }

    def sort1(l: Int, r: Int): Unit = {
      val pivot = xs((l + r) / 2);
      var i = l;
      var j = r;
      While (i <= j) {
	While (less(xs(i), pivot)) { i = i + 1 }
	While (less(pivot, xs(j))) { j = j - 1 }
	if (i <= j) {
	  swap(i, j);
	  i = i + 1;
	  j = j - 1;
	}
      }
      if (l < j) sort1(l, j);
      if (j < r) sort1(i, r);
    }

    if (xs.length > 0) sort1(0, xs.length - 1);
  }

  def list2array(list: List[Int]): Array[Int] = {
    val array = new Array[Int](list.length);
    list.copyToArray(array, 0);
    array;
  }

  def array2list(array: Array[Int]): List[Int] = {
    var list = List[Int]();
    List.range(0, array.length).map(i => list = array(i) :: list);
    list.reverse;
  }

  def isort(list: List[Int]): List[Int] = {
    val array = list2array(list);
    mergesort[Int]((x,y) => x < y)(array);
    array2list(array);
  }

  def test = {
    val list0 = List();
    val list1 = List(0);
    val list2 = List(0,1);
    val list3 = List(1,0);
    val list4 = List(0,1,2);
    val list5 = List(1,0,2);
    val list6 = List(0,1,2);
    val list7 = List(1,0,2);
    val list8 = List(2,0,1);
    val list9 = List(2,1,0);
    val listA = List(6,3,1,8,7,1,2,5,8,4);

    System.out.println("list0: " + list0 + " -> " + isort(list0));
    System.out.println("list1: " + list1 + " -> " + isort(list1));
    System.out.println("list2: " + list2 + " -> " + isort(list2));
    System.out.println("list3: " + list3 + " -> " + isort(list3));
    System.out.println("list4: " + list4 + " -> " + isort(list4));
    System.out.println("list5: " + list5 + " -> " + isort(list5));
    System.out.println("list6: " + list6 + " -> " + isort(list6));
    System.out.println("list7: " + list7 + " -> " + isort(list7));
    System.out.println("list8: " + list8 + " -> " + isort(list8));
    System.out.println("list9: " + list9 + " -> " + isort(list9));
    System.out.println("listA: " + listA + " -> " + isort(listA));
    System.out.println();
  }

}

//############################################################################

object M2 {

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

object M3 {

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

    System.out.println("List(v1) * id = " + matrixTimesMatrix(List(v1),id));
    System.out.println("List(v1) * m1 = " + matrixTimesMatrix(List(v1),m1));
    System.out.println("List(v1) * m2 = " + matrixTimesMatrix(List(v1),m2));
    System.out.println();

    System.out.println("id * List(v1) = " + matrixTimesMatrix(id,List(v1)));
    System.out.println("m1 * List(v1) = " + matrixTimesMatrix(m1,List(v1)));
    System.out.println("m2 * List(v1) = " + matrixTimesMatrix(m2,List(v1)));
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

object Test {
  def main(args: Array[String]): Unit = {
    M0.test;
    M1.test;
    M2.test;
    M3.test;
    ()
  }
}

//############################################################################
