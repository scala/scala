module test {

  type String = java.lang.String;

  def while(def c: Boolean)(def b: Unit): Unit =
    if (c) { b ; while(c)(b) }
    else ();

  def sort(a: Array[Double]): Unit = {

    def swap(i: Int, j: Int): Unit = {
      val t = a(i) ; val u = a.apply(j) ; a(i) = u ; a(j) = t
    }

    def sort1(l: Int, r: Int): Unit = {
      val pivot = a((l + r) / 2);
      var i = l, j = r;
      while (i <= j) {
	while (a(i) < pivot) { i = i + 1 }
	while (a(j) > pivot) { j = j - 1 }
	if (i <= j) {
	  swap(i, j);
	  i = i + 1;
	  j = j - 1;
	}
      }
      if (l < j) sort1(l, j);
      if (j < r) sort1(i, r);
    }

    sort1(0, a.length - 1);
  }
}