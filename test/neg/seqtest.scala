object test {

  def L[a](xs: a*): Seq[a] = xs;

  def L[a](xs: Seq[a]): Seq[a] = xs;

  val a = L(1);

  val b = L(1, 2, 3);

  val c = L(a);

}
