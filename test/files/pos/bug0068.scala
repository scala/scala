class E {
  def f() = {
    val (_::l1) = List(1,2,3);
    l1.tail;
  }
}
