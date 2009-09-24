class C {
  def this(x: Int) = {
    this();
    class D extends C;
  }
}
