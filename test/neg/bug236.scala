  class A {
    private def this(i: Int) = this();
  }
  class B extends A(1) {
    val a = new A(1);
  }
