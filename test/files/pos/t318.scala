object Test {
  def fun: Int = {
    object o {
      def a: Int = 1;
      class C { def b: Int =  a; }
    }
    0
  }
}
