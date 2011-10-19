class A {
  def f1(t: String) = {
    trait T {
      def xs = Nil map (_ => t)
    }
  }
  def f2(t: String) = {
    def xs = Nil map (_ => t)
  }
  def f3(t: String) = {
    var t1 = 5
    trait T {
      def xs = { t1 = 10 ; t }
    }
  }
  def f4() = {
    var u = 5
    trait T {
      def xs = Nil map (_ => u = 10)
    }
  }
}
