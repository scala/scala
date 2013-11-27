class Test {
  def f1(t: String) = {
    trait T {
      def xs = Nil map (_ => t)
    }
    ()
  }
}
