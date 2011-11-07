object Test {
  def f = "abc".count(_ > 'a')
  
  class A {
    private val count: Int = 0
  }
  class B extends A { }
  object B {
    implicit def b2seq(x: B): Seq[Int] = Nil

    def f = (new B) count (_ > 0)
  }
}
