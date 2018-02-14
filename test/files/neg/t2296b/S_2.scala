package s {
  import j.J_1

  trait S extends J_1 {
    def bar(): Unit = {
      foo()
    }
  }

  class SC extends J_1 with S
}

object Test {
  def main(args : Array[String]): Unit = {
    (new s.SC).bar()
    (new s.S { }).bar()
  }
}
