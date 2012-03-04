package s {
  import j.J_1

  trait S extends J_1 {
    def bar() {
      foo()
    }
  }

  class SC extends J_1 with S
}

object Test {
  def main(args : Array[String]) {
    (new s.SC).bar()
    (new s.S { }).bar()
  }
}
