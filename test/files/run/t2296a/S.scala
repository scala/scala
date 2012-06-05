package s {
  import j.J

  trait S extends J {
    def bar() {
      foo()
    }
  }

  class SC extends J with S
}

object Test {
  def main(args : Array[String]) {
    (new s.SC).bar()
    (new s.S { }).bar()
  }
}