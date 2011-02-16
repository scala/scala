object Test {

  class SA[@specialized(Int) A] {
    def o[U](f: ((Int, A) => Any)) {}

    def o[U](f: A => Any) {}
  }

  class X[@specialized(Int) B] {
    def x(b: B) = {
      new SA[B]().o((x: Any) => x)
    }
  }
}

