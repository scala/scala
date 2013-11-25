trait M[-X] {
  def m(x: X): Boolean
}

class C
class A { class C }

object Test {
  def main(args: Array[String]) {
    val a = new A

    // class O extends M[a.C] { def m(x: a.C) = true }
    // (new O: M[Null]).m(null) // Okay

    ((a: A) => {
      class N extends M[a.C] { def m(x: a.C) = true }
      new N: M[Null]
    }).apply(a).m(null) // NPE, missing bridge
  }
}
