//> using options --target:11
//> using jvm 11+

class C private (private val i: Int, private val j: Int) {
  private val c = i + C.secret

  @inline def f = j * 2
}
object C {
  def unwrap(c: C): Int = c.c

  def apply(i: Int, j: Int): C = new C(i, j)

  private def secret = 5
}

class D(d0: String) {
  private def d = d0
  def e = new E
  class E {
    def e = D.this.d
  }
}
object D {
}

object Top {
  private def i = 42
  class Nested {
    def f = i
  }
  def j = new Nested().f
}

class TopHeavy {
  private def i = TopHeavy.underlying
}
object TopHeavy {
  private def underlying = 42
  class Nested {
    def f = new TopHeavy().i
  }
  def j = new Nested().f
}

object Test {
  import java.lang.reflect.Modifier.{PRIVATE => Private}
  def main(args: Array[String]): Unit = {
    assert(C.unwrap(C(42, 27)) == 47)
    for (m <- Class.forName("C$").getDeclaredMethods; n = m.getName if n.contains("secret")) {
      assert(n == "secret")
      assert((m.getModifiers & Private) != 0)
    }

    val d = new D("mystery")
    assert(d.e.e == "mystery")
    for (m <- Class.forName("D").getDeclaredMethods; n = m.getName if n.contains("d")) {
      assert(n == "d")
      assert((m.getModifiers & Private) != 0)
    }

    assert(Top.j == 42)
    for (m <- Class.forName("Top$").getDeclaredMethods; n = m.getName if n.contains("i")) {
      assert(n == "i")
      assert((m.getModifiers & Private) != 0)
    }
  }
}
