import scala.annotation.unchecked.uncheckedStable

object Test {
  @uncheckedStable def A = 9
  @uncheckedStable def o = O
  @uncheckedStable var T = "hi"
  @uncheckedStable var Uh = new U(1)

  def m(a: Any) = a match {
    case A => 1
    case `o` => 2
    case T => 3
  }

  def main(args: Array[String]): Unit = {
    assert(m(9) == 1)
    assert(m(O) == 2)
    assert(m("hi") == 3)
    T = "by"
    assert(m("by") == 3)

    val oc = new o.C
    assert(oc.oof + oc.cf == 3)

    val uc = new Uh.C
    assert(uc.uuf + uc.cf == 3)
    Uh = new U(2)
    assert(uc.uuf + uc.cf == 3)
  }
}

object O {
  val of = 1
  class C {
    def oof = of
    val cf = 2
  }
}

class U(x: Int) {
  val uf = x
  class C {
    def uuf = uf
    val cf = 2
  }
}
