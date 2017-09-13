package shapeless {

class Defns {
  trait G[T] { type Repr }
  object G {
    type Aux[T, R] = G[T] { type Repr = R }
    def apply[T](implicit gen: G[T]): Aux[T, gen.Repr] = gen
    implicit def g[T]: Aux[T, Int] = new G[T] { type Repr = Int }
  }

  trait LG[T] { type Repr }
  object LG {
    type Aux[T, R] = LG[T] { type Repr = R }
    implicit def lg[T]: Aux[T, Int] = new LG[T] { type Repr = Int }
  }

  case class Quux2(i: Int, s: String)
  object Quux2 {
    val gen0 = cachedImplicit[G[Quux2]]
    implicit val gen: G.Aux[Quux2, gen0.Repr] = gen0

    val lgen0 = cachedImplicit[LG[Quux2]]
    implicit val lgen: LG.Aux[Quux2, lgen0.Repr] = lgen0
  }

  def testRefined2 {
    assert(Quux2.gen != null)
    assert(Quux2.gen eq Quux2.gen0)

    val gen = G[Quux2]
    assert(gen eq Quux2.gen0)
  }
}
}

object Test extends App {
  (new shapeless.Defns).testRefined2
}
