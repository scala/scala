object t3556 {
  trait CollectsCases[C, El, +U, +V] {
    def caseDef(xs: C): U
    def caseSeq(xs: Seq[El]): V
  }

  trait Id {
    type Apply[U] = U
  }

  trait Collects[C, El, +CaseDef[T], +CaseSel[T]] {
    type PMat[U] = CollectsCases[C, El, CaseDef[U], CaseSel[U]]
    def doMatch[U](x: C)(pmat: PMat[U]): U
  }

  class CollectsDefault {
    implicit def collectsDef[T] = new Collects[T, T, Id#Apply, Any] {
      def doMatch[U](x: T)(pmat: PMat[U]) = pmat.caseDef(x)
    }
  }

  object Collects extends CollectsDefault {
    implicit def collectsSeq[C <: Seq[_], T](implicit ev: C <:< Seq[T]) = new Collects[C, T, Any, Id#Apply] {
      def doMatch[U](x: C)(pmat: PMat[U]) = pmat.caseSeq(x)
    }
  }

  object Test extends App {
    def frob[C, T](xs: C)(implicit ev: Collects[C, T, Any, Any]) /*BUG: Nothing is inferred */ = {
      val pmat = new CollectsCases[C, T, C, T]{
        def caseDef(xs: C) = xs
        def caseSeq(xs: Seq[T]) = xs.head
      }
      ev.doMatch(xs)(pmat)
    }
    println(frob(List(1, 2)))
    println(frob("not a List(1, 2)"))
  }
}

