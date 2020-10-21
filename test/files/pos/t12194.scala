object Test {
  trait Trait[A]
  class CaseA extends Trait[Int]
  class CaseB extends Trait[Boolean]

  object Pattern {
    def unapply(fa: CaseB): Option[CaseB] = None
  }

  def foo[A](t: Trait[A]) = {
    def bar(f: Trait[A]): Unit = {}

    t match {
      case Pattern(_) =>
    }

    t match {
      case a: CaseA => bar(new CaseA)
    }
  }
}
