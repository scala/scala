// /scala/trac/5359/a.scala
// Thu Jan  5 13:31:05 PST 2012

object test {
  trait Step[F[_]] {
    // crash: typeConstructor inapplicable for <none>
    this match {
      case S1() =>
    }
  }
  case class S1[F[_]]() extends Step[F]

  // okay
  (null: Step[Option]) match {
    case S1() =>
  }
}
