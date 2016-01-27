object OhNoes {

  sealed trait F
  sealed abstract class FA extends F
  sealed abstract class FB extends F

  case object FA1 extends FA
  case object FB1 extends FB
  case object FB2 extends FB

  sealed trait G
  case object G1 extends G
  case object G2 extends G

  sealed trait H
  case class H1(a: FB, b: G) extends H
  case class H2(b: F)        extends H

  val demo: H => Unit = {
    case H1(FB1, G1) =>
    case H1(FB2, G2) =>
    case H2(_: FB) =>
    case H2(_: FA) =>
    case H1(FB1, G2) =>
    case H1(FB2, G1) =>
  }

  val demo2: H => Unit = {
    case H2(_: FA) =>
    case H2(_: FB) =>
    case H1(FB1, G1) =>
    case H1(FB2, G1) =>
    case H1(FB1, G2) =>
    case H1(FB2, G2) =>
  }
}
