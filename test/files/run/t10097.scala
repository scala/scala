
case class C(implicit c: Int)

object Test extends App {
  assert(C()(42).productArity == 0)
}
