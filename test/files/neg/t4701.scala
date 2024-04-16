//> using options -Yrangepos
trait HL[A]
object HN {
  def :: [A](x: A): HL[A] = new HL[A] {}
}
object Test {
  import Predef.{identity => hasType}
  final val nnn = 1
  hasType[HL[String]](nnn :: HN) // type mismatch error should have position at `nnn`
}