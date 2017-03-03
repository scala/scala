import scala.language.higherKinds

trait Proxy[+T]
case class Stuff[+P[PP] <: Proxy[PP]]() {
  // canEqual was incorrectly synthetized and started reporting a kind error.
}
