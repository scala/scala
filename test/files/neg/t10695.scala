
import X._

object Main extends App {

  val node: raw.Node = null

  Seq().fold(node)(_ => _)

}

object X {
  def raw(s: String) = ???
}
