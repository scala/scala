class Gox {
  object Zed { }
  class Zed  { }
}

object Test {
  type GoxZed = t#Zed forSome { type t <: Gox }

  def main(args: Array[String]): Unit = {
    val x = new Gox
    val y: GoxZed = x
  }
}
