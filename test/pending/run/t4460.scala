trait A

class B(val x: Int) {
  self: A =>

  def this() = this()
}

object Test extends B(2) with A  {
  def main(args: Array[String]) {  }
}

