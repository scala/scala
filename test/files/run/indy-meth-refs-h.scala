// scalac: -Ydelambdafy:method-ref
trait Entity {
  def name: String
  def announce = {
    def msg = s"I am $name"
    None.getOrElse(msg)
  }
}

object Test extends Entity {
  def name = "Test"
  def main(args: Array[String]): Unit = assert(announce == "I am Test")
}
