
object Test {
  import scala.xml.NodeBuffer

  def main(args: Array[String]): Unit = {
    val xml =  <hello>world</hello>
    assert(xml.toString == "helloworld")
    val nodeBuffer: NodeBuffer = <hello/><world/>
    assert(nodeBuffer.mkString == "helloworld")
  }
}
