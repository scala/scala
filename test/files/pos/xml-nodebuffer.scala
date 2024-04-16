//> using options -Ystop-after:parser
//

import scala.xml.NodeBuffer

object foo {
  // SI-9027
  // xml-nodebuffer.scala:10: error: ';' expected but '.' found.
  val nodeBuffer: NodeBuffer = <hello/><world/>
  nodeBuffer.foreach(println)
}
