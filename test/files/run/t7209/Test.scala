import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def code = """
j.J.copyOf[String](new java.util.ArrayList[String]: java.util.Collection[_ <: String])
j.J.copyOf[String](new java.util.ArrayList[String]: java.util.Collection[_ <: String])
"""

// The second call was resulting in:
// <console>:34: error: type mismatch;
// found   : java.util.ArrayList[?0] where type ?0
// required: java.util.Collection[_ <: String]
//              j.J.copyOf[String](new java.util.ArrayList[String]: java.util.Collection[_ <: String])
//
// This was fixed in SI-7482
}