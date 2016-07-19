import scala.tools.nsc.interactive.tests._

object Test extends InteractiveTest {
  // Normalize ordering of LUB
  override def normalize(s: String) = s.replace("Serializable with Product", "Product with Serializable")
}
