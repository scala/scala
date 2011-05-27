import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
2 ; 3
{ 2 ; 3 }
5 ; 10 ; case object Cow ; 20 ; class Moo { override def toString = "Moooooo" } ; 30 ; def bippy = {
  1 +
  2 +
  3 } ; bippy+88+11

object Bovine { var x: List[_] = null } ; case class Ruminant(x: Int) ; bippy * bippy * bippy
Bovine.x = List(Ruminant(5), Cow, new Moo)
Bovine.x
  """
}