import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
(2)
(2 + 2)
((2 + 2))
(((2 + 2)), ((2 + 2)))
(((2 + 2)), ((2 + 2)), 2)
((((2 + 2)), ((2 + 2)), 2).productIterator ++ Iterator(3) mkString)

55 ; ((2 + 2)) ; (1, 2, 3)
  """.trim
}