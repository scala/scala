import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
  |:kind scala.Option
  |:k (Int, Int) => Int
  |:k -v Either
  |:k -v scala.collection.generic.ImmutableSortedMapFactory
  |:k new { def empty = false }
  |:k Nonexisting
  """.stripMargin
}
