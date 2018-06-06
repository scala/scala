import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |import scala.reflect.classTag
    |classTag[scala.List[_]]
    |classTag[scala.collection.immutable.List[_]]
    |classTag[Predef.Set[_]]
    |classTag[scala.collection.immutable.Set[_]]
  """.stripMargin
}
