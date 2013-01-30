import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |import scala.reflect.classTag
    |classManifest[scala.List[_]]
    |classTag[scala.List[_]]
    |classManifest[scala.collection.immutable.List[_]]
    |classTag[scala.collection.immutable.List[_]]
    |classManifest[Predef.Set[_]]
    |classTag[Predef.Set[_]]
    |classManifest[scala.collection.immutable.Set[_]]
    |classTag[scala.collection.immutable.Set[_]]
  """.stripMargin
}
