import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |import scala.reflect.{ClassManifest, classTag}
    |implicitly[ClassManifest[scala.List[_]]]
    |classTag[scala.List[_]]
    |implicitly[ClassManifest[scala.collection.immutable.List[_]]]
    |classTag[scala.collection.immutable.List[_]]
    |implicitly[ClassManifest[Predef.Set[_]]]
    |classTag[Predef.Set[_]]
    |implicitly[ClassManifest[scala.collection.immutable.Set[_]]]
    |classTag[scala.collection.immutable.Set[_]]
  """.stripMargin
}
