import scala.tools.partest.ReplTest

// check that we don't lose the annotation on the existential type nested in an intersection type
// it's okay that List[_] is represented as List[Any] -- they are equivalent due to variance (existential extrapolation)
// (The above comment should not be construed as an endorsement of rewrapping as a great way to implement a bunch of different type "proxies")
object Test extends ReplTest {
  def code = """
    |def existWith(x: (List[T] forSome {type T}) with Int {def xxx: Int}) = ???
    |def existKeepsAnnot(x: (List[T] forSome {type T})@SerialVersionUID(1L) with Int {def xxx: Int}) = ???
  """.stripMargin
}
