import scala.tools.partest._

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Ytopleveldebug:definitionsWithMembers"

  override def show(): Unit = compile()

  override def code =
    """
    package s

    trait  A { var x = 1 ; lazy val y = 1 ; object Z }
    class  B { var x = 1 ; lazy val y = 1 ; object Z }
    object C { var x = 1 ; lazy val y = 1 ; object Z }
    """
}
