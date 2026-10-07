import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code =
    """:power
      |@annotation.meta.languageFeature("", false) class ann(x: Int) extends annotation.StaticAnnotation
      |class C { @ann(21) val x = 1 }
      |typeOf[C].member(TermName("x ")).annotations.head
      |object defs {
      |  import scala.annotation.StaticAnnotation
      |  sealed trait InputAnnotation extends StaticAnnotation
      |  class body(val s: String) extends InputAnnotation
      |  class jsonbody extends body("x")
      |  case class Cls(@jsonbody id: String)
      |}
      |import defs._
      |val a = typeOf[Cls].typeSymbol.primaryConstructor.paramss.head.head.annotations.head
      |a.argsForSuper(typeOf[body].typeSymbol)
      |""".stripMargin
}
