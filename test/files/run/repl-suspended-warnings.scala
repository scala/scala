import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-Wconf:any:e"
  def code =
    """@annotation.nowarn def f { }
      |def f { }
      |@annotation.nowarn def f { }
      |class C { def match = 42 }
      |class C { def `match` = 42 }
      |class C { def `match` = 42 }
      |""".stripMargin
}
