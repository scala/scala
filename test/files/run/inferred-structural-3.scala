import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-Xsource:3 -Xsource-features:no-infer-structural,infer-override"
  def code =
    """trait A { def f: AnyRef }                    // refinement dropped
      |def a = Option(new { def g = 1 })            // refinement dropped
      |def b: Option[{ def g: Int }] = Option(new { def g = 1 })
      |def c(p: { def i: Int }): Int = 0
      |def d = new A { def f: A = this }            // refinement of existing method is kept, in Scala 3 too
      |def e = new A { def f: AnyRef = new AnyRef } // no refinement in 2.13 eihter
      |def f = new A { def f = new AnyRef }         // no refinement in 2.13 either
      |def g = new A { def f = this }               // inferred type of `f` is AnyRef because of infer-override
      |def h = new AnyRef { type T = String }       // TODO: dropped in Scala 3; figure out the rules Scala 3 uses and approximate them
      |def i = new AnyRef { val x = 2 }             // dropped
      |""".stripMargin
}
