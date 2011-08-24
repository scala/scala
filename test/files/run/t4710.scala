import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """def method : String = { implicit def f(s: Symbol) = "" ; 'symbol }"""
}

