import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-deprecation"

  def code = """
import scala.annotation.nowarn
scala.#::.unapply(Stream(1))
scala.#::.unapply(Stream(1)): @nowarn
(scala.#::.unapply(Stream(1)): @nowarn)
scala.#::.unapply(Stream(1)): @inline
(scala.#::.unapply(Stream(1)): @nowarn).isEmpty
"""
}
