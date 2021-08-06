package scala.collection

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class SeqViewTest {
  @Test
  def _toString(): Unit = {
    assertEquals("SeqView(<not computed>)", Seq(1, 2, 3).view.toString)
  }
}
