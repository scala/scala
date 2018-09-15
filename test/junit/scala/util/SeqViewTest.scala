package scala

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.collection.SeqView
import scala.runtime.BoxedUnit

class SeqViewTest {
  @Test
  def t11159(): Unit = {

    val typeTest1: SeqView[Int] = Nil.view
    val typeTest2: SeqView[Int] = 1 +: 2 +: Nil.view

    assertEquals(List(1, 2), (1 +: 2 +: Nil.view).toList)
  }
}
