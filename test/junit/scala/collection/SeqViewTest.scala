
package scala.collection

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class SeqViewTest {

  @Test def t11159(): Unit = {
    (1 +: 2 +: Nil.view) : SeqView[Int]
    (Nil.view :+ 1 :+ 2) : SeqView[Int]
    (List(1, 2).view ++ List(3, 4).view) : SeqView[Int]
  }
}
