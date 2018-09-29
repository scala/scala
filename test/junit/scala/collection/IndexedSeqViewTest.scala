package scala.collection

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class IndexedSeqViewTest {

  @Test def t11159(): Unit = {
    (1 +: 2 +: Vector().view) : IndexedSeqView[Int]
    (Vector().view :+ 1 :+ 2) : IndexedSeqView[Int]
    (Vector(1, 2).view ++ Vector(3, 4).view) : IndexedSeqView[Int]
    (Vector(1, 2).view ++ List(3, 4).view) : SeqView[Int]
    (List(1, 2).view ++ Vector(3, 4).view) : SeqView[Int]
  }
}
