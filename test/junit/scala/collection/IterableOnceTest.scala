package scala.collection


import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.language.higherKinds
import org.junit.Assert.assertEquals

import scala.concurrent.Await



@RunWith(classOf[JUnit4])
class IterableOnceTest {

  // tests scala/bug#11054
  @Test
  def buildFromIterableOnceToIterableOnce: Unit = {
    import scala.concurrent.{ExecutionContext, Future}
    import scala.concurrent.duration._
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(_.run())

    val iterableOnce: IterableOnce[Future[Char]] = Iterator('a', 'b', 'c').map(Future.successful)
    val result = Future.sequence(iterableOnce)
    assertEquals(Seq('a','b','c'), Await.result(result, 10.millis).iterator.to(Seq))
  }

}

