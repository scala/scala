package scala.reflect.internal.names

import java.util.concurrent.{CyclicBarrier, Executors, TimeUnit}

import org.junit.Assert.assertSame
import org.junit.Test

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

trait ConcurrentNamesTest {
  self: ExtendedNameTest =>
  @Test def concurrentLookup: Unit = {
    val threads = 20
    val barrier = new CyclicBarrier(threads)
    val pool = Executors.newCachedThreadPool()
    implicit val context = ExecutionContext.fromExecutor(pool)
    try {
      val futures = for (thread <- 0 until threads) yield {
        Future{
          barrier.await()
          lookupInRandom
        }
      }
      val results = futures map {Await.result(_, Duration(1, TimeUnit.MINUTES))}
      val first = results(0)
      for (result <- results;
        i <- 0 until sources.length) {
        assertSame(first(i), result(i))
      }
    } finally pool.shutdownNow()
  }
}
