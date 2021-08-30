package scala.collection.concurrent

import java.util.concurrent.Executors
import scala.concurrent.duration.TimeUnit

class ConcurrentMapTester[K, V](map: Map[K, V]) {
  def runTasks(executionTimeout: Long, unit: TimeUnit)(tasks: (Map[K, V] => Unit)*): Unit = {
    val exec = Executors.newCachedThreadPool()
    for (task <- tasks) exec.execute(() => task(map))
    exec.shutdown()
    exec.awaitTermination(executionTimeout, unit)
  }

  @throws[AssertionError]
  def assertContainsEntry(k: K, v: V): Unit = {
    val value = map.get(k)
    assert(value.isDefined, s"map does not contain key '$k'")
    assert(value.contains(v), s"key '$k' is mapped to '${value.get}', not to '$v'")
  }

  @throws[AssertionError]
  def assertExistsEntry(k: K, p: V => Boolean): Unit = {
    val value = map.get(k)
    assert(value.isDefined, s"map does not contain key '$k'")
    assert(value.exists(p), s"key '$k' is mapped to '${value.get}', which does not match the predicate")
  }

  @throws[AssertionError]
  def assertDoesNotContain(k: K): Unit = {
    val value = map.get(k)
    assert(value.isEmpty, s"key '$k' is not empty and is mapped to '${value.get}'")
  }
}
