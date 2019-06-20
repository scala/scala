
import scala.collection._
import scala.concurrent._
import scala.concurrent.duration.Duration
import java.util.concurrent.{TimeoutException, CountDownLatch, TimeUnit}

object Test {

  val DefaultTimeout = Duration(5, TimeUnit.SECONDS)

  def main(args: Array[String]): Unit = {
    (new FutureTests).check()
    (new PromiseTests).check()
    (new TryTests).check()
  }

}

trait Features {
  implicit def implicitously = scala.language.implicitConversions
  implicit def reflectively  = scala.language.reflectiveCalls
  implicit def postulously   = scala.language.postfixOps
}


trait Output {
  val buffer = new StringBuilder

  def bufferPrintln(a: Any) = buffer.synchronized {
    buffer.append(a.toString + "\n")
  }
}


trait MinimalScalaTest extends Output with Features with Vigil {

  val throwables = mutable.ArrayBuffer[Throwable]()

  def check(): Unit = {
    if (throwables.nonEmpty) println(buffer.toString)
  }

  implicit def stringops(s: String) = new {

    def should[U](snippets: => U) = {
      bufferPrintln(s + " should:")
      snippets
    }

    def in[U](snippet: => U) = {
      try {
        bufferPrintln("- " + s)
        snippet
        bufferPrintln("[OK] Test passed.")
      } catch {
        case e: Throwable =>
          bufferPrintln("[FAILED] " + e)
          bufferPrintln(e.getStackTrace().mkString("\n"))
          throwables += e
      }
    }

  }

  implicit def objectops(obj: Any) = new {

    def mustBe(other: Any) = assert(obj == other, s"$obj is not $other")
    def mustEqual(other: Any) = mustBe(other)

  }

  def intercept[T <: Throwable: Manifest](body: => Any): T = {
    try {
      body
      throw new Exception("Exception of type %s was not thrown".format(manifest[T]))
    } catch {
      case t: Throwable =>
        if (!manifest[T].runtimeClass.isAssignableFrom(t.getClass)) throw t
        else t.asInstanceOf[T]
    }
  }

  def checkType[T: Manifest, S](in: Future[T], refmanifest: Manifest[S]): Boolean = manifest[T] == refmanifest
}

trait Vigil {
  def waitForIt(terminated: => Boolean): Unit = {
    val limit = 5
    var n = 1
    var (dormancy, factor) = (250L, 4)
    var period = 0L
    var done = false
    var ended = false
    while (!done && n < limit) {
      try {
        ended = terminated
        if (ended) {
          done = true
        } else {
          Thread.sleep(dormancy)
          period += dormancy
        }
      } catch {
        case _: InterruptedException => done = true
      }
      n += 1
      dormancy *= factor
    }
    assert(ended, s"Expired after dormancy period $period waiting for termination condition")
  }
}

object TestLatch {
  val DefaultTimeout = Test.DefaultTimeout

  def apply(count: Int = 1) = new TestLatch(count)
}


class TestLatch(count: Int = 1) extends Awaitable[Unit] {
  private var latch = new CountDownLatch(count)

  def countDown() = latch.countDown()
  def isOpen: Boolean = latch.getCount == 0
  def open() = while (!isOpen) countDown()
  def reset() = latch = new CountDownLatch(count)

  @throws(classOf[TimeoutException])
  def ready(atMost: Duration)(implicit permit: CanAwait) = {
    val opened = latch.await(atMost.toNanos, TimeUnit.NANOSECONDS)
    if (!opened) throw new TimeoutException("Timeout of %s." format (atMost.toString))
    this
  }

  @throws(classOf[TimeoutException])
  def result(atMost: Duration)(implicit permit: CanAwait): Unit = {
    ready(atMost)
  }

}
