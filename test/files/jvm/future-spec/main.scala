


import scala.collection._
import scala.concurrent._
import scala.concurrent.util.Duration
import java.util.concurrent.{ TimeoutException, CountDownLatch, TimeUnit }


object Test {
  
  def main(args: Array[String]) {
    FutureTests.check()
    PromiseTests.check()
  }
  
}


trait Output {
  val buffer = new StringBuilder
  
  def bufferPrintln(a: Any) = buffer.synchronized {
    buffer.append(a.toString + "\n")
  }
}


trait MinimalScalaTest extends Output {
  
  val throwables = mutable.ArrayBuffer[Throwable]()
  
  def check() {
    if (throwables.nonEmpty) println(buffer.toString)
  }
  
  implicit def stringops(s: String) = new {
    
    def should[U](snippets: =>U) = {
      bufferPrintln(s + " should:")
      snippets
    }
    
    def in[U](snippet: =>U) = {
      try {
        bufferPrintln("- " + s)
        snippet
        bufferPrintln("[OK] Test passed.")
      } catch {
        case e =>
          bufferPrintln("[FAILED] " + e)
          bufferPrintln(e.getStackTrace().mkString("\n"))
          throwables += e
      }
    }
    
  }
  
  implicit def objectops(obj: Any) = new {
    
    def mustBe(other: Any) = assert(obj == other, obj + " is not " + other)
    
  }
  
  def intercept[T <: Throwable: Manifest](body: =>Any): T = {
    try {
      body
      throw new Exception("Exception of type %s was not thrown".format(manifest[T]))
    } catch {
      case t: Throwable =>
        if (manifest[T].erasure != t.getClass) throw t
        else t.asInstanceOf[T]
    }
  }
  
  def checkType[T: Manifest, S](in: Future[T], refmanifest: Manifest[S]): Boolean = manifest[T] == refmanifest
}


object TestLatch {
  val DefaultTimeout = Duration(5, TimeUnit.SECONDS)

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
  
  @throws(classOf[Exception])
  def result(atMost: Duration)(implicit permit: CanAwait): Unit = {
    ready(atMost)
  }
  
}

