






@deprecated("Suppress warnings", since="2.11")
object Test {

  def main(args: Array[String]) {
    ConcurrentMapSpec.test()
    IteratorSpec.test()
    LNodeSpec.test()
    SnapshotSpec.test()
  }



trait Spec {

  implicit def implicitously = scala.language.implicitConversions
  implicit def reflectively  = scala.language.reflectiveCalls

  implicit def str2ops(s: String) = new {
    def in[U](body: =>U) {
      // just execute body
      body
    }
  }

  implicit def any2ops(a: Any) = new {
    def shouldEqual(other: Any) = assert(a == other)
  }

  def evaluating[U](body: =>U) = new {
    def shouldProduce[T <: Throwable: ClassManifest]() = {
      var produced = false
      try body
      catch {
        case e: Throwable => if (e.getClass == implicitly[ClassManifest[T]].runtimeClass) produced = true
      } finally {
        assert(produced, "Did not produce exception of type: " + implicitly[ClassManifest[T]])
      }
    }
  }

}
}
