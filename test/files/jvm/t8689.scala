object Test {
  def main(args: Array[String]): Unit = {
    import scala.concurrent._
    import ExecutionContext.Implicits.global
    val source1 = Promise[Int]()
    val source2 = Promise[Int]()
    source2.completeWith(source1.future).future.onComplete {
      case _ => print("success")
    }
    source2.tryFailure(new TimeoutException)
    source1.success(123)
  }
}