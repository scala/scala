object Test {
  def main(args: Array[String]): Unit = {
    import scala.concurrent._
    import ExecutionContext.Implicits.global
    val source1 = Promise[Int]()
    val source2 = Promise[Int]()
    val done = Promise[Unit]()
    source2.completeWith(source1.future).future.onComplete {
      case _ =>
         print("success")
         done.success(())
    }
    source2.tryFailure(new TimeoutException)
    source1.success(123)
    import duration._
    Await.result(done.future, 120.seconds)
  }
}
