// Tests to assert that references to threads are not strongly held when scala-reflection is used inside of them.
object Test {
  import scala.ref.WeakReference

  def forceGc() = {
    var obj = new Object
    val ref = new WeakReference(obj)
    obj = null;
    while(ref.get.nonEmpty)
      Array.ofDim[Byte](16 * 1024 * 1024)
  }

  def main(args: Array[String]): Unit = {
    val threads = for (i <- (1 to 16)) yield {
      val t = new Thread {
        override def run(): Unit = {
          import reflect.runtime.universe._
          typeOf[List[String]] <:< typeOf[Seq[_]]
        }
      }
      t.start()
      t.join()
      WeakReference(t)
    }
    forceGc()
    val nonGCdThreads = threads.filter(_.get.nonEmpty).length
    assert(nonGCdThreads == 0, s"${nonGCdThreads} threads were retained; expected 0.")
  }
}
