import scala.collection.mutable._

trait SB[A] extends Buffer[A] {
  abstract override def insertAll(n: Int, iter: IterableOnce[A]): Unit = synchronized {
     super.insertAll(n, iter)
  }

  abstract override def update(n: Int, newelem: A): Unit = synchronized {
    super.update(n, newelem)
  }
}

object Test extends App {
  new ArrayBuffer[Int] with SB[Int]
}

