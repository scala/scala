/* $Id$ */

object iq {
  def main = {
    val q:scala.collection.immutable.Queue[Int] =
	scala.collection.immutable.Queue.Empty;
    if(q.isEmpty) {
      java.lang.System.out.println("Empty");
    }

    val q2 = q + 42;

    if(q2.isEmpty) {
      java.lang.System.out.println("Empty");
    }

  }
}

object Test {
  def main(args: Array[String]): Unit = {
    iq.main;
    ();
  }
}
