/* $Id$ */

object iq {
  def main = {
    val q:scala.collection.immutable.Queue[Int] =
      scala.collection.immutable.Queue.Empty;
    if(q.isEmpty) {
      java.lang.System.out.println("Empty");
    }

    val q2 = q + 42 + 0;

    val q4 =
      if(q2.isEmpty) {
        java.lang.System.out.println("Empty");
        q2;
      } else {
	  val Pair(head,q3) = q2.dequeue;
	  java.lang.System.out.println("Head: " + head);
	  q3;
      };

    val q5 = q4.enqueue(1,2,3,4,5,6,7,8,9);
    java.lang.System.out.println("q5: " + q5);
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    iq.main;
    ();
  }
}
