/* $Id$
 * Test file for immutable queues.
 */

import scala.collection.immutable.Queue;

object iq {
  def main = {
    /* Create an empty queue. */
    val q:Queue[Int] = Queue.Empty;

    /* Test isEmpty.
     * Expected: Empty
     */
    if(q.isEmpty) {
      java.lang.System.out.println("Empty");
    }

    /* Test infix enqueing. */
    val q2 = q + 42 + 0;

    /* Test is empty and dequeue.
     * Expected: Head: 42
     */
    val q4 =
      if(q2.isEmpty) {
        java.lang.System.out.println("Empty");
        q2;
      } else {
	  val Pair(head,q3) = q2.dequeue;
	  java.lang.System.out.println("Head: " + head);
	  q3;
      };

    /* Test sequence enqueing. */
    val q5:Queue[Any] = q4.enqueue(1,2,3,4,5,6,7,8,9);
    /* Test toString.
     * Expected: Head: q5: Queue(0,1,2,3,4,5,6,7,8,9)
     */
    java.lang.System.out.println("q5: " + q5);
    /* Test apply
     * Expected: q5[5]: 5
     */
    java.lang.System.out.println("q5[5]: " + q5(5));



    val q5c:Queue[char] = Queue.Empty.enqueue(0: char, 1: char,
					      2: char, 3: char,
					      4: char, 5: char,
					      6: char, 7: char,
					      8: char, 9: char);

    /* Testing ==
     *  Expected: q5 == q9: true
     *            q9 == q5: true
     */
    java.lang.System.out.println("q5 == q5c: " + (q5 == q5c));
    java.lang.System.out.println("q5c == q5: " + (q5c == q5));

    val Pair(_,q6) = q5.dequeue;
    val Pair(_,q7) = q6.dequeue;
    val q8 = q7 + 10 + 11;
    /* Test dequeu
     * Expected: q8: Queue(2,3,4,5,6,7,8,9,10,11)
     */
    java.lang.System.out.println("q8: " + q8);
    val q9 = new Queue(2,3,4,5,6,7,8,9,10,11);

    /* Testing ==
     *  Expected: q8 == q9: true
     */
    java.lang.System.out.println("q8 == q9: " + (q8 == q9));

    /* Testing elements
     *  Expected: Elements:  1  2  3  4  5  6  7  8  9
     */
    java.lang.System.out.print("Elements: ");
    q6.elements.foreach(e => java.lang.System.out.print(" "+ e + " "));
    java.lang.System.out.println();

   /* Testing mkString
     *  Expected: String: <1-2-3-4-5-6-7-8-9>
     */
    java.lang.System.out.println("String: " + q6.mkString("<","-",">"));

    /* Testing length
     *  Expected: Length: 9
     */
    java.lang.System.out.println("Length: " + q6.length);

    /* Testing front
     *  Expected: Front: 1
     */
    java.lang.System.out.println("Front: " + q6.front);
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    iq.main;
    ();
  }
}
