/*
 * Test file for immutable queues.
 */

import scala.collection.immutable.Queue

object iq {
  def main {
    /* Create an empty queue. */
    val q: Queue[Int] = Queue.empty

    /* Test isEmpty.
     * Expected: Empty
     */
    if (q.isEmpty) {
      Console.println("Empty")
    }

    /* Test enqueing. */
    val q2 = q.enqueue(42).enqueue(0)
    val qa = q :+ 42 :+ 0
    assert(q2 == qa)
    
    val qb = 42 +: 0 +: q
    assert(q2 == qb)
    val qc = 42 +: q :+ 0
    assert(q2 == qc)

    Console.println("q2: " + q2)
    Console.println("qa: " + qa)
    Console.println("qb: " + qb)
    Console.println("qc: " + qc)
    
    /* Test is empty and dequeue.
     * Expected: Head: 42
     */
    val q4 =
      if (q2.isEmpty) {
        Console.println("Empty")
        q2
      }
      else {
	val (head, q3) = q2.dequeue
        Console.println("Head: " + head)
        q3
      }

    /* Test sequence enqueing. */
    val q5: Queue[Any] = q4.enqueue(List(1,2,3,4,5,6,7,8,9))
    /* Test toString.
     * Expected: q5: Queue(0,1,2,3,4,5,6,7,8,9)
     */
    Console.println("q5: " + q5)
    /* Test apply
     * Expected: q5[5]: 5
     */
    Console.println("q5[5]: " + q5(5))

    val q5c: Queue[Int] = Queue.empty.enqueue(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))

    /* Testing ==
     *  Expected: q5 == q9: true
     *            q9 == q5: true
     */
    Console.println("q5 == q5c: " + (q5 == q5c))
    Console.println("q5c == q5: " + (q5c == q5))

    val (_, q6) = q5.dequeue
    val (_, q7) = q6.dequeue
    //val q8 = q7 + 10 + 11  //deprecated
    val q8 = q7.enqueue(10).enqueue(11)
    /* Test dequeue
     * Expected: q8: Queue(2,3,4,5,6,7,8,9,10,11)
     */
    Console.println("q8: " + q8)
    val q9 = Queue(2,3,4,5,6,7,8,9,10,11)

    /* Testing ==
     *  Expected: q8 == q9: true
     */
    Console.println("q8 == q9: " + (q8 == q9))

    /* Testing elements
     *  Expected: Elements:  1  2  3  4  5  6  7  8  9
     */
    Console.print("Elements: ");
    q6.iterator.foreach(e => Console.print(" "+ e + " "))
    Console.println;

   /* Testing mkString
     *  Expected: String: <1-2-3-4-5-6-7-8-9>
     */
    Console.println("String: " + q6.mkString("<","-",">"))

    /* Testing length
     *  Expected: Length: 9
     */
    Console.println("Length: " + q6.length)

    /* Testing front
     *  Expected: Front: 1
     */
    Console.println("Front: " + q6.front);
  }
}

object Test {
  def main(args: Array[String]) {
    iq.main
  }
}
