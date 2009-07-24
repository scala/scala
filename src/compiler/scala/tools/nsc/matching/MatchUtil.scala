/* NSC -- new Scala compiler
 */

package scala.tools.nsc
package matching

/**
 *  Utility classes, most of which probably belong somewhere else.
 */
object MatchUtil
{
  import collection.mutable.ListBuffer

  def impossible:           Nothing = abort("this never happens")
  def abort(msg: String):   Nothing = throw new RuntimeException(msg)

  /** Transforms a list of triples into a triple of lists.
   *
   *  @param xs the list of triples to unzip
   *  @return a triple of lists.
   */
  def unzip3[A,B,C](xs: List[(A,B,C)]): (List[A], List[B], List[C]) = {
    val b1 = new ListBuffer[A]
    val b2 = new ListBuffer[B]
    val b3 = new ListBuffer[C]
    var xc = xs
    while (!xc.isEmpty) {
      b1 += xc.head._1
      b2 += xc.head._2
      b3 += xc.head._3
      xc = xc.tail
    }
    (b1.toList, b2.toList, b3.toList)
  }
}
