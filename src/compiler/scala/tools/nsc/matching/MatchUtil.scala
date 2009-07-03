/* NSC -- new Scala compiler
 */

package scala.tools.nsc.matching

/**
 *  Utility classes, most of which probably belong somewhere else.
 */
object MatchUtil
{
  import collection.mutable.ListBuffer

  def impossible:           Nothing = abort("this never happens")
  def abort(msg: String):   Nothing = throw new RuntimeException(msg)

  object Implicits {
    implicit def listPlusOps[T](xs: List[T]) = new ListPlus(xs)
  }

  class ListPlus[A](list: List[A]) {
    /** Returns the list without the element at index <code>n</code>.
     *  If this list has fewer than <code>n</code> elements, the same list is returned.
     *
     * @param n the index of the element to drop.
     * @return the list without the <code>n</code>th element.
     */
    def dropIndex(n: Int) = list.take(n) ::: list.drop(n + 1)

    /** Returns a list formed from this list and the specified lists <code>list2</code>
     *  and <code>list3</code> by associating each element of the first list with
     *  the elements at the same positions in the other two.
     *  If any of the lists is shorter than the others, later elements in the other two are ignored.
     *
     *  @return     <code>List((a<sub>0</sub>,b<sub>0</sub>), ...,
     *              (a<sub>min(m,n)</sub>,b<sub>min(m,n)</sub>))</code> when
     *              <code>List(a<sub>0</sub>, ..., a<sub>m</sub>)
     *              zip List(b<sub>0</sub>, ..., b<sub>n</sub>)</code> is invoked.
     */
    def zip3[B, C](list2: List[B], list3: List[C]): List[(A, B, C)] = {
      val b = new ListBuffer[(A, B, C)]
      var xs1 = list
      var xs2 = list2
      var xs3 = list3
      while (!xs1.isEmpty && !xs2.isEmpty && !xs3.isEmpty) {
        b += ((xs1.head, xs2.head, xs3.head))
        xs1 = xs1.tail
        xs2 = xs2.tail
        xs3 = xs3.tail
      }
      b.toList
    }
  }

  object ListPlus {
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
}
