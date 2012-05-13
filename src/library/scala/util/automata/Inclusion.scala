/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.util.automata


/** A fast test of language inclusion between minimal automata.
 *  inspired by the ''AMoRE automata library''.
 *
 *  @author Burak Emir
 *  @version 1.0
 */
@deprecated("This class will be removed", "2.10.0")
trait Inclusion[A <: AnyRef] {

  val labels: Seq[A]

  /** Returns true if `dfa1` is included in `dfa2`.
   */
  def inclusion(dfa1: DetWordAutom[A], dfa2: DetWordAutom[A]) = {

    def encode(q1: Int, q2: Int) = 1 + q1 + q2 * dfa1.nstates
    def decode2(c: Int) = (c-1) / (dfa1.nstates) //integer division
    def decode1(c: Int) = (c-1) % (dfa1.nstates)

    var q1 = 0 //dfa1.initstate; // == 0
    var q2 = 0 //dfa2.initstate; // == 0

    val max = 1 + dfa1.nstates * dfa2.nstates
    val mark = new Array[Int](max)

    var result = true
    var current = encode(q1, q2)
    var last = current
    mark(last) = max // mark (q1,q2)
    while (current != 0 && result) {
      //Console.println("current = [["+q1+" "+q2+"]] = "+current);
      for (letter <- labels) {
        val r1 = dfa1.next(q1,letter)
        val r2 = dfa2.next(q2,letter)
        if (dfa1.isFinal(r1) && !dfa2.isFinal(r2))
	  result = false
        val test = encode(r1, r2)
        //Console.println("test = [["+r1+" "+r2+"]] = "+test);
        if (mark(test) == 0) {
	  mark(last) = test
	  mark(test) = max
	  last = test
        }
      }
      val ncurrent = mark(current)
      if( ncurrent != max ) {
        q1 = decode1(ncurrent)
        q2 = decode2(ncurrent)
        current = ncurrent
      } else {
        current = 0
      }
    }
    result
  }
}
