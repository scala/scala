/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.util.automata

class SubsetConstruction[T <: AnyRef](val nfa: NondetWordAutom[T]) {
  import nfa.labels
  import collection.{ mutable, Map }
  import collection.immutable.BitSet

  def selectTag(Q: BitSet, finals: Array[Int]) =
    Q map finals filter (_ > 0) min

  def determinize: DetWordAutom[T] = {
    // for assigning numbers to bitsets
    var indexMap    = Map[BitSet, Int]()
    var invIndexMap = Map[Int, BitSet]()
    var ix = 0

    // we compute the dfa with states = bitsets
    val q0 = BitSet(0)            // the set { 0 }
    val sink = BitSet.empty       // the set { }

    var states = Set(q0, sink)    // initial set of sets
    val delta    = new mutable.HashMap[BitSet, mutable.HashMap[T, BitSet]]
    var deftrans = Map(q0 -> sink, sink -> sink)  // initial transitions
    var finals: Map[BitSet, Int]  = Map()

    val rest = new mutable.Stack[BitSet]
    rest.push(sink, q0)

    def addFinal(q: BitSet) {
      if (nfa containsFinal q)
        finals = finals.updated(q, selectTag(q, nfa.finals))
    }
    def add(Q: BitSet) {
      if (!states.contains(Q)) {
        states = states + Q
        rest push Q
        addFinal(Q)
      }
    }

    addFinal(q0)                          // initial state may also be a final state

    while (!rest.isEmpty) {
      val P = rest.pop
      // assign a number to this bitset
      indexMap = indexMap.updated(P, ix)
      invIndexMap = invIndexMap.updated(ix, P)
      ix += 1

      // make transitiion map
      val Pdelta = new mutable.HashMap[T, BitSet]
      delta.update(P, Pdelta)

      labels foreach { label =>
        val Q = nfa.next(P, label)
        Pdelta.update(label, Q)
        add(Q)
      }

      // collect default transitions
      val Pdef = nfa nextDefault P
      deftrans = deftrans.updated(P, Pdef)
      add(Pdef)
    }

    // create DetWordAutom, using indices instead of sets
    val nstatesR = states.size
    val deltaR = new Array[Map[T,Int]](nstatesR)
    val defaultR = new Array[Int](nstatesR)
    val finalsR = new Array[Int](nstatesR)

    for (Q <- states) {
      val q = indexMap(Q)
      val trans = delta(Q)
      val transDef = deftrans(Q)
      val qDef = indexMap(transDef)
      val ntrans = new mutable.HashMap[T,Int]()

      for ((label, value) <- trans) {
        val p = indexMap(value)
        if (p != qDef)
          ntrans.update(label, p)
      }

      deltaR(q) = ntrans
      defaultR(q) = qDef
    }

    finals foreach { case (k,v) => finalsR(indexMap(k)) = v }

    new DetWordAutom [T] {
      val nstates = nstatesR
      val delta = deltaR
      val default = defaultR
      val finals = finalsR
    }
  }
}
