/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.automata

import scala.collection.{ mutable, immutable }

@deprecated("This class will be removed", "2.10.0")
class SubsetConstruction[T <: AnyRef](val nfa: NondetWordAutom[T]) {
  import nfa.labels

  def selectTag(Q: immutable.BitSet, finals: Array[Int]) =
    (Q map finals filter (_ > 0)).min

  def determinize: DetWordAutom[T] = {
    // for assigning numbers to bitsets
    var indexMap    = collection.Map[immutable.BitSet, Int]()
    var invIndexMap = collection.Map[Int, immutable.BitSet]()
    var ix = 0

    // we compute the dfa with states = bitsets
    val q0 = immutable.BitSet(0)            // the set { 0 }
    val sink = immutable.BitSet.empty       // the set { }

    var states = Set(q0, sink)    // initial set of sets
    val delta    = new mutable.HashMap[immutable.BitSet, mutable.HashMap[T, immutable.BitSet]]
    var deftrans = mutable.Map(q0 -> sink, sink -> sink)  // initial transitions
    var finals: mutable.Map[immutable.BitSet, Int]  = mutable.Map()
    val rest = new mutable.Stack[immutable.BitSet]

    rest.push(sink, q0)

    def addFinal(q: immutable.BitSet) {
      if (nfa containsFinal q)
        finals = finals.updated(q, selectTag(q, nfa.finals))
    }
    def add(Q: immutable.BitSet) {
      if (!states(Q)) {
        states += Q
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

      // make transition map
      val Pdelta = new mutable.HashMap[T, immutable.BitSet]
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
    val deltaR = new Array[mutable.Map[T, Int]](nstatesR)
    val defaultR = new Array[Int](nstatesR)
    val finalsR = new Array[Int](nstatesR)

    for (Q <- states) {
      val q = indexMap(Q)
      val trans = delta(Q)
      val transDef = deftrans(Q)
      val qDef = indexMap(transDef)
      val ntrans = new mutable.HashMap[T, Int]()

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
