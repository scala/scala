/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.automata


class SubsetConstruction[T <: AnyRef](val nfa: NondetWordAutom[T]) {
  import nfa.labels
  import scala.collection.{immutable, mutable, Map}
  import immutable.BitSet

  implicit def toOrdered(bs: BitSet): Ordered[BitSet] = new Ordered[BitSet] {
    def compare(that: BitSet): Int = {
        val it1 = bs.elements
        val it2 = that.elements
        var res = 0
        while((0 == res) && it1.hasNext) {
          while((0 == res) && it2.hasNext) {
            if (!it1.hasNext)
              res = -1
            else {
              val i1 = it1.next
              val i2 = it2.next
              if (i1 < i2)
                res = -1
              else if (i1 > i2)
                res = 1
            }
          }
          if (it1.hasNext)
            res = 1
        }
        if (it2.hasNext)
          res = -1
        res
    }

    override def equals(other: Any): Boolean =
        other match { case that: BitSet => compare(that) == 0
                      case that: AnyRef => this.eq(that)
                      case _ => false }

      //case _ => -(other.compare(this))
  }

  /** the set {0} */
  final val _initialBitSet = {
    val rbs = new mutable.BitSet(1)
    rbs += 0
    rbs.toImmutable
  }

  /** the set {} */
  final val _sinkBitSet = new mutable.BitSet(1).toImmutable

  final val _emptyBitSet = new scala.collection.mutable.BitSet(1).toImmutable

  def selectTag(Q: BitSet, finals: Array[Int]) = {
    val it = Q.elements
    var mintag = Math.MAX_INT
    while (it.hasNext) {
      val tag = finals(it.next)
      if ((0 < tag) && (tag < mintag))
        mintag = tag
    }
    mintag
  }

  def determinize: DetWordAutom[T] = {

    // for assigning numbers to bitsets
    var indexMap    = Map[BitSet, Int]()
    var invIndexMap = Map[Int, BitSet]()
    var ix = 0

    // we compute the dfa with states = bitsets
    var states   = Set[BitSet]()
    val delta    = new mutable.HashMap[BitSet,
                                       mutable.HashMap[T, BitSet]]
    var deftrans = Map[BitSet, BitSet]()
    var finals   = Map[BitSet, Int]()

    val q0 = _initialBitSet
    states = states + q0

    val sink = _emptyBitSet
    states = states + sink

    deftrans = deftrans.add(q0,sink);
    deftrans = deftrans.add(sink,sink);

    val rest = new mutable.ArrayStack[BitSet]();

    def add(Q: BitSet) {
      if (!states.contains(Q)) {
        states = states + Q
        rest.push(Q)
        if (nfa.containsFinal(Q))
          finals = finals.add(Q, selectTag(Q,nfa.finals));
      }
    }
    rest.push(sink)
    val sinkIndex = 1
    rest.push(q0)
    while (!rest.isEmpty) {
      // assign a number to this bitset
      val P = rest.pop
      indexMap = indexMap.add(P,ix)
      invIndexMap = invIndexMap.add(ix,P)
      ix += 1

      // make transitiion map
      val Pdelta = new mutable.HashMap[T, BitSet]
      delta.add(P, Pdelta)

      val it = labels.elements; while(it.hasNext) {
        val label = it.next
        val Q = nfa.next(P,label)
	Pdelta.add(label, Q)
        add(Q)
      }

      // collect default transitions
      val Pdef = nfa.nextDefault(P)
      deftrans = deftrans.add(P, Pdef)
      add(Pdef)
    };

    // create DetWordAutom, using indices instead of sets
    val nstatesR = states.size
    val deltaR = new Array[Map[T,Int]](nstatesR)
    val defaultR = new Array[Int](nstatesR)
    val finalsR = new Array[Int](nstatesR)

    for (w <- states) {
      val Q = w
      val q = indexMap(Q)
      val trans = delta(Q)
      val transDef = deftrans(Q)
      val qDef = indexMap(transDef)
      val ntrans = new mutable.HashMap[T,Int]()
      val it = trans.keys; while(it.hasNext) {
        val label = it.next
        val p = indexMap(trans(label))
        if (p != qDef)
          ntrans.add(label, p)
      }
      deltaR(q) = ntrans
      defaultR(q) = qDef

      //cleanup? leave to garbage collector?
      //delta.remove(Q);
      //default.remove(Q);

    }

    for (fQ <- finals.keys) finalsR(indexMap(fQ)) = finals(fQ)

    new DetWordAutom [T] {
      //type _labelT = SubsetConstruction.this.nfa._labelT;
      val nstates = nstatesR
      val delta = deltaR
      val default = defaultR
      val finals = finalsR
    }
  }
}
