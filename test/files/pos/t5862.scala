package test

import java.io.DataOutput
import java.io.DataInput

/** Interface for writing outputs from a DoFn. */
trait Emitter[A] {
  def emit(value: A): Unit
}

/** A wrapper for a 'map' function tagged for a specific output channel. */
abstract class TaggedMapper[A, K, V]
    (val tags: Set[Int])
    (implicit val mA: Manifest[A], val wtA: WireFormat[A],
              val mK: Manifest[K], val wtK: WireFormat[K], val ordK: Ordering[K],
              val mV: Manifest[V], val wtV: WireFormat[V])
  extends Serializable {
}

/** Type-class for sending types across the Hadoop wire. */
trait WireFormat[A]

class MapReduceJob {
  trait DataSource

  import scala.collection.mutable.{ Set => MSet, Map => MMap }
  private val mappers: MMap[DataSource, MSet[TaggedMapper[_, _, _]]] = MMap.empty

  def addTaggedMapper[A, K, V](input: DataSource, m: TaggedMapper[A, K, V]): Unit = {
    if (!mappers.contains(input))
      mappers += (input -> MSet(m))
    else
      mappers(input) += m // : Unit

    m.tags.foreach { tag =>
    }
  }
}
