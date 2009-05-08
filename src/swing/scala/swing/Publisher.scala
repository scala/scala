package scala.swing

import scala.collection.mutable._
import event.Event

/**
 * Notifies registered reactions when an event is published. Publishers are also
 * reactors and listen to themselves per default as a convenience.
 *
 * In order to reduce memory leaks, reactions are weakly referenced by default,
 * unless they implement <code>Reactions.StronglyReferenced</code>. That way,
 * the lifetime of reactions are more easily bound to the registering object,
 * which are reactors in common client code and hold strong references to their
 * reactions. As a result, reactors can be garbage collected even though they
 * still have reactions registered at some publisher, but not vice versa
 * since reactors (strongly) reference publishers they are interested in.
 */
trait Publisher extends Reactor {
  import Reactions._

  protected val listeners = new RefSet[Reaction] {
    import scala.ref._
    val underlying = new HashSet[Reference[Reaction]]
    protected def Ref(a: Reaction) = a match {
      case a: StronglyReferenced => new StrongReference[Reaction](a) with super.Ref[Reaction]
      case _ => new WeakReference[Reaction](a, referenceQueue) with super.Ref[Reaction]
    }
  }

  def subscribe(listener: Reaction) { listeners += listener }
  def unsubscribe(listener: Reaction) { listeners -= listener }

  /**
   * Notify all registered reactions.
   */
  def publish(e: Event) { for (l <- listeners) l(e) }

  listenTo(this)
}

private[swing] trait LazyPublisher extends Publisher {
  import Reactions._

  def onFirstSubscribe()
  def onLastUnsubscribe()

  override def subscribe(listener: Reaction) {
    if(listeners.size == 1) onFirstSubscribe()
    super.subscribe(listener)
  }
  override def unsubscribe(listener: Reaction) {
    super.unsubscribe(listener)
    if(listeners.size == 1) onLastUnsubscribe()
  }
}



import scala.ref._

private[swing] trait SingleRefCollection[+A <: AnyRef] extends Collection[A] { self =>

  trait Ref[+A <: AnyRef] extends Reference[A] {
    override def hashCode() = {
      val v = get
      if (v == None) 0 else v.get.hashCode
    }
    override def equals(that: Any) = that match {
      case that: ReferenceWrapper[_] =>
        val v1 = this.get
        val v2 = that.get
        v1 == v2
      case _ => false
    }
  }

  //type Ref <: Reference[A] // TODO: could use higher kinded types, but currently crashes
  protected[this] def Ref(a: A): Ref[A]
  protected[this] val referenceQueue = new ReferenceQueue[A]

  protected val underlying: Collection[Reference[A]]

  def purgeReferences() {
    var ref = referenceQueue.poll
    while (ref != None) {
      removeReference(ref.get.asInstanceOf[Reference[A]])
      ref = referenceQueue.poll
    }
  }

  protected[this] def removeReference(ref: Reference[A])

  def elements = new Iterator[A] {
    private val elems = self.underlying.elements
    private var hd: A = _
    private var ahead: Boolean = false
    private def skip: Unit =
      while (!ahead && elems.hasNext) {
        // make sure we have a reference to the next element,
        // otherwise it might be garbage collected
        val next = elems.next.get
        ahead = next != None
        if (ahead) hd = next.get
      }
    def hasNext: Boolean = { skip; ahead }
    def next(): A =
      if (hasNext) { ahead = false; hd }
      else throw new NoSuchElementException("next on empty iterator")
  }
}

private[swing] class StrongReference[+T <: AnyRef](value: T) extends Reference[T] {
    private[this] var ref: Option[T] = Some(value)
    @deprecated def isValid: Boolean = ref != None
    def apply(): T = ref.get
    def get : Option[T] = ref
    override def toString = get.map(_.toString).getOrElse("<deleted>")
    def clear() { ref = None }
    def enqueue(): Boolean = false
    def isEnqueued(): Boolean = false
  }

abstract class RefBuffer[A <: AnyRef] extends Buffer[A] with SingleRefCollection[A] { self =>
  protected val underlying: Buffer[Reference[A]]

  def +=(el: A) { purgeReferences(); underlying += Ref(el) }
  def +:(el: A) = { purgeReferences(); Ref(el) +: underlying; this }
  def remove(el: A) { underlying -= Ref(el); purgeReferences(); }
  def remove(n: Int) = { val el = apply(n); remove(el); el }
  def insertAll(n: Int, iter: Iterable[A]) {
    purgeReferences()
    underlying.insertAll(n, iter.projection.map(Ref(_)))
  }
  def update(n: Int, el: A) { purgeReferences(); underlying(n) = Ref(el) }
  def readOnly : Seq[A] = new Seq[A] {
    def length = self.length
    def elements = self.elements
    def apply(n: Int) = self(n)
  }
  def apply(n: Int) = {
    purgeReferences()
    var el = underlying(n).get
    while (el == None) {
      purgeReferences(); el = underlying(n).get
    }
    el.get
  }

  def length = { purgeReferences(); underlying.length }
  def clear() { underlying.clear(); purgeReferences() }

  protected[this] def removeReference(ref: Reference[A]) { underlying -= ref }
}

private[swing] abstract class RefSet[A <: AnyRef] extends Set[A] with SingleRefCollection[A] { self =>
  protected val underlying: Set[Reference[A]]

  def -=(el: A) { underlying -= Ref(el); purgeReferences() }
  def +=(el: A) { purgeReferences(); underlying += Ref(el) }
  def contains(el: A) = { purgeReferences(); underlying.contains(Ref(el)) }
  override def size = { purgeReferences(); underlying.size }

  protected[this] def removeReference(ref: Reference[A]) { underlying -= ref }
}
