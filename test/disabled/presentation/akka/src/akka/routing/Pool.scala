/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka.routing

import akka.actor.{ Actor, ActorRef, PoisonPill }
import java.util.concurrent.TimeUnit

/**
 * Actor pooling
 *
 * An actor pool is an message router for a set of delegate actors. The pool is an actor itself.
 * There are a handful of basic concepts that need to be understood when working with and defining your pool.
 *
 * Selectors - A selector is a trait that determines how and how many pooled actors will receive an incoming message.
 * Capacitors - A capacitor is a trait that influences the size of pool.  There are effectively two types.
 *                              The first determines the size itself - either fixed or bounded.
 *                              The second determines how to adjust of the pool according to some internal pressure characteristic.
 * Filters - A filter can be used to refine the raw pressure value returned from a capacitor.
 *
 * It should be pointed out that all actors in the pool are treated as essentially equivalent.  This is not to say
 * that one couldn't instance different classes within the pool, only that the pool, when selecting and routing,
 * will not take any type information into consideration.
 *
 * @author Garrick Evans
 */

object ActorPool {
  case object Stat
  case class Stats(size: Int)
}

/**
 * Defines the nature of an actor pool.
 */
trait ActorPool {
  def instance(): ActorRef //Question, Instance of what?
  def capacity(delegates: Seq[ActorRef]): Int //Question, What is the semantics of this return value?
  def select(delegates: Seq[ActorRef]): Tuple2[Iterator[ActorRef], Int] //Question, Why does select return this instead of an ordered Set?
}

/**
 * A default implementation of a pool, on each message to route,
 *      - checks the current capacity and adjusts accordingly if needed
 *      - routes the incoming message to a selection set of delegate actors
 */
trait DefaultActorPool extends ActorPool { this: Actor =>
  import ActorPool._
  import collection.mutable.LinkedList
  import akka.actor.MaximumNumberOfRestartsWithinTimeRangeReached

  protected var _delegates = Vector[ActorRef]()
  private var _lastCapacityChange = 0
  private var _lastSelectorCount = 0

  override def postStop() = _delegates foreach { delegate =>
    try {
      delegate ! PoisonPill
    } catch { case e: Exception => } //Ignore any exceptions here
  }

  protected def _route(): Receive = {
    // for testing...
    case Stat =>
      self reply_? Stats(_delegates length)
    case max: MaximumNumberOfRestartsWithinTimeRangeReached =>
      _delegates = _delegates filterNot { _.uuid == max.victim.uuid }
    case msg =>
      resizeIfAppropriate()

      select(_delegates) match {
        case (selectedDelegates, count) =>
          _lastSelectorCount = count
          selectedDelegates foreach { _ forward msg } //Should we really send the same message to several actors?
      }
  }

  private def resizeIfAppropriate() {
    val requestedCapacity = capacity(_delegates)
    val newDelegates = requestedCapacity match {
      case qty if qty > 0 =>
        _delegates ++ {
          for (i ← 0 until requestedCapacity) yield {
            val delegate = instance()
            self startLink delegate
            delegate
          }
        }
      case qty if qty < 0 =>
        _delegates.splitAt(_delegates.length + requestedCapacity) match {
          case (keep, abandon) =>
            abandon foreach { _ ! PoisonPill }
            keep
        }
      case _ => _delegates //No change
    }

    _lastCapacityChange = requestedCapacity
    _delegates = newDelegates
  }
}

/**
 * Selectors
 *      These traits define how, when a message needs to be routed, delegate(s) are chosen from the pool
 */

/**
 * Returns the set of delegates with the least amount of message backlog.
 */
trait SmallestMailboxSelector {
  def selectionCount: Int
  def partialFill: Boolean

  def select(delegates: Seq[ActorRef]): Tuple2[Iterator[ActorRef], Int] = {
    var set: Seq[ActorRef] = Nil
    var take = if (partialFill) math.min(selectionCount, delegates.length) else selectionCount

    while (take > 0) {
      set = delegates.sortWith(_.mailboxSize < _.mailboxSize).take(take) ++ set //Question, doesn't this risk selecting the same actor multiple times?
      take -= set.size
    }

    (set.iterator, set.size)
  }
}

/**
 * Returns the set of delegates that occur sequentially 'after' the last delegate from the previous selection
 */
trait RoundRobinSelector {
  private var _last: Int = -1;

  def selectionCount: Int
  def partialFill: Boolean

  def select(delegates: Seq[ActorRef]): Tuple2[Iterator[ActorRef], Int] = {
    val length = delegates.length
    val take = if (partialFill) math.min(selectionCount, length)
    else selectionCount

    val set =
      for (i ← 0 until take) yield {
        _last = (_last + 1) % length
        delegates(_last)
      }

    (set.iterator, set.size)
  }
}

/**
 * Capacitors
 *      These traits define how to alter the size of the pool
 */

/**
 * Ensures a fixed number of delegates in the pool
 */
trait FixedSizeCapacitor {
  def limit: Int
  def capacity(delegates: Seq[ActorRef]): Int = (limit - delegates.size) max 0
}

/**
 * Constrains the pool capacity to a bounded range
 */
trait BoundedCapacitor {
  def lowerBound: Int
  def upperBound: Int

  def capacity(delegates: Seq[ActorRef]): Int = {
    val current = delegates length
    val delta = _eval(delegates)
    val proposed = current + delta

    if (proposed < lowerBound) delta + (lowerBound - proposed)
    else if (proposed > upperBound) delta - (proposed - upperBound)
    else delta
  }

  protected def _eval(delegates: Seq[ActorRef]): Int
}

/**
 * Returns the number of delegates required to manage the current message backlogs
 */
trait MailboxPressureCapacitor {
  def pressureThreshold: Int
  def pressure(delegates: Seq[ActorRef]): Int =
    delegates count { _.mailboxSize > pressureThreshold }
}

/**
 * Returns the number of delegates required to respond to the number of pending futures
 */
trait ActiveFuturesPressureCapacitor {
  def pressure(delegates: Seq[ActorRef]): Int =
    delegates count { _.senderFuture.isDefined }
}

/**
 */
trait CapacityStrategy {
  import ActorPool._

  def pressure(delegates: Seq[ActorRef]): Int
  def filter(pressure: Int, capacity: Int): Int

  protected def _eval(delegates: Seq[ActorRef]): Int = filter(pressure(delegates), delegates.size)
}

trait FixedCapacityStrategy extends FixedSizeCapacitor
trait BoundedCapacityStrategy extends CapacityStrategy with BoundedCapacitor

/**
 * Filters
 *  These traits refine the raw pressure reading into a more appropriate capacity delta.
 */

/**
 * The basic filter trait that composes ramp-up and back-off subfiltering.
 */
trait Filter {
  def rampup(pressure: Int, capacity: Int): Int
  def backoff(pressure: Int, capacity: Int): Int

  // pass through both filters just to be sure any internal counters
  // are updated consistently. ramping up is always + and backing off
  // is always - and each should return 0 otherwise...
  def filter(pressure: Int, capacity: Int): Int =
    rampup(pressure, capacity) + backoff(pressure, capacity)
}

trait BasicFilter extends Filter with BasicRampup with BasicBackoff

/**
 * Filter performs steady incremental growth using only the basic ramp-up subfilter
 */
trait BasicNoBackoffFilter extends BasicRampup {
  def filter(pressure: Int, capacity: Int): Int = rampup(pressure, capacity)
}

/**
 * Basic incremental growth as a percentage of the current pool capacity
 */
trait BasicRampup {
  def rampupRate: Double

  def rampup(pressure: Int, capacity: Int): Int =
    if (pressure < capacity) 0 else math.ceil(rampupRate * capacity) toInt
}

/**
 * Basic decrement as a percentage of the current pool capacity
 */
trait BasicBackoff {
  def backoffThreshold: Double
  def backoffRate: Double

  def backoff(pressure: Int, capacity: Int): Int =
    if (capacity > 0 && pressure / capacity < backoffThreshold) math.ceil(-1.0 * backoffRate * capacity) toInt else 0
}
/**
 * This filter tracks the average pressure over the lifetime of the pool (or since last reset) and
 * will begin to reduce capacity once this value drops below the provided threshold.  The number of
 * delegates to cull from the pool is determined by some scaling factor (the backoffRate) multiplied
 * by the difference in capacity and pressure.
 */
trait RunningMeanBackoff {
  def backoffThreshold: Double
  def backoffRate: Double

  private var _pressure: Double = 0.0
  private var _capacity: Double = 0.0

  def backoff(pressure: Int, capacity: Int): Int = {
    _pressure += pressure
    _capacity += capacity

    if (capacity > 0 && pressure / capacity < backoffThreshold
      && _capacity > 0 && _pressure / _capacity < backoffThreshold) //Why does the entire clause need to be true?
      math.floor(-1.0 * backoffRate * (capacity - pressure)).toInt
    else 0
  }

  def backoffReset {
    _pressure = 0.0
    _capacity = 0.0
  }
}
