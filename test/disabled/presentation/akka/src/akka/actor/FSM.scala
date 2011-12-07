/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */
package akka.actor

import akka.util._
import akka.event.EventHandler

import scala.collection.mutable
import java.util.concurrent.ScheduledFuture

object FSM {

  object NullFunction extends PartialFunction[Any, Nothing] {
    def isDefinedAt(o: Any) = false
    def apply(o: Any) = sys.error("undefined")
  }

  case class CurrentState[S](fsmRef: ActorRef, state: S)
  case class Transition[S](fsmRef: ActorRef, from: S, to: S)
  case class SubscribeTransitionCallBack(actorRef: ActorRef)
  case class UnsubscribeTransitionCallBack(actorRef: ActorRef)

  sealed trait Reason
  case object Normal extends Reason
  case object Shutdown extends Reason
  case class Failure(cause: Any) extends Reason

  case object StateTimeout
  case class TimeoutMarker(generation: Long)

  case class Timer(name: String, msg: AnyRef, repeat: Boolean, generation: Int) {
    private var ref: Option[ScheduledFuture[AnyRef]] = _

    def schedule(actor: ActorRef, timeout: Duration) {
      if (repeat) {
        ref = Some(Scheduler.schedule(actor, this, timeout.length, timeout.length, timeout.unit))
      } else {
        ref = Some(Scheduler.scheduleOnce(actor, this, timeout.length, timeout.unit))
      }
    }

    def cancel {
      if (ref.isDefined) {
        ref.get.cancel(true)
        ref = None
      }
    }
  }

  /*
   * This extractor is just convenience for matching a (S, S) pair, including a
   * reminder what the new state is.
   */
  object -> {
    def unapply[S](in: (S, S)) = Some(in)
  }

  /*
  * With these implicits in scope, you can write "5 seconds" anywhere a
  * Duration or Option[Duration] is expected. This is conveniently true
  * for derived classes.
  */
  implicit def d2od(d: Duration): Option[Duration] = Some(d)
}

/**
 * Finite State Machine actor trait. Use as follows:
 *
 * <pre>
 *   object A {
 *     trait State
 *     case class One extends State
 *     case class Two extends State
 *
 *     case class Data(i : Int)
 *   }
 *
 *   class A extends Actor with FSM[A.State, A.Data] {
 *     import A._
 *
 *     startWith(One, Data(42))
 *     when(One) {
 *         case Event(SomeMsg, Data(x)) => ...
 *         case Ev(SomeMsg) => ... // convenience when data not needed
 *     }
 *     when(Two, stateTimeout = 5 seconds) { ... }
 *     initialize
 *   }
 * </pre>
 *
 * Within the partial function the following values are returned for effecting
 * state transitions:
 *
 *  - <code>stay</code> for staying in the same state
 *  - <code>stay using Data(...)</code> for staying in the same state, but with
 *    different data
 *  - <code>stay forMax 5.millis</code> for staying with a state timeout; can be
 *    combined with <code>using</code>
 *  - <code>goto(...)</code> for changing into a different state; also supports
 *    <code>using</code> and <code>forMax</code>
 *  - <code>stop</code> for terminating this FSM actor
 *
 * Each of the above also supports the method <code>replying(AnyRef)</code> for
 * sending a reply before changing state.
 *
 * While changing state, custom handlers may be invoked which are registered
 * using <code>onTransition</code>. This is meant to enable concentrating
 * different concerns in different places; you may choose to use
 * <code>when</code> for describing the properties of a state, including of
 * course initiating transitions, but you can describe the transitions using
 * <code>onTransition</code> to avoid having to duplicate that code among
 * multiple paths which lead to a transition:
 *
 * <pre>
 * onTransition {
 *   case Active -&gt; _ =&gt; cancelTimer("activeTimer")
 * }
 * </pre>
 *
 * Multiple such blocks are supported and all of them will be called, not only
 * the first matching one.
 *
 * Another feature is that other actors may subscribe for transition events by
 * sending a <code>SubscribeTransitionCallback</code> message to this actor;
 * use <code>UnsubscribeTransitionCallback</code> before stopping the other
 * actor.
 *
 * State timeouts set an upper bound to the time which may pass before another
 * message is received in the current state. If no external message is
 * available, then upon expiry of the timeout a StateTimeout message is sent.
 * Note that this message will only be received in the state for which the
 * timeout was set and that any message received will cancel the timeout
 * (possibly to be started again by the next transition).
 *
 * Another feature is the ability to install and cancel single-shot as well as
 * repeated timers which arrange for the sending of a user-specified message:
 *
 * <pre>
 *   setTimer("tock", TockMsg, 1 second, true) // repeating
 *   setTimer("lifetime", TerminateMsg, 1 hour, false) // single-shot
 *   cancelTimer("tock")
 *   timerActive_? ("tock")
 * </pre>
 */
trait FSM[S, D] extends ListenerManagement {
  this: Actor =>

  import FSM._

  type StateFunction = scala.PartialFunction[Event[D], State]
  type Timeout = Option[Duration]
  type TransitionHandler = PartialFunction[(S, S), Unit]

  /**
   * ****************************************
   *                 DSL
   * ****************************************
   */

  /**
   * Insert a new StateFunction at the end of the processing chain for the
   * given state. If the stateTimeout parameter is set, entering this state
   * without a differing explicit timeout setting will trigger a StateTimeout
   * event; the same is true when using #stay.
   *
   * @param stateName designator for the state
   * @param stateTimeout default state timeout for this state
   * @param stateFunction partial function describing response to input
   */
  protected final def when(stateName: S, stateTimeout: Timeout = None)(stateFunction: StateFunction) = {
    register(stateName, stateFunction, stateTimeout)
  }

  /**
   * Set initial state. Call this method from the constructor before the #initialize method.
   *
   * @param stateName initial state designator
   * @param stateData initial state data
   * @param timeout state timeout for the initial state, overriding the default timeout for that state
   */
  protected final def startWith(stateName: S,
                                stateData: D,
                                timeout: Timeout = None) = {
    currentState = State(stateName, stateData, timeout)
  }

  /**
   * Produce transition to other state. Return this from a state function in
   * order to effect the transition.
   *
   * @param nextStateName state designator for the next state
   * @return state transition descriptor
   */
  protected final def goto(nextStateName: S): State = {
    State(nextStateName, currentState.stateData)
  }

  /**
   * Produce "empty" transition descriptor. Return this from a state function
   * when no state change is to be effected.
   *
   * @return descriptor for staying in current state
   */
  protected final def stay(): State = {
    // cannot directly use currentState because of the timeout field
    goto(currentState.stateName)
  }

  /**
   * Produce change descriptor to stop this FSM actor with reason "Normal".
   */
  protected final def stop(): State = {
    stop(Normal)
  }

  /**
   * Produce change descriptor to stop this FSM actor including specified reason.
   */
  protected final def stop(reason: Reason): State = {
    stop(reason, currentState.stateData)
  }

  /**
   * Produce change descriptor to stop this FSM actor including specified reason.
   */
  protected final def stop(reason: Reason, stateData: D): State = {
    stay using stateData withStopReason (reason)
  }

  /**
   * Schedule named timer to deliver message after given delay, possibly repeating.
   * @param name identifier to be used with cancelTimer()
   * @param msg message to be delivered
   * @param timeout delay of first message delivery and between subsequent messages
   * @param repeat send once if false, scheduleAtFixedRate if true
   * @return current state descriptor
   */
  protected final def setTimer(name: String, msg: AnyRef, timeout: Duration, repeat: Boolean): State = {
    if (timers contains name) {
      timers(name).cancel
    }
    val timer = Timer(name, msg, repeat, timerGen.next)
    timer.schedule(self, timeout)
    timers(name) = timer
    stay
  }

  /**
   * Cancel named timer, ensuring that the message is not subsequently delivered (no race).
   * @param name of the timer to cancel
   */
  protected final def cancelTimer(name: String) = {
    if (timers contains name) {
      timers(name).cancel
      timers -= name
    }
  }

  /**
   * Inquire whether the named timer is still active. Returns true unless the
   * timer does not exist, has previously been canceled or if it was a
   * single-shot timer whose message was already received.
   */
  protected final def timerActive_?(name: String) = timers contains name

  /**
   * Set state timeout explicitly. This method can safely be used from within a
   * state handler.
   */
  protected final def setStateTimeout(state: S, timeout: Timeout) {
    stateTimeouts(state) = timeout
  }

  /**
   * Set handler which is called upon each state transition, i.e. not when
   * staying in the same state. This may use the pair extractor defined in the
   * FSM companion object like so:
   *
   * <pre>
   * onTransition {
   *   case Old -&gt; New =&gt; doSomething
   * }
   * </pre>
   *
   * It is also possible to supply a 2-ary function object:
   *
   * <pre>
   * onTransition(handler _)
   *
   * private def handler(from: S, to: S) { ... }
   * </pre>
   *
   * The underscore is unfortunately necessary to enable the nicer syntax shown
   * above (it uses the implicit conversion total2pf under the hood).
   *
   * <b>Multiple handlers may be installed, and every one of them will be
   * called, not only the first one matching.</b>
   */
  protected final def onTransition(transitionHandler: TransitionHandler) {
    transitionEvent :+= transitionHandler
  }

  /**
   * Convenience wrapper for using a total function instead of a partial
   * function literal. To be used with onTransition.
   */
  implicit protected final def total2pf(transitionHandler: (S, S) => Unit) =
    new PartialFunction[(S, S), Unit] {
      def isDefinedAt(in: (S, S)) = true
      def apply(in: (S, S)) { transitionHandler(in._1, in._2) }
    }

  /**
   * Set handler which is called upon termination of this FSM actor.
   */
  protected final def onTermination(terminationHandler: PartialFunction[StopEvent[S, D], Unit]) = {
    terminateEvent = terminationHandler
  }

  /**
   * Set handler which is called upon reception of unhandled messages.
   */
  protected final def whenUnhandled(stateFunction: StateFunction) = {
    handleEvent = stateFunction orElse handleEventDefault
  }

  /**
   * Verify existence of initial state and setup timers. This should be the
   * last call within the constructor.
   */
  def initialize {
    makeTransition(currentState)
  }

  /**
   * ****************************************************************
   *                PRIVATE IMPLEMENTATION DETAILS
   * ****************************************************************
   */

  /*
   * FSM State data and current timeout handling
   */
  private var currentState: State = _
  private var timeoutFuture: Option[ScheduledFuture[AnyRef]] = None
  private var generation: Long = 0L

  /*
   * Timer handling
   */
  private val timers = mutable.Map[String, Timer]()
  private val timerGen = Iterator from 0

  /*
   * State definitions
   */
  private val stateFunctions = mutable.Map[S, StateFunction]()
  private val stateTimeouts = mutable.Map[S, Timeout]()

  private def register(name: S, function: StateFunction, timeout: Timeout) {
    if (stateFunctions contains name) {
      stateFunctions(name) = stateFunctions(name) orElse function
      stateTimeouts(name) = timeout orElse stateTimeouts(name)
    } else {
      stateFunctions(name) = function
      stateTimeouts(name) = timeout
    }
  }

  /*
   * unhandled event handler
   */
  private val handleEventDefault: StateFunction = {
    case Event(value, stateData) =>
      stay
  }
  private var handleEvent: StateFunction = handleEventDefault

  /*
   * termination handling
   */
  private var terminateEvent: PartialFunction[StopEvent[S, D], Unit] = {
    case StopEvent(Failure(cause), _, _) =>
    case StopEvent(reason, _, _)         =>
  }

  /*
   * transition handling
   */
  private var transitionEvent: List[TransitionHandler] = Nil
  private def handleTransition(prev: S, next: S) {
    val tuple = (prev, next)
    for (te ← transitionEvent) { if (te.isDefinedAt(tuple)) te(tuple) }
  }

  // ListenerManagement shall not start() or stop() listener actors
  override protected val manageLifeCycleOfListeners = false

  /**
   * *******************************************
   *       Main actor receive() method
   * *******************************************
   */
  override final protected def receive: Receive = {
    case TimeoutMarker(gen) =>
      if (generation == gen) {
        processEvent(StateTimeout)
      }
    case t@Timer(name, msg, repeat, generation) =>
      if ((timers contains name) && (timers(name).generation == generation)) {
        processEvent(msg)
        if (!repeat) {
          timers -= name
        }
      }
    case SubscribeTransitionCallBack(actorRef) =>
      addListener(actorRef)
      // send current state back as reference point
      try {
        actorRef ! CurrentState(self, currentState.stateName)
      } catch {
        case e: ActorInitializationException =>
          EventHandler.warning(this, "trying to register not running listener")
      }
    case UnsubscribeTransitionCallBack(actorRef) =>
      removeListener(actorRef)
    case value => {
      if (timeoutFuture.isDefined) {
        timeoutFuture.get.cancel(true)
        timeoutFuture = None
      }
      generation += 1
      processEvent(value)
    }
  }

  private def processEvent(value: Any) = {
    val event = Event(value, currentState.stateData)
    val stateFunc = stateFunctions(currentState.stateName)
    val nextState = if (stateFunc isDefinedAt event) {
      stateFunc(event)
    } else {
      // handleEventDefault ensures that this is always defined
      handleEvent(event)
    }
    nextState.stopReason match {
      case Some(reason) => terminate(reason)
      case None         => makeTransition(nextState)
    }
  }

  private def makeTransition(nextState: State) = {
    if (!stateFunctions.contains(nextState.stateName)) {
      terminate(Failure("Next state %s does not exist".format(nextState.stateName)))
    } else {
      if (currentState.stateName != nextState.stateName) {
        handleTransition(currentState.stateName, nextState.stateName)
        notifyListeners(Transition(self, currentState.stateName, nextState.stateName))
      }
      applyState(nextState)
    }
  }

  private def applyState(nextState: State) = {
    currentState = nextState
    val timeout = if (currentState.timeout.isDefined) currentState.timeout else stateTimeouts(currentState.stateName)
    if (timeout.isDefined) {
      val t = timeout.get
      if (t.finite_? && t.length >= 0) {
        timeoutFuture = Some(Scheduler.scheduleOnce(self, TimeoutMarker(generation), t.length, t.unit))
      }
    }
  }

  private def terminate(reason: Reason) = {
    terminateEvent.apply(StopEvent(reason, currentState.stateName, currentState.stateData))
    self.stop()
  }

  case class Event[D](event: Any, stateData: D)
  object Ev {
    def unapply[D](e: Event[D]): Option[Any] = Some(e.event)
  }

  case class State(stateName: S, stateData: D, timeout: Timeout = None) {

    /**
     * Modify state transition descriptor to include a state timeout for the
     * next state. This timeout overrides any default timeout set for the next
     * state.
     */
    def forMax(timeout: Duration): State = {
      copy(timeout = Some(timeout))
    }

    /**
     * Send reply to sender of the current message, if available.
     *
     * @return this state transition descriptor
     */
    def replying(replyValue: Any): State = {
      self.sender match {
        case Some(sender) => sender ! replyValue
        case None         =>
      }
      this
    }

    /**
     * Modify state transition descriptor with new state data. The data will be
     * set when transitioning to the new state.
     */
    def using(nextStateDate: D): State = {
      copy(stateData = nextStateDate)
    }

    private[akka] var stopReason: Option[Reason] = None

    private[akka] def withStopReason(reason: Reason): State = {
      stopReason = Some(reason)
      this
    }
  }

  case class StopEvent[S, D](reason: Reason, currentState: S, stateData: D)
}
