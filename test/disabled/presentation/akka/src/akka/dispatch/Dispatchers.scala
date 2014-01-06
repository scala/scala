/**
 *   Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka.dispatch

import akka.actor.{ Actor, ActorRef }
import akka.actor.newUuid
import akka.config.Config._
import akka.util.{ Duration, ReflectiveAccess }

import akka.config.Configuration

import java.util.concurrent.TimeUnit

/**
 * Scala API. Dispatcher factory.
 * <p/>
 * Example usage:
 * <pre/>
 *   val dispatcher = Dispatchers.newExecutorBasedEventDrivenDispatcher("name")
 *   dispatcher
 *     .withNewThreadPoolWithLinkedBlockingQueueWithCapacity(100)
 *     .setCorePoolSize(16)
 *     .setMaxPoolSize(128)
 *     .setKeepAliveTimeInMillis(60000)
 *     .setRejectionPolicy(new CallerRunsPolicy)
 *     .build
 * </pre>
 * <p/>
 * Java API. Dispatcher factory.
 * <p/>
 * Example usage:
 * <pre/>
 *   MessageDispatcher dispatcher = Dispatchers.newExecutorBasedEventDrivenDispatcher("name");
 *   dispatcher
 *     .withNewThreadPoolWithLinkedBlockingQueueWithCapacity(100)
 *     .setCorePoolSize(16)
 *     .setMaxPoolSize(128)
 *     .setKeepAliveTimeInMillis(60000)
 *     .setRejectionPolicy(new CallerRunsPolicy())
 *     .build();
 * </pre>
 * <p/>
 *
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
object Dispatchers {
  val THROUGHPUT = config.getInt("akka.actor.throughput", 5)
  val DEFAULT_SHUTDOWN_TIMEOUT = config.getLong("akka.actor.dispatcher-shutdown-timeout").
    map(time => Duration(time, TIME_UNIT)).
    getOrElse(Duration(1000, TimeUnit.MILLISECONDS))
  val MAILBOX_CAPACITY = config.getInt("akka.actor.default-dispatcher.mailbox-capacity", -1)
  val MAILBOX_PUSH_TIME_OUT = Duration(config.getInt("akka.actor.default-dispatcher.mailbox-push-timeout-time", 10), TIME_UNIT)
  val THROUGHPUT_DEADLINE_TIME = Duration(config.getInt("akka.actor.throughput-deadline-time", -1), TIME_UNIT)
  val THROUGHPUT_DEADLINE_TIME_MILLIS = THROUGHPUT_DEADLINE_TIME.toMillis.toInt
  val MAILBOX_TYPE: MailboxType = if (MAILBOX_CAPACITY < 1) UnboundedMailbox() else BoundedMailbox()

  lazy val defaultGlobalDispatcher = {
    config.getSection("akka.actor.default-dispatcher").flatMap(from).getOrElse(globalExecutorBasedEventDrivenDispatcher)
  }

  object globalExecutorBasedEventDrivenDispatcher extends ExecutorBasedEventDrivenDispatcher("global", THROUGHPUT, THROUGHPUT_DEADLINE_TIME_MILLIS, MAILBOX_TYPE)

  /**
   * Creates an thread based dispatcher serving a single actor through the same single thread.
   * Uses the default timeout
   * <p/>
   * E.g. each actor consumes its own thread.
   */
  def newThreadBasedDispatcher(actor: ActorRef) = new ThreadBasedDispatcher(actor)

  /**
   * Creates an thread based dispatcher serving a single actor through the same single thread.
   * Uses the default timeout
   * If capacity is negative, it's Integer.MAX_VALUE
   * <p/>
   * E.g. each actor consumes its own thread.
   */
  def newThreadBasedDispatcher(actor: ActorRef, mailboxCapacity: Int) = new ThreadBasedDispatcher(actor, mailboxCapacity)

  /**
   * Creates an thread based dispatcher serving a single actor through the same single thread.
   * If capacity is negative, it's Integer.MAX_VALUE
   * <p/>
   * E.g. each actor consumes its own thread.
   */
  def newThreadBasedDispatcher(actor: ActorRef, mailboxCapacity: Int, pushTimeOut: Duration) =
    new ThreadBasedDispatcher(actor, mailboxCapacity, pushTimeOut)

  /**
   * Creates an executor-based event-driven dispatcher serving multiple (millions) of actors through a thread pool.
   * <p/>
   * Has a fluent builder interface for configuring its semantics.
   */
  def newExecutorBasedEventDrivenDispatcher(name: String) =
    ThreadPoolConfigDispatcherBuilder(config => new ExecutorBasedEventDrivenDispatcher(name, config), ThreadPoolConfig())

  /**
   * Creates an executor-based event-driven dispatcher serving multiple (millions) of actors through a thread pool.
   * <p/>
   * Has a fluent builder interface for configuring its semantics.
   */
  def newExecutorBasedEventDrivenDispatcher(name: String, throughput: Int, mailboxType: MailboxType) =
    ThreadPoolConfigDispatcherBuilder(config =>
      new ExecutorBasedEventDrivenDispatcher(name, throughput, THROUGHPUT_DEADLINE_TIME_MILLIS, mailboxType, config), ThreadPoolConfig())

  /**
   * Creates an executor-based event-driven dispatcher serving multiple (millions) of actors through a thread pool.
   * <p/>
   * Has a fluent builder interface for configuring its semantics.
   */
  def newExecutorBasedEventDrivenDispatcher(name: String, throughput: Int, throughputDeadlineMs: Int, mailboxType: MailboxType) =
    ThreadPoolConfigDispatcherBuilder(config =>
      new ExecutorBasedEventDrivenDispatcher(name, throughput, throughputDeadlineMs, mailboxType, config), ThreadPoolConfig())

  /**
   * Creates an executor-based event-driven dispatcher, with work-stealing, serving multiple (millions) of actors through a thread pool.
   * <p/>
   * Has a fluent builder interface for configuring its semantics.
   */
  def newExecutorBasedEventDrivenWorkStealingDispatcher(name: String) =
    ThreadPoolConfigDispatcherBuilder(config => new ExecutorBasedEventDrivenWorkStealingDispatcher(name, config), ThreadPoolConfig())

  /**
   * Creates an executor-based event-driven dispatcher, with work-stealing, serving multiple (millions) of actors through a thread pool.
   * <p/>
   * Has a fluent builder interface for configuring its semantics.
   */
  def newExecutorBasedEventDrivenWorkStealingDispatcher(name: String, throughput: Int) =
    ThreadPoolConfigDispatcherBuilder(config =>
      new ExecutorBasedEventDrivenWorkStealingDispatcher(name, throughput, THROUGHPUT_DEADLINE_TIME_MILLIS, MAILBOX_TYPE, config), ThreadPoolConfig())

  /**
   * Creates an executor-based event-driven dispatcher, with work-stealing, serving multiple (millions) of actors through a thread pool.
   * <p/>
   * Has a fluent builder interface for configuring its semantics.
   */
  def newExecutorBasedEventDrivenWorkStealingDispatcher(name: String, throughput: Int, mailboxType: MailboxType) =
    ThreadPoolConfigDispatcherBuilder(config =>
      new ExecutorBasedEventDrivenWorkStealingDispatcher(name, throughput, THROUGHPUT_DEADLINE_TIME_MILLIS, mailboxType, config), ThreadPoolConfig())

  /**
   * Creates an executor-based event-driven dispatcher, with work-stealing, serving multiple (millions) of actors through a thread pool.
   * <p/>
   * Has a fluent builder interface for configuring its semantics.
   */
  def newExecutorBasedEventDrivenWorkStealingDispatcher(name: String, throughput: Int, throughputDeadlineMs: Int, mailboxType: MailboxType) =
    ThreadPoolConfigDispatcherBuilder(config =>
      new ExecutorBasedEventDrivenWorkStealingDispatcher(name, throughput, throughputDeadlineMs, mailboxType, config), ThreadPoolConfig())
  /**
   * Utility function that tries to load the specified dispatcher config from the akka.conf
   * or else use the supplied default dispatcher
   */
  def fromConfig(key: String, default: => MessageDispatcher = defaultGlobalDispatcher): MessageDispatcher =
    config getSection key flatMap from getOrElse default

  /*
   * Creates of obtains a dispatcher from a ConfigMap according to the format below
   *
   * default-dispatcher {
   *   type = "GlobalExecutorBasedEventDriven" # Must be one of the following, all "Global*" are non-configurable
   *                               # (ExecutorBasedEventDrivenWorkStealing), ExecutorBasedEventDriven,
   *                               # GlobalExecutorBasedEventDriven
   *                               # A FQCN to a class inheriting MessageDispatcherConfigurator with a no-arg visible constructor
   *   keep-alive-time = 60        # Keep alive time for threads
   *   core-pool-size-factor = 1.0 # No of core threads ... ceil(available processors * factor)
   *   max-pool-size-factor  = 4.0 # Max no of threads ... ceil(available processors * factor)
   *   executor-bounds = -1        # Makes the Executor bounded, -1 is unbounded
   *   allow-core-timeout = on     # Allow core threads to time out
   *   rejection-policy = "caller-runs" # abort, caller-runs, discard-oldest, discard
   *   throughput = 5              # Throughput for ExecutorBasedEventDrivenDispatcher
   * }
   * ex: from(config.getConfigMap(identifier).get)
   *
   * Gotcha: Only configures the dispatcher if possible
   * Returns: None if "type" isn't specified in the config
   * Throws: IllegalArgumentException if the value of "type" is not valid
   *         IllegalArgumentException if it cannot
   */
  def from(cfg: Configuration): Option[MessageDispatcher] = {
    cfg.getString("type") map {
      case "ExecutorBasedEventDriven"             => new ExecutorBasedEventDrivenDispatcherConfigurator()
      case "ExecutorBasedEventDrivenWorkStealing" => new ExecutorBasedEventDrivenWorkStealingDispatcherConfigurator()
      case "GlobalExecutorBasedEventDriven"       => GlobalExecutorBasedEventDrivenDispatcherConfigurator
      case fqn =>
        ReflectiveAccess.getClassFor[MessageDispatcherConfigurator](fqn) match {
          case r: Right[_, Class[MessageDispatcherConfigurator]] =>
            ReflectiveAccess.createInstance[MessageDispatcherConfigurator](r.b, Array[Class[_]](), Array[AnyRef]()) match {
              case r: Right[Exception, MessageDispatcherConfigurator] => r.b
              case l: Left[Exception, MessageDispatcherConfigurator] =>
                throw new IllegalArgumentException("Cannot instantiate MessageDispatcherConfigurator type [%s], make sure it has a default no-args constructor" format fqn, l.a)
            }
          case l: Left[Exception, _] =>
            throw new IllegalArgumentException("Unknown MessageDispatcherConfigurator type [%s]" format fqn, l.a)
        }
    } map {
      _ configure cfg
    }
  }
}

object GlobalExecutorBasedEventDrivenDispatcherConfigurator extends MessageDispatcherConfigurator {
  def configure(config: Configuration): MessageDispatcher = Dispatchers.globalExecutorBasedEventDrivenDispatcher
}

class ExecutorBasedEventDrivenDispatcherConfigurator extends MessageDispatcherConfigurator {
  def configure(config: Configuration): MessageDispatcher = {
    configureThreadPool(config, threadPoolConfig => new ExecutorBasedEventDrivenDispatcher(
      config.getString("name", newUuid.toString),
      config.getInt("throughput", Dispatchers.THROUGHPUT),
      config.getInt("throughput-deadline-time", Dispatchers.THROUGHPUT_DEADLINE_TIME_MILLIS),
      mailboxType(config),
      threadPoolConfig)).build
  }
}

class ExecutorBasedEventDrivenWorkStealingDispatcherConfigurator extends MessageDispatcherConfigurator {
  def configure(config: Configuration): MessageDispatcher = {
    configureThreadPool(config, threadPoolConfig => new ExecutorBasedEventDrivenWorkStealingDispatcher(
      config.getString("name", newUuid.toString),
      config.getInt("throughput", Dispatchers.THROUGHPUT),
      config.getInt("throughput-deadline-time", Dispatchers.THROUGHPUT_DEADLINE_TIME_MILLIS),
      mailboxType(config),
      threadPoolConfig)).build
  }
}
