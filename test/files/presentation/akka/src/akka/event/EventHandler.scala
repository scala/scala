/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka.event

import akka.actor._
import akka.config.Config._
import akka.config.ConfigurationException
import akka.util.{ ListenerManagement, ReflectiveAccess }
import akka.AkkaException

/**
 * Event handler.
 * <p/>
 * Create, add and remove a listener:
 * <pre>
 * val eventHandlerListener = Actor.actorOf(new Actor {
 *   self.dispatcher = EventHandler.EventHandlerDispatcher
 *
 *   def receive = {
 *     case EventHandler.Error(cause, instance, message) => ...
 *     case EventHandler.Warning(instance, message)      => ...
 *     case EventHandler.Info(instance, message)         => ...
 *     case EventHandler.Debug(instance, message)        => ...
 *     case genericEvent                                 => ...
 *   }
 * })
 *
 * EventHandler.addListener(eventHandlerListener)
 * ...
 * EventHandler.removeListener(eventHandlerListener)
 * </pre>
 * <p/>
 * However best is probably to register the listener in the 'akka.conf'
 * configuration file.
 * <p/>
 * Log an error event:
 * <pre>
 * EventHandler.notify(EventHandler.Error(exception, this, message))
 * </pre>
 * Or use the direct methods (better performance):
 * <pre>
 * EventHandler.error(exception, this, message)
 * </pre>
 *
 * Shut down the EventHandler:
 * <pre>
 * EventHandler.shutdown()
 * </pre>
 *
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
object EventHandler extends ListenerManagement {
  import java.io.{ StringWriter, PrintWriter }
  import java.text.DateFormat
  import java.util.Date
  import akka.dispatch.Dispatchers

  val ErrorLevel = 1
  val WarningLevel = 2
  val InfoLevel = 3
  val DebugLevel = 4

  sealed trait Event {
    @transient
    val thread: Thread = Thread.currentThread
    val level: Int
  }
  case class Error(cause: Throwable, instance: AnyRef, message: Any = "") extends Event {
    override val level = ErrorLevel
  }
  case class Warning(instance: AnyRef, message: Any = "") extends Event {
    override val level = WarningLevel
  }
  case class Info(instance: AnyRef, message: Any = "") extends Event {
    override val level = InfoLevel
  }
  case class Debug(instance: AnyRef, message: Any = "") extends Event {
    override val level = DebugLevel
  }

  val error = "[ERROR]   [%s] [%s] [%s] %s\n%s".intern
  val warning = "[WARN]    [%s] [%s] [%s] %s".intern
  val info = "[INFO]    [%s] [%s] [%s] %s".intern
  val debug = "[DEBUG]   [%s] [%s] [%s] %s".intern
  val generic = "[GENERIC] [%s] [%s]".intern
  val ID = "event:handler".intern

  class EventHandlerException extends AkkaException

  lazy val EventHandlerDispatcher = Dispatchers.newExecutorBasedEventDrivenDispatcher(ID).build

  val level: Int = config.getString("akka.event-handler-level", "INFO") match {
    case "ERROR"   => ErrorLevel
    case "WARNING" => WarningLevel
    case "INFO"    => InfoLevel
    case "DEBUG"   => DebugLevel
    case unknown => throw new ConfigurationException(
      "Configuration option 'akka.event-handler-level' is invalid [" + unknown + "]")
  }

  /**
   * Shuts down all event handler listeners including the event handle dispatcher.
   */
  def shutdown() {
    foreachListener(_.stop())
    EventHandlerDispatcher.shutdown()
  }

  def notify(event: Any) {
    if (event.isInstanceOf[Event]) {
      if (level >= event.asInstanceOf[Event].level) notifyListeners(event)
    } else
      notifyListeners(event)
  }

  def notify[T <: Event: ClassManifest](event: => T) {
    if (level >= levelFor(classManifest[T].erasure.asInstanceOf[Class[_ <: Event]])) notifyListeners(event)
  }

  def error(cause: Throwable, instance: AnyRef, message: => String) {
    if (level >= ErrorLevel) notifyListeners(Error(cause, instance, message))
  }

  def error(cause: Throwable, instance: AnyRef, message: Any) {
    if (level >= ErrorLevel) notifyListeners(Error(cause, instance, message))
  }

  def error(instance: AnyRef, message: => String) {
    if (level >= ErrorLevel) notifyListeners(Error(new EventHandlerException, instance, message))
  }

  def error(instance: AnyRef, message: Any) {
    if (level >= ErrorLevel) notifyListeners(Error(new EventHandlerException, instance, message))
  }

  def warning(instance: AnyRef, message: => String) {
    if (level >= WarningLevel) notifyListeners(Warning(instance, message))
  }

  def warning(instance: AnyRef, message: Any) {
    if (level >= WarningLevel) notifyListeners(Warning(instance, message))
  }

  def info(instance: AnyRef, message: => String) {
    if (level >= InfoLevel) notifyListeners(Info(instance, message))
  }

  def info(instance: AnyRef, message: Any) {
    if (level >= InfoLevel) notifyListeners(Info(instance, message))
  }

  def debug(instance: AnyRef, message: => String) {
    if (level >= DebugLevel) notifyListeners(Debug(instance, message))
  }

  def debug(instance: AnyRef, message: Any) {
    if (level >= DebugLevel) notifyListeners(Debug(instance, message))
  }

  def isInfoEnabled = level >= InfoLevel

  def isDebugEnabled = level >= DebugLevel

  def formattedTimestamp = DateFormat.getInstance.format(new Date)

  def stackTraceFor(e: Throwable) = {
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    e.printStackTrace(pw)
    sw.toString
  }

  private def levelFor(eventClass: Class[_ <: Event]) = {
    if (eventClass.isInstanceOf[Error]) ErrorLevel
    else if (eventClass.isInstanceOf[Warning]) WarningLevel
    else if (eventClass.isInstanceOf[Info]) InfoLevel
    else if (eventClass.isInstanceOf[Debug]) DebugLevel
    else DebugLevel
  }

  class DefaultListener extends Actor {
    self.id = ID
    self.dispatcher = EventHandlerDispatcher

    def receive = {
      case event@Error(cause, instance, message) =>
        println(error.format(
          formattedTimestamp,
          event.thread.getName,
          instance.getClass.getSimpleName,
          message,
          stackTraceFor(cause)))
      case event@Warning(instance, message) =>
        println(warning.format(
          formattedTimestamp,
          event.thread.getName,
          instance.getClass.getSimpleName,
          message))
      case event@Info(instance, message) =>
        println(info.format(
          formattedTimestamp,
          event.thread.getName,
          instance.getClass.getSimpleName,
          message))
      case event@Debug(instance, message) =>
        println(debug.format(
          formattedTimestamp,
          event.thread.getName,
          instance.getClass.getSimpleName,
          message))
      case event =>
        println(generic.format(formattedTimestamp, event.toString))
    }
  }

  val defaultListeners = config.getList("akka.event-handlers") match {
    case Nil       => "akka.event.EventHandler$DefaultListener" :: Nil
    case listeners => listeners
  }
  defaultListeners foreach { listenerName =>
    try {
      ReflectiveAccess.getClassFor[Actor](listenerName) match {
        case r: Right[_, Class[Actor]] => addListener(Actor.actorOf(r.b).start())
        case l: Left[Exception, _]     => throw l.a
      }
    } catch {
      case e: Exception =>
        throw new ConfigurationException(
          "Event Handler specified in config can't be loaded [" + listenerName +
            "] due to [" + e.toString + "]", e)
    }
  }
}
