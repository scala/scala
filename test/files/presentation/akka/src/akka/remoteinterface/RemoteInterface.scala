/**
 * Copyright (C) 2009-2010 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka.remoteinterface

import akka.japi.Creator
import akka.actor._
import akka.util._
import akka.dispatch.CompletableFuture
import akka.AkkaException

import scala.beans.BeanProperty

import java.net.InetSocketAddress
import java.util.concurrent.ConcurrentHashMap
import java.io.{ PrintWriter, PrintStream }
import java.lang.reflect.InvocationTargetException

trait RemoteModule {
  val UUID_PREFIX = "uuid:".intern

  def optimizeLocalScoped_?(): Boolean //Apply optimizations for remote operations in local scope
  protected[akka] def notifyListeners(message: => Any): Unit

  private[akka] def actors: ConcurrentHashMap[String, ActorRef]
  private[akka] def actorsByUuid: ConcurrentHashMap[String, ActorRef]
  private[akka] def actorsFactories: ConcurrentHashMap[String, () => ActorRef]
  private[akka] def typedActors: ConcurrentHashMap[String, AnyRef]
  private[akka] def typedActorsByUuid: ConcurrentHashMap[String, AnyRef]
  private[akka] def typedActorsFactories: ConcurrentHashMap[String, () => AnyRef]

  /** Lookup methods **/

  private[akka] def findActorById(id: String): ActorRef = actors.get(id)

  private[akka] def findActorByUuid(uuid: String): ActorRef = actorsByUuid.get(uuid)

  private[akka] def findActorFactory(id: String): () => ActorRef = actorsFactories.get(id)

  private[akka] def findTypedActorById(id: String): AnyRef = typedActors.get(id)

  private[akka] def findTypedActorFactory(id: String): () => AnyRef = typedActorsFactories.get(id)

  private[akka] def findTypedActorByUuid(uuid: String): AnyRef = typedActorsByUuid.get(uuid)

  private[akka] def findActorByIdOrUuid(id: String, uuid: String): ActorRef = {
    var actorRefOrNull = if (id.startsWith(UUID_PREFIX)) findActorByUuid(id.substring(UUID_PREFIX.length))
    else findActorById(id)
    if (actorRefOrNull eq null) actorRefOrNull = findActorByUuid(uuid)
    actorRefOrNull
  }

  private[akka] def findTypedActorByIdOrUuid(id: String, uuid: String): AnyRef = {
    var actorRefOrNull = if (id.startsWith(UUID_PREFIX)) findTypedActorByUuid(id.substring(UUID_PREFIX.length))
    else findTypedActorById(id)
    if (actorRefOrNull eq null) actorRefOrNull = findTypedActorByUuid(uuid)
    actorRefOrNull
  }
}

/**
 * Life-cycle events for RemoteClient.
 */
sealed trait RemoteClientLifeCycleEvent
case class RemoteClientError(
  @BeanProperty cause: Throwable,
  @BeanProperty client: RemoteClientModule,
  @BeanProperty remoteAddress: InetSocketAddress) extends RemoteClientLifeCycleEvent
case class RemoteClientDisconnected(
  @BeanProperty client: RemoteClientModule,
  @BeanProperty remoteAddress: InetSocketAddress) extends RemoteClientLifeCycleEvent
case class RemoteClientConnected(
  @BeanProperty client: RemoteClientModule,
  @BeanProperty remoteAddress: InetSocketAddress) extends RemoteClientLifeCycleEvent
case class RemoteClientStarted(
  @BeanProperty client: RemoteClientModule,
  @BeanProperty remoteAddress: InetSocketAddress) extends RemoteClientLifeCycleEvent
case class RemoteClientShutdown(
  @BeanProperty client: RemoteClientModule,
  @BeanProperty remoteAddress: InetSocketAddress) extends RemoteClientLifeCycleEvent
case class RemoteClientWriteFailed(
  @BeanProperty request: AnyRef,
  @BeanProperty cause: Throwable,
  @BeanProperty client: RemoteClientModule,
  @BeanProperty remoteAddress: InetSocketAddress) extends RemoteClientLifeCycleEvent

/**
 *  Life-cycle events for RemoteServer.
 */
sealed trait RemoteServerLifeCycleEvent
case class RemoteServerStarted(
  @BeanProperty val server: RemoteServerModule) extends RemoteServerLifeCycleEvent
case class RemoteServerShutdown(
  @BeanProperty val server: RemoteServerModule) extends RemoteServerLifeCycleEvent
case class RemoteServerError(
  @BeanProperty val cause: Throwable,
  @BeanProperty val server: RemoteServerModule) extends RemoteServerLifeCycleEvent
case class RemoteServerClientConnected(
  @BeanProperty val server: RemoteServerModule,
  @BeanProperty val clientAddress: Option[InetSocketAddress]) extends RemoteServerLifeCycleEvent
case class RemoteServerClientDisconnected(
  @BeanProperty val server: RemoteServerModule,
  @BeanProperty val clientAddress: Option[InetSocketAddress]) extends RemoteServerLifeCycleEvent
case class RemoteServerClientClosed(
  @BeanProperty val server: RemoteServerModule,
  @BeanProperty val clientAddress: Option[InetSocketAddress]) extends RemoteServerLifeCycleEvent
case class RemoteServerWriteFailed(
  @BeanProperty request: AnyRef,
  @BeanProperty cause: Throwable,
  @BeanProperty server: RemoteServerModule,
  @BeanProperty clientAddress: Option[InetSocketAddress]) extends RemoteServerLifeCycleEvent

/**
 * Thrown for example when trying to send a message using a RemoteClient that is either not started or shut down.
 */
class RemoteClientException private[akka] (
  message: String,
  @BeanProperty val client: RemoteClientModule,
  val remoteAddress: InetSocketAddress, cause: Throwable = null) extends AkkaException(message, cause)

/**
 * Thrown when the remote server actor dispatching fails for some reason.
 */
class RemoteServerException private[akka] (message: String) extends AkkaException(message)

/**
 * Thrown when a remote exception sent over the wire cannot be loaded and instantiated
 */
case class CannotInstantiateRemoteExceptionDueToRemoteProtocolParsingErrorException private[akka] (cause: Throwable, originalClassName: String, originalMessage: String)
  extends AkkaException("\nParsingError[%s]\nOriginalException[%s]\nOriginalMessage[%s]"
    .format(cause.toString, originalClassName, originalMessage)) {
  override def printStackTrace = cause.printStackTrace
  override def printStackTrace(printStream: PrintStream) = cause.printStackTrace(printStream)
  override def printStackTrace(printWriter: PrintWriter) = cause.printStackTrace(printWriter)
}

abstract class RemoteSupport extends ListenerManagement with RemoteServerModule with RemoteClientModule {

  lazy val eventHandler: ActorRef = {
    val handler = Actor.actorOf[RemoteEventHandler].start()
    // add the remote client and server listener that pipes the events to the event handler system
    addListener(handler)
    handler
  }

  def shutdown() {
    eventHandler.stop()
    removeListener(eventHandler)
    this.shutdownClientModule()
    this.shutdownServerModule()
    clear
  }

  /**
   * Creates a Client-managed ActorRef out of the Actor of the specified Class.
   * If the supplied host and port is identical of the configured local node, it will be a local actor
   * <pre>
   *   import Actor._
   *   val actor = actorOf(classOf[MyActor],"www.akka.io", 2552)
   *   actor.start()
   *   actor ! message
   *   actor.stop()
   * </pre>
   * You can create and start the actor in one statement like this:
   * <pre>
   *   val actor = actorOf(classOf[MyActor],"www.akka.io", 2552).start()
   * </pre>
   */
  @deprecated("Will be removed after 1.1", "1.1")
  def actorOf(factory: => Actor, host: String, port: Int): ActorRef =
    Actor.remote.clientManagedActorOf(() => factory, host, port)

  /**
   * Creates a Client-managed ActorRef out of the Actor of the specified Class.
   * If the supplied host and port is identical of the configured local node, it will be a local actor
   * <pre>
   *   import Actor._
   *   val actor = actorOf(classOf[MyActor],"www.akka.io",2552)
   *   actor.start()
   *   actor ! message
   *   actor.stop()
   * </pre>
   * You can create and start the actor in one statement like this:
   * <pre>
   *   val actor = actorOf(classOf[MyActor],"www.akka.io",2552).start()
   * </pre>
   */
  @deprecated("Will be removed after 1.1", "1.1")
  def actorOf(clazz: Class[_ <: Actor], host: String, port: Int): ActorRef =
    clientManagedActorOf(() => createActorFromClass(clazz), host, port)

  /**
   * Creates a Client-managed ActorRef out of the Actor of the specified Class.
   * If the supplied host and port is identical of the configured local node, it will be a local actor
   * <pre>
   *   import Actor._
   *   val actor = actorOf[MyActor]("www.akka.io",2552)
   *   actor.start()
   *   actor ! message
   *   actor.stop()
   * </pre>
   * You can create and start the actor in one statement like this:
   * <pre>
   *   val actor = actorOf[MyActor]("www.akka.io",2552).start()
   * </pre>
   */
  @deprecated("Will be removed after 1.1", "1.1")
  def actorOf[T <: Actor: Manifest](host: String, port: Int): ActorRef =
    clientManagedActorOf(() => createActorFromClass(manifest.erasure), host, port)

  protected def createActorFromClass(clazz: Class[_]): Actor = {
    import ReflectiveAccess.{ createInstance, noParams, noArgs }
    createInstance[Actor](clazz, noParams, noArgs) match {
      case Right(actor) => actor
      case Left(exception) =>
        val cause = exception match {
          case i: InvocationTargetException => i.getTargetException
          case _                            => exception
        }

        throw new ActorInitializationException(
          "Could not instantiate Actor of " + clazz +
            "\nMake sure Actor is NOT defined inside a class/trait," +
            "\nif so put it outside the class/trait, f.e. in a companion object," +
            "\nOR try to change: 'actorOf[MyActor]' to 'actorOf(new MyActor)'.", cause)
    }
  }

  protected override def manageLifeCycleOfListeners = false
  protected[akka] override def notifyListeners(message: => Any): Unit = super.notifyListeners(message)

  private[akka] val actors = new ConcurrentHashMap[String, ActorRef]
  private[akka] val actorsByUuid = new ConcurrentHashMap[String, ActorRef]
  private[akka] val actorsFactories = new ConcurrentHashMap[String, () => ActorRef]
  private[akka] val typedActors = new ConcurrentHashMap[String, AnyRef]
  private[akka] val typedActorsByUuid = new ConcurrentHashMap[String, AnyRef]
  private[akka] val typedActorsFactories = new ConcurrentHashMap[String, () => AnyRef]

  def clear {
    actors.clear
    actorsByUuid.clear
    typedActors.clear
    typedActorsByUuid.clear
    actorsFactories.clear
    typedActorsFactories.clear
  }
}

/**
 * This is the interface for the RemoteServer functionality, it's used in Actor.remote
 */
trait RemoteServerModule extends RemoteModule {
  protected val guard = new ReentrantGuard

  /**
   * Signals whether the server is up and running or not
   */
  def isRunning: Boolean

  /**
   *  Gets the name of the server instance
   */
  def name: String

  /**
   * Gets the address of the server instance
   */
  def address: InetSocketAddress

  /**
   *  Starts the server up
   */
  def start(): RemoteServerModule =
    start(ReflectiveAccess.Remote.configDefaultAddress.getAddress.getHostAddress,
      ReflectiveAccess.Remote.configDefaultAddress.getPort,
      None)

  /**
   *  Starts the server up
   */
  def start(loader: ClassLoader): RemoteServerModule =
    start(ReflectiveAccess.Remote.configDefaultAddress.getAddress.getHostAddress,
      ReflectiveAccess.Remote.configDefaultAddress.getPort,
      Option(loader))

  /**
   *  Starts the server up
   */
  def start(host: String, port: Int): RemoteServerModule =
    start(host, port, None)

  /**
   *  Starts the server up
   */
  def start(host: String, port: Int, loader: ClassLoader): RemoteServerModule =
    start(host, port, Option(loader))

  /**
   *  Starts the server up
   */
  def start(host: String, port: Int, loader: Option[ClassLoader]): RemoteServerModule

  /**
   *  Shuts the server down
   */
  def shutdownServerModule(): Unit

  /**
   *  Register typed actor by interface name.
   */
  def registerTypedActor(intfClass: Class[_], typedActor: AnyRef): Unit = registerTypedActor(intfClass.getName, typedActor)

  /**
   * Register remote typed actor by a specific id.
   * @param id custom actor id
   * @param typedActor typed actor to register
   */
  def registerTypedActor(id: String, typedActor: AnyRef): Unit

  /**
   * Register typed actor by interface name.
   */
  def registerTypedPerSessionActor(intfClass: Class[_], factory: => AnyRef): Unit = registerTypedActor(intfClass.getName, factory)

  /**
   * Register typed actor by interface name.
   * Java API
   */
  def registerTypedPerSessionActor(intfClass: Class[_], factory: Creator[AnyRef]): Unit = registerTypedActor(intfClass.getName, factory)

  /**
   * Register remote typed actor by a specific id.
   * @param id custom actor id
   * @param typedActor typed actor to register
   */
  def registerTypedPerSessionActor(id: String, factory: => AnyRef): Unit

  /**
   * Register remote typed actor by a specific id.
   * @param id custom actor id
   * @param typedActor typed actor to register
   * Java API
   */
  def registerTypedPerSessionActor(id: String, factory: Creator[AnyRef]): Unit = registerTypedPerSessionActor(id, factory.create)

  /**
   * Register Remote Actor by the Actor's 'id' field. It starts the Actor if it is not started already.
   */
  def register(actorRef: ActorRef): Unit = register(actorRef.id, actorRef)

  /**
   *  Register Remote Actor by the Actor's uuid field. It starts the Actor if it is not started already.
   */
  def registerByUuid(actorRef: ActorRef): Unit

  /**
   *  Register Remote Actor by a specific 'id' passed as argument. The actor is registered by UUID rather than ID
   *  when prefixing the handle with the “uuid:” protocol.
   * <p/>
   * NOTE: If you use this method to register your remote actor then you must unregister the actor by this ID yourself.
   */
  def register(id: String, actorRef: ActorRef): Unit

  /**
   * Register Remote Session Actor by a specific 'id' passed as argument.
   * <p/>
   * NOTE: If you use this method to register your remote actor then you must unregister the actor by this ID yourself.
   */
  def registerPerSession(id: String, factory: => ActorRef): Unit

  /**
   * Register Remote Session Actor by a specific 'id' passed as argument.
   * <p/>
   * NOTE: If you use this method to register your remote actor then you must unregister the actor by this ID yourself.
   * Java API
   */
  def registerPerSession(id: String, factory: Creator[ActorRef]): Unit = registerPerSession(id, factory.create)

  /**
   * Unregister Remote Actor that is registered using its 'id' field (not custom ID).
   */
  def unregister(actorRef: ActorRef): Unit

  /**
   * Unregister Remote Actor by specific 'id'.
   * <p/>
   * NOTE: You need to call this method if you have registered an actor by a custom ID.
   */
  def unregister(id: String): Unit

  /**
   * Unregister Remote Actor by specific 'id'.
   * <p/>
   * NOTE: You need to call this method if you have registered an actor by a custom ID.
   */
  def unregisterPerSession(id: String): Unit

  /**
   * Unregister Remote Typed Actor by specific 'id'.
   * <p/>
   * NOTE: You need to call this method if you have registered an actor by a custom ID.
   */
  def unregisterTypedActor(id: String): Unit

  /**
   * Unregister Remote Typed Actor by specific 'id'.
   * <p/>
   * NOTE: You need to call this method if you have registered an actor by a custom ID.
   */
  def unregisterTypedPerSessionActor(id: String): Unit
}

trait RemoteClientModule extends RemoteModule { self: RemoteModule =>

  def actorFor(classNameOrServiceId: String, hostname: String, port: Int): ActorRef =
    actorFor(classNameOrServiceId, classNameOrServiceId, Actor.TIMEOUT, hostname, port, None)

  def actorFor(classNameOrServiceId: String, hostname: String, port: Int, loader: ClassLoader): ActorRef =
    actorFor(classNameOrServiceId, classNameOrServiceId, Actor.TIMEOUT, hostname, port, Some(loader))

  def actorFor(serviceId: String, className: String, hostname: String, port: Int): ActorRef =
    actorFor(serviceId, className, Actor.TIMEOUT, hostname, port, None)

  def actorFor(serviceId: String, className: String, hostname: String, port: Int, loader: ClassLoader): ActorRef =
    actorFor(serviceId, className, Actor.TIMEOUT, hostname, port, Some(loader))

  def actorFor(classNameOrServiceId: String, timeout: Long, hostname: String, port: Int): ActorRef =
    actorFor(classNameOrServiceId, classNameOrServiceId, timeout, hostname, port, None)

  def actorFor(classNameOrServiceId: String, timeout: Long, hostname: String, port: Int, loader: ClassLoader): ActorRef =
    actorFor(classNameOrServiceId, classNameOrServiceId, timeout, hostname, port, Some(loader))

  def actorFor(serviceId: String, className: String, timeout: Long, hostname: String, port: Int): ActorRef =
    actorFor(serviceId, className, timeout, hostname, port, None)

  def typedActorFor[T](intfClass: Class[T], serviceIdOrClassName: String, hostname: String, port: Int): T =
    typedActorFor(intfClass, serviceIdOrClassName, serviceIdOrClassName, Actor.TIMEOUT, hostname, port, None)

  def typedActorFor[T](intfClass: Class[T], serviceIdOrClassName: String, timeout: Long, hostname: String, port: Int): T =
    typedActorFor(intfClass, serviceIdOrClassName, serviceIdOrClassName, timeout, hostname, port, None)

  def typedActorFor[T](intfClass: Class[T], serviceIdOrClassName: String, timeout: Long, hostname: String, port: Int, loader: ClassLoader): T =
    typedActorFor(intfClass, serviceIdOrClassName, serviceIdOrClassName, timeout, hostname, port, Some(loader))

  def typedActorFor[T](intfClass: Class[T], serviceId: String, implClassName: String, timeout: Long, hostname: String, port: Int, loader: ClassLoader): T =
    typedActorFor(intfClass, serviceId, implClassName, timeout, hostname, port, Some(loader))

  @deprecated("Will be removed after 1.1", "1.1")
  def clientManagedActorOf(factory: () => Actor, host: String, port: Int): ActorRef

  /**
   * Clean-up all open connections.
   */
  def shutdownClientModule(): Unit

  /**
   * Shuts down a specific client connected to the supplied remote address returns true if successful
   */
  def shutdownClientConnection(address: InetSocketAddress): Boolean

  /**
   * Restarts a specific client connected to the supplied remote address, but only if the client is not shut down
   */
  def restartClientConnection(address: InetSocketAddress): Boolean

  /** Methods that needs to be implemented by a transport **/

  protected[akka] def typedActorFor[T](intfClass: Class[T], serviceId: String, implClassName: String, timeout: Long, host: String, port: Int, loader: Option[ClassLoader]): T

  protected[akka] def actorFor(serviceId: String, className: String, timeout: Long, hostname: String, port: Int, loader: Option[ClassLoader]): ActorRef

  protected[akka] def send[T](message: Any,
                              senderOption: Option[ActorRef],
                              senderFuture: Option[CompletableFuture[T]],
                              remoteAddress: InetSocketAddress,
                              timeout: Long,
                              isOneWay: Boolean,
                              actorRef: ActorRef,
                              typedActorInfo: Option[Tuple2[String, String]],
                              actorType: ActorType,
                              loader: Option[ClassLoader]): Option[CompletableFuture[T]]

  private[akka] def registerSupervisorForActor(actorRef: ActorRef): ActorRef

  private[akka] def deregisterSupervisorForActor(actorRef: ActorRef): ActorRef

  @deprecated("Will be removed after 1.1", "1.1")
  private[akka] def registerClientManagedActor(hostname: String, port: Int, uuid: Uuid): Unit

  @deprecated("Will be removed after 1.1", "1.1")
  private[akka] def unregisterClientManagedActor(hostname: String, port: Int, uuid: Uuid): Unit
}
