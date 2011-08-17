/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka.actor

import scala.collection.mutable.{ ListBuffer, Map }
import scala.reflect.Manifest

import java.util.concurrent.{ ConcurrentSkipListSet, ConcurrentHashMap }
import java.util.{ Set => JSet }

import annotation.tailrec
import akka.util.ReflectiveAccess._
import akka.util.{ ReflectiveAccess, ReadWriteGuard, ListenerManagement }

/**
 * Base trait for ActorRegistry events, allows listen to when an actor is added and removed from the ActorRegistry.
 *
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
sealed trait ActorRegistryEvent
case class ActorRegistered(actor: ActorRef) extends ActorRegistryEvent
case class ActorUnregistered(actor: ActorRef) extends ActorRegistryEvent

/**
 * Registry holding all Actor instances in the whole system.
 * Mapped by:
 * <ul>
 * <li>the Actor's UUID</li>
 * <li>the Actor's id field (which can be set by user-code)</li>
 * <li>the Actor's class</li>
 * <li>all Actors that are subtypes of a specific type</li>
 * <ul>
 *
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */

final class ActorRegistry private[actor] () extends ListenerManagement {

  private val actorsByUUID = new ConcurrentHashMap[Uuid, ActorRef]
  private val actorsById = new Index[String, ActorRef]
  private val guard = new ReadWriteGuard

  /**
   * Returns all actors in the system.
   */
  def actors: Array[ActorRef] = filter(_ => true)

  /**
   * Returns the number of actors in the system.
   */
  def size: Int = actorsByUUID.size

  /**
   * Invokes a function for all actors.
   */
  def foreach(f: (ActorRef) => Unit) = {
    val elements = actorsByUUID.elements
    while (elements.hasMoreElements) f(elements.nextElement)
  }

  /**
   * Invokes the function on all known actors until it returns Some
   * Returns None if the function never returns Some
   */
  def find[T](f: PartialFunction[ActorRef, T]): Option[T] = {
    val elements = actorsByUUID.elements
    while (elements.hasMoreElements) {
      val element = elements.nextElement
      if (f isDefinedAt element) return Some(f(element))
    }
    None
  }

  /**
   * Finds all actors that are subtypes of the class passed in as the Manifest argument and supporting passed message.
   */
  def actorsFor[T <: Actor](message: Any)(implicit manifest: Manifest[T]): Array[ActorRef] =
    filter(a => manifest.erasure.isAssignableFrom(a.actor.getClass) && a.isDefinedAt(message))

  /**
   * Finds all actors that satisfy a predicate.
   */
  def filter(p: ActorRef => Boolean): Array[ActorRef] = {
    val all = new ListBuffer[ActorRef]
    val elements = actorsByUUID.elements
    while (elements.hasMoreElements) {
      val actorId = elements.nextElement
      if (p(actorId)) all += actorId
    }
    all.toArray
  }

  /**
   * Finds all actors that are subtypes of the class passed in as the Manifest argument.
   */
  def actorsFor[T <: Actor](implicit manifest: Manifest[T]): Array[ActorRef] =
    actorsFor[T](manifest.erasure.asInstanceOf[Class[T]])

  /**
   * Finds any actor that matches T. Very expensive, traverses ALL alive actors.
   */
  def actorFor[T <: Actor](implicit manifest: Manifest[T]): Option[ActorRef] =
    find({ case a: ActorRef if manifest.erasure.isAssignableFrom(a.actor.getClass) => a })

  /**
   * Finds all actors of type or sub-type specified by the class passed in as the Class argument.
   */
  def actorsFor[T <: Actor](clazz: Class[T]): Array[ActorRef] =
    filter(a => clazz.isAssignableFrom(a.actor.getClass))

  /**
   * Finds all actors that has a specific id.
   */
  def actorsFor(id: String): Array[ActorRef] = actorsById values id

  /**
   * Finds the actor that has a specific UUID.
   */
  def actorFor(uuid: Uuid): Option[ActorRef] = Option(actorsByUUID get uuid)

  /**
   * Returns all typed actors in the system.
   */
  def typedActors: Array[AnyRef] = filterTypedActors(_ => true)

  /**
   * Invokes a function for all typed actors.
   */
  def foreachTypedActor(f: (AnyRef) => Unit) = {
    TypedActorModule.ensureEnabled
    val elements = actorsByUUID.elements
    while (elements.hasMoreElements) {
      val proxy = typedActorFor(elements.nextElement)
      if (proxy.isDefined) f(proxy.get)
    }
  }

  /**
   * Invokes the function on all known typed actors until it returns Some
   * Returns None if the function never returns Some
   */
  def findTypedActor[T](f: PartialFunction[AnyRef, T]): Option[T] = {
    TypedActorModule.ensureEnabled
    val elements = actorsByUUID.elements
    while (elements.hasMoreElements) {
      val proxy = typedActorFor(elements.nextElement)
      if (proxy.isDefined && (f isDefinedAt proxy)) return Some(f(proxy))
    }
    None
  }

  /**
   * Finds all typed actors that satisfy a predicate.
   */
  def filterTypedActors(p: AnyRef => Boolean): Array[AnyRef] = {
    TypedActorModule.ensureEnabled
    val all = new ListBuffer[AnyRef]
    val elements = actorsByUUID.elements
    while (elements.hasMoreElements) {
      val proxy = typedActorFor(elements.nextElement)
      if (proxy.isDefined && p(proxy.get)) all += proxy.get
    }
    all.toArray
  }

  /**
   * Finds all typed actors that are subtypes of the class passed in as the Manifest argument.
   */
  def typedActorsFor[T <: AnyRef](implicit manifest: Manifest[T]): Array[AnyRef] = {
    TypedActorModule.ensureEnabled
    typedActorsFor[T](manifest.erasure.asInstanceOf[Class[T]])
  }

  /**
   * Finds any typed actor that matches T.
   */
  def typedActorFor[T <: AnyRef](implicit manifest: Manifest[T]): Option[AnyRef] = {
    TypedActorModule.ensureEnabled
    def predicate(proxy: AnyRef): Boolean = {
      val actorRef = TypedActorModule.typedActorObjectInstance.get.actorFor(proxy)
      actorRef.isDefined && manifest.erasure.isAssignableFrom(actorRef.get.actor.getClass)
    }
    findTypedActor({ case a: Some[AnyRef] if predicate(a.get) => a })
  }

  /**
   * Finds all typed actors of type or sub-type specified by the class passed in as the Class argument.
   */
  def typedActorsFor[T <: AnyRef](clazz: Class[T]): Array[AnyRef] = {
    TypedActorModule.ensureEnabled
    def predicate(proxy: AnyRef): Boolean = {
      val actorRef = TypedActorModule.typedActorObjectInstance.get.actorFor(proxy)
      actorRef.isDefined && clazz.isAssignableFrom(actorRef.get.actor.getClass)
    }
    filterTypedActors(predicate)
  }

  /**
   * Finds all typed actors that have a specific id.
   */
  def typedActorsFor(id: String): Array[AnyRef] = {
    TypedActorModule.ensureEnabled
    val actorRefs = actorsById values id
    actorRefs.flatMap(typedActorFor(_))
  }

  /**
   * Finds the typed actor that has a specific UUID.
   */
  def typedActorFor(uuid: Uuid): Option[AnyRef] = {
    TypedActorModule.ensureEnabled
    val actorRef = actorsByUUID get uuid
    if (actorRef eq null) None
    else typedActorFor(actorRef)
  }

  /**
   * Get the typed actor proxy for a given typed actor ref.
   */
  private def typedActorFor(actorRef: ActorRef): Option[AnyRef] = {
    TypedActorModule.typedActorObjectInstance.get.proxyFor(actorRef)
  }

  /**
   *  Registers an actor in the ActorRegistry.
   */
  private[akka] def register(actor: ActorRef) {
    val id = actor.id
    val uuid = actor.uuid

    actorsById.put(id, actor)
    actorsByUUID.put(uuid, actor)

    // notify listeners
    notifyListeners(ActorRegistered(actor))
  }

  /**
   * Unregisters an actor in the ActorRegistry.
   */
  private[akka] def unregister(actor: ActorRef) {
    val id = actor.id
    val uuid = actor.uuid

    actorsByUUID remove uuid
    actorsById.remove(id, actor)

    // notify listeners
    notifyListeners(ActorUnregistered(actor))
  }

  /**
   * Shuts down and unregisters all actors in the system.
   */
  def shutdownAll() {
    if (TypedActorModule.isEnabled) {
      val elements = actorsByUUID.elements
      while (elements.hasMoreElements) {
        val actorRef = elements.nextElement
        val proxy = typedActorFor(actorRef)
        if (proxy.isDefined) TypedActorModule.typedActorObjectInstance.get.stop(proxy.get)
        else actorRef.stop()
      }
    } else foreach(_.stop())
    if (Remote.isEnabled) {
      Actor.remote.clear //TODO: REVISIT: Should this be here?
    }
    actorsByUUID.clear
    actorsById.clear
  }
}

/**
 * An implementation of a ConcurrentMultiMap
 * Adds/remove is serialized over the specified key
 * Reads are fully concurrent <-- el-cheapo
 *
 * @author Viktor Klang
 */
class Index[K <: AnyRef, V <: AnyRef: Manifest] {
  private val Naught = Array[V]() //Nil for Arrays
  private val container = new ConcurrentHashMap[K, JSet[V]]
  private val emptySet = new ConcurrentSkipListSet[V]

  /**
   * Associates the value of type V with the key of type K
   * @return true if the value didn't exist for the key previously, and false otherwise
   */
  def put(key: K, value: V): Boolean = {
    //Tailrecursive spin-locking put
    @tailrec
    def spinPut(k: K, v: V): Boolean = {
      var retry = false
      var added = false
      val set = container get k

      if (set ne null) {
        set.synchronized {
          if (set.isEmpty) retry = true //IF the set is empty then it has been removed, so signal retry
          else { //Else add the value to the set and signal that retry is not needed
            added = set add v
            retry = false
          }
        }
      } else {
        val newSet = new ConcurrentSkipListSet[V]
        newSet add v

        // Parry for two simultaneous putIfAbsent(id,newSet)
        val oldSet = container.putIfAbsent(k, newSet)
        if (oldSet ne null) {
          oldSet.synchronized {
            if (oldSet.isEmpty) retry = true //IF the set is empty then it has been removed, so signal retry
            else { //Else try to add the value to the set and signal that retry is not needed
              added = oldSet add v
              retry = false
            }
          }
        } else added = true
      }

      if (retry) spinPut(k, v)
      else added
    }

    spinPut(key, value)
  }

  /**
   * @return a _new_ array of all existing values for the given key at the time of the call
   */
  def values(key: K): Array[V] = {
    val set: JSet[V] = container get key
    val result = if (set ne null) set toArray Naught else Naught
    result.asInstanceOf[Array[V]]
  }

  /**
   * @return Some(value) for the first matching value where the supplied function returns true for the given key,
   * if no matches it returns None
   */
  def findValue(key: K)(f: (V) => Boolean): Option[V] = {
    import scala.collection.JavaConversions._
    val set = container get key
    if (set ne null) set.iterator.find(f)
    else None
  }

  /**
   * Applies the supplied function to all keys and their values
   */
  def foreach(fun: (K, V) => Unit) {
    import scala.collection.JavaConversions._
    container.entrySet foreach { (e) =>
      e.getValue.foreach(fun(e.getKey, _))
    }
  }

  /**
   * Disassociates the value of type V from the key of type K
   * @return true if the value was disassociated from the key and false if it wasn't previously associated with the key
   */
  def remove(key: K, value: V): Boolean = {
    val set = container get key

    if (set ne null) {
      set.synchronized {
        if (set.remove(value)) { //If we can remove the value
          if (set.isEmpty) //and the set becomes empty
            container.remove(key, emptySet) //We try to remove the key if it's mapped to an empty set

          true //Remove succeeded
        } else false //Remove failed
      }
    } else false //Remove failed
  }

  /**
   * @return true if the underlying containers is empty, may report false negatives when the last remove is underway
   */
  def isEmpty: Boolean = container.isEmpty

  /**
   *  Removes all keys and all values
   */
  def clear = foreach { case (k, v) => remove(k, v) }
}
