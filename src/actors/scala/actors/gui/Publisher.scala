package scala.actors.gui

import scala.collection.mutable.ListBuffer

import scala.actors.single.Actor

import scala.actors.gui.event.Event

class EventHandlers {
  type Handler = PartialFunction[Any,unit]

  private val handlers = new ListBuffer[Handler]

  def += (h: Handler) = { handlers += h }
  def -= (h: Handler) = { handlers -= h }

  def compoundHandler = new Handler {
    def isDefinedAt(e: Any): boolean = handlers.exists(.isDefinedAt(e))

    def apply(e: Any): unit =
      handlers.find(.isDefinedAt(e)) match {
        case Some(h) => h.apply(e)
        case None => // do nothing
      }
  }
}

trait Responder extends Actor[Any] {
  protected val handlers = new EventHandlers

  final def eventloop(f: PartialFunction[Any,unit]): scala.All =
    receive(new RecursiveProxyHandler(this, f))

  def eventblock(f: PartialFunction[Any,unit]): unit = {
    try {
      receive(new RecursiveProxyHandler(this, f))
    }
    catch {
      case d: Done =>
        // do nothing
    }
  }

  private class RecursiveProxyHandler(a: Actor[Any], f: PartialFunction[Any,unit]) extends PartialFunction[Any,unit] {
    def isDefinedAt(m: Any): boolean =
      true // events should be removed from the mailbox immediately!

    def apply(m: Any): unit = {
      if (f.isDefinedAt(m)) f(m) // overrides any installed handler
      else
        if (handlers.compoundHandler.isDefinedAt(m))
          handlers.compoundHandler(m)
        else {
          // do nothing
        }
      a receive this
    }
  }
}

case class Subscribe(s: Subscriber)
case class Publish(e: Event)

trait Subscriber extends Responder {
  type Handler = PartialFunction[Any,unit]
  def subscribe(ps: Publisher*) = for (val p <- ps) p send Subscribe(this)
}

trait Publisher extends Responder {
  case class HandlerAdded()

  private val subscribers = new ListBuffer[Subscriber]

  handlers += { // installs _permanent_ handler!
    case Subscribe(s) =>
      //Console.println("" + this + ": rec subscription from " + s)
      subscribers += s
    case Publish(e) => for (val s <- subscribers) s send e
  }

  //Console.println("" + this + ": exec toplevel eventloop (Publisher)")

  eventblock {
    case HandlerAdded() =>
      //Console.println("" + this + " received HandlerAdded()")
  }

  def addHandler(h: EventHandlers#Handler) = {
    //Console.println("" + this + ": installing new handler")
    handlers += h
    this send HandlerAdded() // causes currently active eventloop to recursively call itself
  }

  def publish(e: Event) = {
    //Console.println("Publishing event: " + e)
    for (val s <- subscribers) s send e
  }

  // TODO: super.receive might already be overridden!
  //final override def receive(f: PartialFunction[Any,unit]): scala.All =
    //super.receive(new ProxyPubSubHandler(f))

  private class ProxyPubSubHandler(f: PartialFunction[Any,unit]) extends PartialFunction[Any,unit] {
    def isDefinedAt(m: Any): boolean =
      if (f.isDefinedAt(m)) true
      else m match {
        case Subscribe(s) => true
        case Publish(e) => true
        case other => false
      }

    def apply(m: Any): unit = {
      m match {
        case Subscribe(s) =>
          //Console.println("Rec subscription: " + s)
          subscribers += s
        case Publish(e) =>
          for (val s <- subscribers) s send e
        case other =>
          // do nothing
      }
      if (f.isDefinedAt(m)) f(m)
    }
  }
}
