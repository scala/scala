package swing

import scala.collection.mutable.HashSet
import event.Event

trait Publisher extends Reactor {
  protected var listeners = new HashSet[Reactions]

  def subscribe(listener: Reactions) { listeners += listener }
  def unsubscribe(listener: Reactions) { listeners -= listener }
  def publish(e: Event) { for (val l <- listeners) l.send(e) }

  listenTo(this)
}
