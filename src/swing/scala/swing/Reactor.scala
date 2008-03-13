package swing;

trait Reactor {
  val reactions = new Reactions
  def listenTo(ps: Publisher*) = for (val p <- ps) p.subscribe(reactions)
  def deafTo(ps: Publisher*) = for (val p <- ps) p.unsubscribe(reactions)
}
