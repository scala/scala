package scala.swing

trait Reactor {
  val reactions = new Reactions.Impl
  /**
   * Listen to the given publisher as long as <code>deafTo</code> isn't called for
   * them.
   */
  def listenTo(ps: Publisher*) = for (val p <- ps) p.subscribe(reactions)
  /**
   * Installed reaction won't receive events from the given publisher anylonger.
   */
  def deafTo(ps: Publisher*) = for (val p <- ps) p.unsubscribe(reactions)
}
