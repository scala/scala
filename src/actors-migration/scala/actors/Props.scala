package scala.actors

/**
 * ActorRef configuration object. It represents the minimal subset of Akka Props class.
 */
case class Props(creator: () ⇒ InternalActor, dispatcher: String) {

  /**
   * Returns a new Props with the specified creator set
   */
  def withCreator(c: ⇒ InternalActor) = copy(creator = () ⇒ c)

}
