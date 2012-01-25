package t1000647.foo

trait ActorRef {
  def stop(): Unit = {}
}

trait ScalaActorRef { self: ActorRef => }