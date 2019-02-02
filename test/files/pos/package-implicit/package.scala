package t1000647

package object foo {
  implicit def scala2ActorRef(ref: ScalaActorRef): ActorRef =
    ref.asInstanceOf[ActorRef]
}
