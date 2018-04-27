package t1000647.bar

import t1000647.foo.{ScalaActorRef, scala2ActorRef}

object DataFlow {
  def foo(ref: ScalaActorRef) = ref.stop()
}
