package tastytest

object TestDelayedInternal {

  compiletimeHasChild[DelayedInternal[_]]("tastytest.DelayedInternal.Internal.Instance")

}
