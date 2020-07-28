package tastytest


object TestDelayedInternalEmpty {

  compiletimeHasChild[DelayedInternalEmpty[_]]("tastytest.DelayedInternalEmpty.Internal.Empty")

}
