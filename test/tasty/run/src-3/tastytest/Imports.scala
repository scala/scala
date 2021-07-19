package tastytest.test

sealed trait Imports

object Imports {
  sealed trait Mixin
  case object First extends Imports with Mixin
  case object Second extends Imports with Mixin
}
