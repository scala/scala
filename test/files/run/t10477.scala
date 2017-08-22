abstract class BasicBackend {
  type F
  val f: F
}

class DistributedBackend extends BasicBackend {
  type F = FImpl
  val f: F = new FImpl
  class FImpl
}

trait BasicProfile {
  type Backend <: BasicBackend
  val backend: Backend
  trait SimpleQL {
    val f: backend.F = backend.f
  }
}

trait DistributedProfile extends BasicProfile { _: DistributedDriver =>
  type Backend = DistributedBackend
  val backend: Backend = new DistributedBackend
  class SimpleQlImpl extends SimpleQL
  new SimpleQlImpl
}

class DistributedDriver extends DistributedProfile

object Test extends App {
  new DistributedDriver()
}
