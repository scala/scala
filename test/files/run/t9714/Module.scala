package p {
  object Module {
    override def toString = "Module"
    object Target {
      override def toString = "Target"
    }
  }
}

object Test extends App {
  assert(p.J.f().toString == "J")
  assert(p.J.module().toString == "Module", "moduled")
  assert(p.J.target().toString == "Target", "targeted")
}
