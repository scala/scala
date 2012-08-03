class Test {
  def newComponent(u: Universe): u.Component = ???

  class Universe { self =>
    class Component

    newComponent(this): this.Component // error, but should be fine since this is a stable reference
    newComponent(self): self.Component // error, but should be fine since this is a stable reference
    newComponent(self): this.Component // error, but should be fine since this is a stable reference
    newComponent(this): self.Component // error, but should be fine since this is a stable reference

    val u = this
    newComponent(u): u.Component // ok
  }
}