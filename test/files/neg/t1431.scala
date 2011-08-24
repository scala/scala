object Bug_New {
  trait MyTrait {
    type Alpha
    def the_value : Alpha
    class Factory() {def value : Alpha = the_value}
  }

  def fun[X<:MyTrait with Singleton]() = new X#Factory().value
}

