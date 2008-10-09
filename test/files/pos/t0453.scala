object Test {
  val foo = new {
    trait Bar
    def l () : Bar = { new Bar {} }
  }
}
