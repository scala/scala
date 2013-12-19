object Bug {
  def bar(ev: Any) = {
    trait X {
      def qux = { () => ev }
    }
    new X {}.qux()

    // workaround
    trait Y {
      val ev2 = ev // manually capture `ev` so that `ev2` is added to the trait interface.
      def qux = { () => ev2 }
    }
  }
}

