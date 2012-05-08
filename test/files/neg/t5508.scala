object Test {
  trait NestedTrait { // must be nested and a trait
    private[this] val _st : Int = 0 // must be private[this]
    val escape = { () => _st }
  }
}
