object Test {
  trait NestedTrait { // must be nested and a trait
    private val _st : Int = 0 // crashes if changed to private[this]
    val escape = { () => _st }
  }
}
