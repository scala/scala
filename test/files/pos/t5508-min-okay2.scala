trait TopTrait { // must be nested and a trait
  private[this] val _st : Int = 0 // crashes if TopTrait is not top level
  val escape = { () => _st }
}
