class Quasi {
  import reflect.runtime.universe._

  def test: Unit = {
    (null: Any) match {
      case q"$foo($bar)" =>
    }
    ()
  }
}
