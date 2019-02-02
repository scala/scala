
trait T {
  val f = if (_) 42 else 17

  val p = while (_) println    // weird

  val q = do println while (_) // weird

  val g = (b: () => Boolean) => while (b()) println    // less weird
}
