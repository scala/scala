trait Setting {
  type T
  def value: T
}

object Test {
  def test(x: Some[Setting]) = x match {
    case Some(dep) => Some(dep.value) map (_ => true)
  }
}
