class ImplicitsPrivate {
  private implicit class BarExtender(bar: Int)
}

class TestPrivate extends ImplicitsPrivate {
  override implicit def BarExtender(bar: Int) = super.BarExtender(bar) // error
}