package example

class D extends C {
  override def hi(): Int = {
    super.hi() + 1
  }
}
