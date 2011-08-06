class ClassWithSugar(val x: Int = 123) {
}

class ClassWithoutSugar {
  def this(x: Int = 456) = this()
}
