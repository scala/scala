class A[@specialized(Int) T](val value: T) {
  trait B
  def useValue(x:T): Unit = ()
  useValue(value)
}
