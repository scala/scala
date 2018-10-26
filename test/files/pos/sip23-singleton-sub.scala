object Test {
  val a: Singleton = 1
  val b: Singleton = 1L
  val c: Singleton = 1.0
  val d: Singleton = 1.0F
  val e: Singleton = true
  val f: Singleton = 'c'
  val g: Singleton = "foo"

  implicitly[1 <:< Singleton]
  implicitly[1L <:< Singleton]
  implicitly[1.0 <:< Singleton]
  implicitly[1.0F <:< Singleton]
  implicitly[true <:< Singleton]
  implicitly['c' <:< Singleton]
  implicitly["foo" <:< Singleton]
}
