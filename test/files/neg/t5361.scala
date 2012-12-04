class A {
  val x : { val self = this } = new { self => }
}
