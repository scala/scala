object Test {
  val x: Int = Interface.staticMethod()
}

class C extends Interface // expect no errors about unimplemented members.
