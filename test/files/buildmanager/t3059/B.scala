abstract class B {
    private def getFoo = 12
    lazy val foo: Int = getFoo
}
