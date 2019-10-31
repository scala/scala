package tastytest

object TestBase extends Suite("TestBase") {

  final class Subclass extends Base with Base2 {
    override def foo = s"[foo from Subclass,${super[Base].foo}]"
  }

  test(assert((new Subclass).foo === "[foo from Subclass,foo from Base]"))

}
