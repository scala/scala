trait Proc { def apply(): Unit }
class Test {
  val initCode = List[Proc]()
  initCode foreach { proc => proc() }

}
