object line1 {
  trait MyTrait
}
object line2 {
  import line2._
  class BugTest {def isTheBugHere(in: MyTrait.this.type#SomeData) = false}
}
