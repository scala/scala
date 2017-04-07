
import annotation._, elidable._

class C {
  @elidable(INFO) val i: Int = 42
  @elidable(INFO) lazy val j: Int = 42
  @elidable(INFO) var k: Int = 42
}
@elidable(INFO) class D
