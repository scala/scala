// scalac: -Xelide-below WARNING
//
import annotation._, elidable._

abstract class C {
  @elidable(INFO) val i: Int = 42
  @elidable(INFO) lazy val j: Int = 42
  @elidable(INFO) var k: Int = 42
  @elidable(INFO) def f: Int
}
@elidable(INFO) class D
