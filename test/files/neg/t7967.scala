
trait B
trait C {self: B =>}

object Test {
  new C {} // fails
  type CC = C
  new CC {} // should fail, doesn't
}
