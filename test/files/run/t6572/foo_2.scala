//package foo

import bar._

class FooProvider extends IntProvider {
  def int = 3
}

class Wrapper(users: DefaultIntBase[FooProvider]) {
  final def user(userId: Int) = users.get(userId)
}

object Test {
  def main(args: Array[String]) {
    new Wrapper(new DefaultIntBase)
  }
}