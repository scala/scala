//> using options -Werror

import concurrent._, ExecutionContext.Implicits._

class C {
  def f(): Unit = {
    Future(42)
  }
  def g(): Unit = {
    Future(42)
    true
  }
}
