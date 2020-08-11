// scalac: -Werror -language:_

object Test extends AnyRef with App {
  lazy val task1 = new AnyRef {
    val value: Int = {
      println("flip pancake")
      0
    }
  }

  lazy val task2 = new AnyRef {
    val value: Int = {
      println("flop pancake")
      0
    }
  }

  def use() =
    try {
      unit(task1.value)
      unit(task2.value)
      ()
    } catch {
      case _: Throwable => ()
    }

  use()
}
