// #266, yee ha

trait O {
  self: Test.type =>
  
  Nil foreach identity
  
  def f = (1 to 10).toList map identity
}

object Test extends O {
  def main(args: Array[String]): Unit = {
    assert(f.sum == 55)
  }
}

// Don't lose this part, it's what (formerly) crashes.
// For some reason the one actually mixed in does not.
object Pip

trait P { self: Pip.type =>
  Nil foreach identity
}