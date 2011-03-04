
// test that finally is not covered by any exception handlers.
object Test extends App {
  def bar {
    try {
      println("hi")
    }
    catch {
      case e => println("SHOULD NOT GET HERE")
    }
    finally {
      println("In Finally")
      throw new RuntimeException("ouch")
    }
  }

  def m1 {
    try {
      throw new Exception
    } catch {
      case e =>
        println(e);
        return
    } finally println("in finally")
  }

  try {
    bar
  } catch {
    case e => println(e)
  }

  m1
}
