
// test that finally is not covered by any exception handlers.
object Test extends App {
  def bar {
    try {
      println("hi")
    }
    catch {
      case e => println("GOT HERE")
    }
    finally {
      println("In Finally")
      throw new RuntimeException("ouch")
    }
  }

  try {
    bar
  } catch {
    case e => println(e)
  }
}
