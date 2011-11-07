
object Test extends App {


  // test that finally is not covered by any exception handlers.
  def throwCatchFinally {
    try {
      bar
    } catch {
      case e => println(e)
    }
  }
  
  // test that finally is not covered by any exception handlers.
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

  // return in catch (finally is executed)
  def retCatch {      
    try {
      throw new Exception
    } catch {
      case e =>
        println(e);
        return
    } finally println("in finally")
  }

  // throw in catch (finally is executed, exception propagated)
  def throwCatch {      
    try {
      throw new Exception
    } catch {
      case e =>
        println(e);
        throw e
    } finally println("in finally")
  }

  // return inside body (finally is executed)
  def retBody {      
    try {
      return
    } catch {
      case e =>
        println(e);
        throw e
    } finally println("in finally")
  }

  // throw inside body (finally and catch are executed)
  def throwBody {
    try {
      throw new Exception
    } catch {
      case e =>
        println(e);
    } finally println("in finally")
  }

  // return inside finally (each finally is executed once)
  def retFinally {
    try {
      try println("body")
      finally {
        println("in finally 1")
        return
      } 
    } finally println("in finally 2")
  }


  // throw inside finally (finally is executed once, exception is propagated)
  def throwFinally {
    try {
      try println("body")
      finally {
        println("in finally")
        throw new Exception
      }
    } catch {
      case e => println(e)
    }
  }

  // nested finallies with return value
  def nestedFinalies: Int = 
    try {
      try {
        return 10
      } finally {
        try { () } catch { case _ => () }
        println("in finally 1")
      }
    } finally {
      println("in finally 2")
    }

  def test[A](m: => A, name: String) {
    println("Running %s".format(name))
    try {
      m
    } catch {
      case e => println("COUGHT: " + e)
    }
    println("-" * 40)
  }

  test(throwCatchFinally, "throwCatchFinally")
  test(retCatch, "retCatch")
  test(throwCatch, "throwCatch")
  test(retBody, "retBody")
  test(throwBody, "throwBody")
  test(retFinally, "retFinally")
  test(throwFinally, "throwFinally")
  test(nestedFinalies, "nestedFinalies")
}
