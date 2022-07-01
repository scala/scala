package scala

import scala.tools.testkit.RunTesting

import org.junit.Test

class TypeInferenceTest extends RunTesting {
  @Test def testBasicInference(): Unit = {
    runner.run("""class RS[I]() {

  def map[O](value: ScopedValue[I, O]): RS[O] = ???
}

trait ScopedValue[I, O] { }

object Test {

  implicit def value[I, O](func: (I) => O): ScopedValue[I, O] =
    new ScopedValue[I, O] { }
  
  val exec = new RS[Long]()
  val set = exec.map[Long] { (l: Long) => l }
}""")
  }

  @Test def testUnrestrictedImplicit(): Unit = {
    runner.run("""class RS[I]() {

  def map[O](value: ScopedValue[I, O]): RS[O] = ???
}

trait ScopedValue[I, O] { }

object Test {

  implicit def value[I, O](func: (I) => O): ScopedValue[I, O] =
    new ScopedValue[I, O] { }
  
  val exec = new RS[Long]()
  val set = exec.map[Long] { l => l }
}""")
  }
}
