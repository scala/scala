package scala

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class PartialFunctionSerializationTest {
  val pf1: PartialFunction[Int, Int] = { case n if n > 0 => 1 }
  val pf2: PartialFunction[Int, Int] = { case n if n <= 0 => 2 }


  private def assertSerializable[A,B](fn: A => B): Unit = {
    import java.io._
    new ObjectOutputStream(new ByteArrayOutputStream()).writeObject(fn)
  }

  @Test def canSerializeLiteral = assertSerializable(pf1)

  @Test def canSerializeLifted = assertSerializable(pf1.lift)

  @Test def canSerializeOrElse = assertSerializable(pf1 orElse pf2)

  @Test def canSerializeUnlifted = assertSerializable(Function.unlift((x: Int) => Some(x)))

  @Test def canSerializeAndThen = assertSerializable(pf1.andThen((x: Int) => x))

  @Test def canSerializeEmpty = assertSerializable(PartialFunction.empty)
}
