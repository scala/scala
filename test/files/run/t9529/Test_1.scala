import java.lang.reflect._
import anns._

class A
@javax.annotation.Resource(name = "B") class B
@Ann_0(name = "C", value = "see") class C
@Ann_0(name = "D", value = "dee") @Ann_0(name = "D", value = "dye") class D

class Test @Ann_0(name = "<init>", value = "constructor") @Ann_0(name = "<init>", value = "initializer") () {
  @Ann_0(name = "x", value = "eks") val x = 1
  @Ann_0(name = "y", value = "why") @Ann_0(name = "y", value = "wye") val y = 2

  @Ann_0(name = "t", value = "tee") def t = 1
  @Ann_0(name = "u", value = "you") @Ann_0(name = "u", value = "yew") def u = 2

  def meh(
    @Ann_0(name = "1", value = "one") `1`: Int,
    @Ann_0(name = "2", value = "two") @Ann_0(name = "2", value = "tew") `2`: Int,
  ) = ()

  // todo: annotations on types
  // todo? annotaitons on packages
}

object Test extends App {
  val cls_test = classOf[Test]

  prints {
    List(classOf[A], classOf[B], classOf[C], classOf[D])
      .map(cls => s"${cls.getName}: ${anns(cls)}")
  }

  prints {
    List("x", "y")
      .map(cls_test.getDeclaredField)
      .map(f => s"${f.getName}: ${anns(f)}")
  }

  prints {
    List("t", "u")
      .map(cls_test.getDeclaredMethod(_))
      .map(m => s"${m.getName}: ${anns(m)}")
  }

  prints {
    cls_test
      .getDeclaredMethod("meh", classOf[Int], classOf[Int])
      .getParameters.toList
      .map(p => s"${p.getName}: ${anns(p)}")
  }

  println {
    anns(cls_test.getConstructor()).map(_.toString)
  } ; println()

  def anns(ae: AnnotatedElement) =
    ae.getAnnotations.toList.filterNot(_.isInstanceOf[reflect.ScalaSignature])
  def prints(l: List[String]) = { println(l mkString "\n") ; println() }
}