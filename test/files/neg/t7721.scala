import scala.language.reflectiveCalls

trait A {
  trait Concrete { def conco: Int = 1 }
  type Foo <: { def bippy: Int }
  type Bar <: { def barry: Int }

  implicit def barTag: scala.reflect.ClassTag[Bar]

  def f1(x: Any) = x match {
    case x: Foo with Concrete => x.bippy + x.conco
    case _                    => -1
  }
  def f2(x: Any) = x match {
    case x: Concrete with Foo => x.bippy + x.conco
    case _                    => -1
  }
  def f3(x: Any) = x match {
    case x: Foo with Bar => x.bippy + x.barry
    case _               => -1
  }
  def f4(x: Any) = x match {
    case x: (Foo @unchecked) => x.bippy  // warns, suppressed
    case _                   => -1
  }
  def f5(x: Any) = x match {
    case x: (Bar @unchecked) => x.barry // warns (but about the "outer reference"), suppressed
    case _      => -1
  }
}

trait B extends A {
  type Foo <: { def bippy: Int ; def dingo: Int }
  type Bar <: { def barry: Int ; def bongo: Int }

  override implicit def barTag: scala.reflect.ClassTag[Bar]

  override def f1(x: Any) = x match {
    case x: Foo with Concrete => x.bippy + x.dingo + x.conco
    case _                    => -1
  }
  override def f2(x: Any) = x match {
    case x: Concrete with Foo => x.bippy + x.dingo + x.conco
    case _                    => -1
  }
  override def f3(x: Any) = x match {
    case x: Foo with Bar with Concrete => x.bippy + x.barry + x.dingo + x.conco + x.bongo
    case _                             => -1
  }
  override def f4(x: Any) = x match {
    case x: (Foo @unchecked) => x.bippy + x.dingo // warns, suppressed
    case _                   => -1
  }
  override def f5(x: Any) = x match {
    case x: (Bar @unchecked) => x.barry + x.bongo // warns (but about the "outer reference"), suppressed
    case _                   => -1
  }
}

object Test {
  abstract class Base extends A {
    trait Foo {
      def bippy = 2
      def dingo = 3
    }
    trait Bar {
      def barry = 2
      def bongo = 3
    }
    implicit def barTag: scala.reflect.ClassTag[Bar] = scala.reflect.ClassTag(classOf[Bar])

    def run() {
      println("f1")
      wrap(f1(new Concrete {}))
      wrap(f1(new Foo {}))
      wrap(f1(new Bar {}))
      wrap(f1(new Foo with Concrete {}))
      wrap(f1(new Concrete with Foo {}))

      println("\nf2")
      wrap(f2(new Concrete {}))
      wrap(f2(new Foo {}))
      wrap(f2(new Bar {}))
      wrap(f2(new Foo with Concrete {}))
      wrap(f2(new Concrete with Foo {}))
      wrap(f2(new Bar with Concrete {}))
      wrap(f2(new Concrete with Bar {}))
      wrap(f2(new Concrete with Foo with Bar {}))
      wrap(f2(new Foo with Bar with Concrete {}))

      println("\nf3")
      wrap(f3(new Concrete {}))
      wrap(f3(new Foo {}))
      wrap(f3(new Bar {}))
      wrap(f3(new Foo with Concrete {}))
      wrap(f3(new Concrete with Foo {}))
      wrap(f3(new Bar with Concrete {}))
      wrap(f3(new Concrete with Bar {}))
      wrap(f3(new Concrete with Foo with Bar {}))
      wrap(f3(new Foo with Bar with Concrete {}))

      println("\nf4")
      wrap(f4(new Concrete {}))
      wrap(f4(new Foo {}))
      wrap(f4(new Bar {}))
      wrap(f4(new Foo with Concrete {}))
      wrap(f4(new Concrete with Foo {}))
      wrap(f4(new Bar with Concrete {}))
      wrap(f4(new Concrete with Bar {}))
      wrap(f4(new Concrete with Foo with Bar {}))
      wrap(f4(new Foo with Bar with Concrete {}))

      println("\nf5")
      wrap(f5(new Concrete {}))
      wrap(f5(new Foo {}))
      wrap(f5(new Bar {}))
      wrap(f5(new Foo with Concrete {}))
      wrap(f5(new Concrete with Foo {}))
      wrap(f5(new Bar with Concrete {}))
      wrap(f5(new Concrete with Bar {}))
      wrap(f5(new Concrete with Foo with Bar {}))
      wrap(f5(new Foo with Bar with Concrete {}))
    }
  }

  object ao extends Base
  object bo extends Base with B

  private def wrap(body: => Any) {
    try println(body)
    catch { case ex: NoSuchMethodException => println(ex) }
  }

  def main(args: Array[String]) {
    ao.run()
    bo.run()
  }
}

// java.lang.NoSuchMethodException: Test$$anon$1.bippy()