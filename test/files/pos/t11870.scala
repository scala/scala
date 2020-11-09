class Annot(arg: Any) extends scala.annotation.StaticAnnotation

abstract class Demo1 {
  @Annot(x)
  def x: String = "foo"
}

abstract class Demo2 {
  @Annot(y)
  def x: String = "foo"

  @Annot(x)
  def y: String = "bar"
}

class Annot1(arg: Any) extends scala.annotation.StaticAnnotation
class Annot2(arg: Any) extends scala.annotation.StaticAnnotation

abstract class Demo3 {
  @Annot1(y)
  def x: String = "foo"

  @Annot2(x)
  def y: String = "bar"
}

// test annotations without argument
import scala.annotation.unchecked

class C {
  class D
}

class Test {
  locally {
    val c = new C
    import c._
    new D
  }

  locally {
    import unchecked.uncheckedStable
    @uncheckedStable def c = new C
    import c._
    new D
  }

  locally {
    @unchecked.uncheckedStable def c = new C
    import c._
    new D
  }

  locally {
    import unchecked.{uncheckedStable => uc}
    @uc def c = new C
    import c._
    new D
  }
}
