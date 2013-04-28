import scala.language.dynamics
import scala.reflect.{ ClassTag, classTag }

object Util {
  def show[T](x: T): T = { println(x) ; x }
  def mkArgs(xs: Any*) = xs map { case ((k, v)) => k + "=" + v ; case x => "" + x } mkString ("(", ", ", ")")
}
import Util._

abstract class MonoDynamic extends Dynamic {
  def selectDynamic(name: String): String                           = show(this + "." + name)
  def applyDynamic(name: String)(args: Any*): String                = show(this + "." + name + mkArgs(args: _*))
  def applyDynamicNamed(name: String)(args: (String, Any)*): String = show(this + "." + name + mkArgs(args: _*))

  override def toString = (this.getClass.getName split '.').last
}

object Mono extends MonoDynamic {
  def f(s: String): String = s

  def f1 = this.bar()
  def f2 = this.bar
  def f3 = f(this.bar())
  def f4 = f(this.bar)
  def f5 = f(f(f(f(f(f(this.bar)))))) + f(f(f(f(f(f(this.baz))))))
  def f6 = f(f(f(f(f(f(this.bar(bippy = 1, boppy = 2))))))) + f(f(f(f(f(f(this.baz))))))
}

object Poly extends Dynamic {
  def selectDynamic[T: ClassTag](name: String): String = show(s"$this.$name[${classTag[T]}]")
  def applyDynamic[T: ClassTag](name: String)(args: Any*): String = show(args.mkString(s"$this.$name[${classTag[T]}](", ", ", ")"))

  def f(s: String): String = s

  def f1 = this.bar
  def f2 = this.bar[Int]
  def f3 = this.bar()
  def f4 = this.bar[Int]()
  def f5 = this.bar[Int](1, 2, 3)

  def f6 = f(f(this.bar))
  def f7 = f(f(this.bar[Int]))
  def f8 = f(f(this.bar()))
  def f9 = f(f(this.bar[Int]()))
  def f10 = f(f(this.bar[Int](1, 2, 3)))

  override def toString = "Poly"
}

object Updating extends Dynamic {
  def selectDynamic(name: String): String = show(s"$this.$name")
  def updateDynamic(name: String)(value: Any): String = show(s"$this.$name = $value")

  def f1 = this.bar
  def f2 = this.bar = "b"

  override def toString = "Updating"
}

object Nest1 extends Dynamic {
  def applyDynamic(name: String)(args: Any*): Nest2.type = Nest2

  object Nest2 extends Dynamic {
    def applyDynamicNamed(name: String)(args: (String, Any)*): Nest3.type = Nest3

    object Nest3 extends MonoDynamic {

    }
  }

  def f1 = Nest1.bip().bop(foo = "bar").bippy(1, 2, 3)
  def f2 = Nest1.bip("abc").bop(foo = 5).bippy
}

object Named extends Dynamic {
  def applyDynamic(name: String)(args: Any*): Named.type = {
    show(this + "." + name + mkArgs(args: _*))
    this
  }
  def applyDynamicNamed(name: String)(args: (String, Any)*): Named.type = {
    show(this + "." + name + mkArgs(args: _*))
    this
  }

  def f1 = this.bippy(a = 1, b = 2).boppy(c = 3, d = 4)()()(e = 5, f = 6)
  override def toString = "Named"
}

object Named2 extends Dynamic {
  def applyDynamic(name: String)(a: Any)(b: Any = "b", c: Any = "c"): Named2.type = {
    show(this + "." + name + mkArgs(a) + mkArgs(b, c))
    this
  }
  def applyDynamicNamed(name: String)(a: (String, Any))(b: (String, Any), c: (String, Any)): Named2.type = {
    show(this + "." + name + mkArgs(a) + mkArgs(b, c))
    this
  }

  def f1 = this.bippy(1)(b = "q0")
  def f2 = this.bippy(1)("q0")
  def f3 = this.bippy(1)(c = "q0")
  def f4 = this.bippy(1)("q0")
  def f5 = this.bippy(1)(c = "b", b = "c")
  def f6 = this.bippy(1)("b", "c")
  def f7 = this.bippy(1)(b = "q0").bippy(2)()
  def f8 = this.bippy(1)("q0").bippy(5)(c = "c").dingus(100)(c = "dong")
  def f9 = this.bippy(1)(b = "q0", c = "q1").hello(100)("!!", "!!")

  override def toString = "Named2"
}


object Test {
  def main(args: Array[String]): Unit = {
    {
      import Mono._
      f1 ; f2 ; f3 ; f4 ; f5
      f6
    }
    {
      import Poly._
      f1 ; f2 ; f3 ; f4 ; f5
      f6 ; f7 ; f8 ; f9 ; f10
    }
    {
      import Updating._
      f1 ; f2
    }
    {
      import Nest1._
      f1 ; f2
    }
    {
      import Named._
      f1
    }
    {
      import Named2._
      f1 ; f2 ; f3 ; f4 ; f5
      f6 ; f7 ; f8 ; f9
    }
  }
}
