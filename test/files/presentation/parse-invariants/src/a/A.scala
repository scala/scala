package syntax

object Terms {
  object Literals {
    0
    0l
    0f
    0d
    0xb33f
    'c'
    "string"
    """
      multi-line
      string
    """
    'symbol
    true
    false
    null
    ()
  }

  object Patterns {
    0             match { case 0                               =>               }
    1             match { case (0 | 1)                         =>               }
    2             match { case _: Int                          =>               }
    3             match { case _                               =>               }
    Some(0)       match { case Some(0)                         =>               }
    Some(0)       match { case name @ Some(_)                  =>               }
    Some(Some(0)) match { case nested @ Some(deeper @ Some(_)) =>               }
    List(1, 2, 3) match { case unapplySeq @ List(1, 2, _*)     =>               }
    0             match { case i if i > 0                      =>               }
    List(1)       match { case _: List[t]                      => List.empty[t] }
  }

  object New {
    class Foo
    trait Bar
    new Foo
    new Foo { selfie => }
    new Foo with Bar
    new { val early = 1 } with Bar
    new { val name = "anon "}
  }

  def tuple         = (1, 'two, "three")
  def lambda        = (x: Int, y: Int) => x + y
  def lambda2       = (_: Int) + (_: Int)
  def blocks        = { { {}; {} }; {} }
  def ascription    = (1: Int)
  def select        = Nil.size
  def method1       = "s".replace("foo", "bar")
  def method2       = "s" + "s"
  def method3       = Nil.foreach { e => }
  def method4       = 1 :: 2 :: 3 :: Nil
  def if1           = if (true) true else false
  def if2           = if (true) true
  def `return`: Int = { return 0 }
  def `throw`       = throw new Exception
  def `match`       = 0 match { case 0 => case _ => }
  def `try`         = try 0 catch { case _ => } finally 0
  def `while`       = while(true) 0
  def `do while`    = do 0 while(true)
  def `for`         = for (i <- 1 to 10; if i % 2 == 0; j = i + 1) j
  def `for yield`   = for (a <- List(List(1)); b <- a; c = b * 2) yield b
  def interpolation = s"$tuple and maybe also $blocks"
  def assign        = { var x = 1; x = 2 }
  def assign2       = { object o { var x = 1 }; o.x = 2 }
  def update        = { val v = collection.mutable.Seq(1); v(0) = 2 }
  def `this`        = this
}

object Types {
  type Reference    = scala.App
  type Tuple        = (Int, String, Double)
  type Function     = (Int, String) => Double
  type Refined      = Int { val meta: Any }
  type Lambda       = ({ type F[T] = List[T] })#F[_]
  type Infix        = Int Either String
  type Application  = List[Int]
  type Existential  = List[T] forSome { type T }
  object O { type T = Int }
  type Dependent    = O.T
  class O { type T  = Int }
  type Selection    = O#T
}

object Definitions {
  private val x1 = 0
  private[this] val x2 = 0
  private[Definitions] val x3 = 0
  protected val x4 = 0
  protected[AcessManagement] val x5 = 0
  val x1 = 1
  val x2: Int = 1
  val x3, y3 = 1
  lazy val x4 = 1
  implicit val x5 = 1
  final val x6 = 1
  lazy final val x7 = 1
  val Some(x8) = Some(0)
  var x9 = 1
  var x10, y10 = 1
  var x11: Int = 1

  implicit def implicitView: Option[Int] = None
  def implicitArg1(implicit i: Int) = i + 2
  def implicitArg2[T: Fooable] = implicitly[Fooable[T]]
  def bound1[T <: Int](x: T): T = x
  def bound2[T >: Any](x: T): T = x
  def bound3[T <% Int](x: T): Int = x
  def vararg(args: Int*) = args.toList
  def sum(x: Int, y: Int) = x + y
  def multipleArgLists(x: Int)(y: Int) = x + y

  type _0 = Int
  type _1[T] = List[T]
  type _2[A, B] = Either[A, B]

  class Val(value: Int) extends AnyVal
  implicit class Impl(value: Int) { def foo = "foo" }
  abstract class Abs
  sealed class Sealed
  class Child extends Sealed
  case class Point(x: Int, y: Int)

  trait Fooable[T]
  trait Barable with Fooable[Barable]

  object Foo
  object Foo with Fooable[Foo]
  case object Zero
}

package Packages {
  package object PackageObject
  package Nested { package Deeper { } }
}
