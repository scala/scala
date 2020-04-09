
import scala.tools.testkit.AssertUtil.assertThrows
import scala.util.Using
import scala.util.control.NonFatal

object Test1 {

  object WeekDays extends Enumeration  {
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
  }

  def isWorkingDay(d: WeekDays.Value) = d != WeekDays.Sat && d != WeekDays.Sun

  def run: Int = WeekDays.values.filter(isWorkingDay).size
}

object Test2 {

  object ThreadState extends Enumeration {
    val New          = Value("NEW")
    val Runnable     = Value("RUNNABLE")
    val Blocked      = Value("BLOCKED")
    val Waiting      = Value("WAITING")
    val TimedWaiting = Value("TIMED_WAITING")
    val Terminated   = Value("TERMINATED")
  }

  def run: Int = {
    // immutable.SortedSet[Test2.ThreadState.Value]
    val filtered = for (s <- ThreadState.values if s.id != 0) yield s
    filtered.size
  }
}

object Test3 {

  object Direction extends Enumeration {
    val North = Value("North")
    val South = Value("South")
    val East  = Value("East")
    val West  = Value("West")
  }

  def run: Int = {
    val filtered = for (d <- Direction.values if d.toString() startsWith "N") yield d
    filtered.size
  }
}

object Test4 {

  object Direction extends Enumeration {
    val North = Value("North")
    val South = Value("South")
    val East  = Value("East")
    val West  = Value("West")
  }

  def run(): Unit = {
    val dir = Direction.withName("North")
    assert(dir.toString == "North")
    assertThrows[NoSuchElementException](Direction.withName("Nord"), _ == "No value found for 'Nord'")
  }
}

object Test5 {

  object D1 extends Enumeration(0) {
    val North, South, East, West = Value
  }

  object D2 extends Enumeration(-2) {
    val North, South, East, West = Value
  }

  object WeekDays extends Enumeration  {
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
  }

  def run(): Unit = {
    val s1 = D1.ValueSet(D1.North, D1.East)
    val s2 = D2.North + D2.East
    println(s1)
    println(s2)
    println(s1 + D1.West)
    println(s2 + D2.West)
    println(s1.toBitMask.map(_.toBinaryString).toList)
    println(s2.toBitMask.map(_.toBinaryString).toList)
    println(D1.ValueSet.fromBitMask(s1.toBitMask))
    println(D2.ValueSet.fromBitMask(s2.toBitMask))
    println(WeekDays.values.range(WeekDays.Tue, WeekDays.Sat): WeekDays.ValueSet)
  }
}

object SerializationTest {
  object Types extends Enumeration { val X, Y = Value }
  class A extends Serializable { val types = Types.values }
  class B extends Serializable { val types = Set(Types.X, Types.Y) }

  def serialize(obj: AnyRef): Unit = {
    import java.io._
    val baos = new ByteArrayOutputStream()
    Using.resource(new ObjectOutputStream(baos))(_.writeObject(obj))
    val bais = new ByteArrayInputStream(baos.toByteArray)
    Using.resource(new ObjectInputStream(bais))(_.readObject())
    ()
  }

  def run(): Unit = {
    serialize(new A())
    serialize(new B())
  }
}

object Test {

  def check_success(name: String, closure: => Int, expected: Int): Unit = {
    val title = s"test $name"
    try {
      val actual: Int = closure
      if (actual == expected)
        println(s"$title was successful")
      else
        println(s"$title failed: expected $expected, found $actual")
    } catch {
      case NonFatal(exception) =>
        println(s"$title raised exception $exception")
        exception.printStackTrace()
    }
  }
  def check_success(name: String, closure: => Unit): Unit = {
    println(s"test $name")
    closure
  }

  def main(args: Array[String]): Unit = {
    check_success("Test1", Test1.run, 5)
    check_success("Test2", Test2.run, 5)
    check_success("Test3", Test3.run, 1)
    check_success("Test4", Test4.run())
    check_success("Test5", Test5.run())
    check_success("SerializationTest", SerializationTest.run())
  }
}
