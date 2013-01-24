//############################################################################
// Enumerations
//############################################################################

object Test1 {

  object WeekDays extends Enumeration  {
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
  }

  def isWorkingDay(d: WeekDays.Value) =
    ! (d == WeekDays.Sat || d == WeekDays.Sun);

  def run: Int = {
    val it = WeekDays.values filter (isWorkingDay);
    it.toList.length
  }
}

object Test2 {

  object ThreadState extends Enumeration {
    val New          = Value("NEW");
    val Runnable     = Value("RUNNABLE");
    val Blocked      = Value("BLOCKED");
    val Waiting      = Value("WAITING");
    val TimedWaiting = Value("TIMED_WAITING");
    val Terminated   = Value("TERMINATED");
  }

  def run: Int = {
    val it = for (s <- ThreadState.values; if s.id != 0) yield s;
    it.toList.length
  }
}

object Test3 {

  object Direction extends Enumeration {
    val North = Value("North")
    val South = Value("South")
    val East = Value("East")
    val West = Value("West")
  }

  def run: Int = {
    val it = for (d <- Direction.values; if d.toString() startsWith "N") yield d;
    it.toList.length
  }
}

object Test4 {

  object Direction extends Enumeration {
    val North = Value("North")
    val South = Value("South")
    val East = Value("East")
    val West = Value("West")
  }

  def run: Int = {
    val dir = Direction.withName("North")
    assert(dir.toString == "North")
    try {
      Direction.withName("Nord")
      assert(false)
    } catch {
      case e: Exception => /* do nothing */
    }
    0
  }
}

object Test5 {

  object D1 extends Enumeration(0) {
    val North, South, East, West = Value;
  }

  object D2 extends Enumeration(-2) {
    val North, South, East, West = Value;
  }

  object WeekDays extends Enumeration  {
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
  }

  def run {
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
    println(WeekDays.values.range(WeekDays.Tue, WeekDays.Sat))
  }
}

object SerializationTest {
  object Types extends Enumeration { val X, Y = Value }
  class A extends java.io.Serializable { val types = Types.values }
  class B extends java.io.Serializable { val types = Set(Types.X, Types.Y) }

  def serialize(obj: AnyRef) = {
    val baos = new java.io.ByteArrayOutputStream()
    val oos = new java.io.ObjectOutputStream(baos)
    oos.writeObject(obj)
    oos.close()
    val bais = new java.io.ByteArrayInputStream(baos.toByteArray)
    val ois = new java.io.ObjectInputStream(bais)
    val prime = ois.readObject()
    ois.close()
    prime
  }

  def run {
    serialize(new B())
    serialize(new A())
  }
}

//############################################################################
// Test code

object Test {

  def check_success(name: String, closure: => Int, expected: Int): Unit = {
    Console.print("test " + name);
    try {
      val actual: Int = closure;
      if (actual == expected) {
        Console.print(" was successful");
      } else {
        Console.print(" failed: expected "+ expected +", found "+ actual);
      }
    } catch {
      case exception: Throwable => {
        Console.print(" raised exception " + exception);
        exception.printStackTrace();
      }
    }
    Console.println;
  }

  def main(args: Array[String]): Unit = {
    check_success("Test1", Test1.run, 5);
    check_success("Test2", Test2.run, 5);
    check_success("Test3", Test3.run, 1);
    check_success("Test4", Test4.run, 0);
    Console.println;
    Test5.run;
    Console.println;
    SerializationTest.run;
  }
}

//############################################################################
