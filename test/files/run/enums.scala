//############################################################################
// Enumerations
//############################################################################
// $Id$

object Test1 {

  object WeekDays extends Enumeration  {
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
  }

  def isWorkingDay(d: WeekDays.Value) =
    ! (d == WeekDays.Sat || d == WeekDays.Sun);

  def run: Int = {
    val it = WeekDays filter (isWorkingDay);
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
    val it = for (val s <- ThreadState; s.id != 0) yield s;
    it.toList.length
  }
}

object Test3 {

  object Direction extends Enumeration("North", "South", "East", "West") {
    val North, South, East, West = Value;
  }

  def run: Int = {
    val it = for (val d <- Direction; d.toString() startsWith "N") yield d;
    it.toList.length
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
      }
    }
    Console.println;
  }

  def main(args: Array[String]): Unit = {
    check_success("Test1", Test1.run, 5);
    check_success("Test2", Test2.run, 5);
    check_success("Test3", Test3.run, 1);
    Console.println;
  }
}

//############################################################################
