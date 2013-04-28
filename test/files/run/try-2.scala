/*
 * Test different variants of the try-catch block.
 *
 */


object Test {


  def tryAllUnit: Unit =
    try {
      throw new Error();
    }
    catch {
      case _: Throwable => Console.println("exception happened\n");
    }

  def tryUnitAll: Unit =
    try {
      Console.println("Nothin");
    } catch {
      case _: Throwable => sys.error("Bad, bad, lama!");
    }

  def tryAllAll: Unit =
    try {
      throw new Error();
    } catch {
      case _: Throwable => sys.error("Bad, bad, lama!");
    }

  def tryUnitUnit: Unit =
    try {
      Console.println("Nothin");
    } catch {
      case _: Throwable => Console.println("Nothin");
    }

  def tryIntUnit: Unit =
    try {
      10;
    } catch {
      case _: Throwable => Console.println("Huh?");
    }


  def execute(f: => Unit) = try {
    f;
  } catch {
    case _: Throwable => ();
  }


  def main(args:Array[String]): Unit = {
    execute(tryAllUnit);
    execute(tryUnitAll);
    execute(tryAllAll);
    execute(tryUnitUnit);
    execute(tryIntUnit);
 }
}
