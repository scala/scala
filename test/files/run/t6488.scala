import sys.process._
object Test {
  // Program that prints "Success" if the command was successfully run then destroyed
  // It will silently pass if the command "/bin/ls" does not exist
  // It will fail due to the uncatchable exception in t6488 race condition
  def main(args: Array[String]) {
    try Process("/bin/ls").run(ProcessLogger { _ => () }).destroy
    catch { case _ => () }
    println("Success")
  }
}
