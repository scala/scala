object Test {
  trait Command
  object Command {
    sealed trait Execution extends Command
  }
   
  case class Buzz() extends Command.Execution
  case class Door() extends Command.Execution

  def foo(cmd: Command.Execution) = cmd match {
    case x @ (_: Buzz) => ???
    case x @ (_: Door) => ???
  }
}
