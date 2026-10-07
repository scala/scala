//> using options -Werror -Xlint

object Main {
  def callFunction(function: PartialFunction[Unit, Unit]): Unit = function(???)
  def main(arguments: Array[String]): Unit = callFunction { _ => }
}
