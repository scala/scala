package scala.tools.nsc;

mixin class EvalLoop {

  def prompt: String;

  def loop(action: (String) => Unit): Unit = {
    Console.print(prompt);
    val line = Console.readLine;
    if (line != null && line.length() > 0) {
      action(line);
      loop(action);
    }
  }
}
