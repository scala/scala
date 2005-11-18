package scala.tools.nsc;

import java.io._;

trait EvalLoop {

  def prompt: String;

  def loop(action: (String) => Unit): unit = {
    val in = new BufferedReader(new InputStreamReader(System.in));
    System.out.print(prompt);
    var line = in.readLine();
    while (line != null && line.length() > 0) {
      action(line);
      System.out.print(prompt);
      line = in.readLine();
    }
  }
}
