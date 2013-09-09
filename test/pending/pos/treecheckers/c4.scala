sealed trait Message[+A]
class Script[A] extends Message[A] {
  def iterator: Iterator[Message[A]] = ???
}

trait Test4[A] {
  def f(cmd: Message[A]): Iterator[A] = cmd match { case s: Script[t] => s.iterator flatMap f }
  def g(cmd: Message[A]) = cmd match { case s: Script[t] => s }
}
