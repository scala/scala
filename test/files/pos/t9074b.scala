trait Echo [T] {
  def echo(t: T): Unit
}

trait IntEcho extends Echo[Int] {
  def echo(t: Int) = println(t)
}

object echo extends IntEcho
package object echo1  extends IntEcho

object App extends App {
  echo.echo(1)
  echo1.echo(1)
}
