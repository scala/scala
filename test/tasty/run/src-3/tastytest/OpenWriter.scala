package tastytest

open class OpenWriter[T] {

  def send(x: T): String = String.valueOf(x)

  def sendAll(xs: T*) = xs.map(send).mkString(",")

}
