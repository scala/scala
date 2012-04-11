package scala.concurrent


//Looks awesome, but what does it do?
trait Task[+T] {

  def start(): Unit

  def future: Future[T]

}


