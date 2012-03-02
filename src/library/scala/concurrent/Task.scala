package scala.concurrent



trait Task[+T] {

  def start(): Unit

  def future: Future[T]

}


