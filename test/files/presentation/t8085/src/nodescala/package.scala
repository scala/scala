import scala.concurrent.Future // <-- if you move the import *inside* the package object, then it all works fine!!

package object nodescala {
  implicit class FutureCompanionOps[T](val f: Future.type) extends AnyVal {
    def always[T](value: T): Future[T] = Promise[T].success(value).future
  }
}

