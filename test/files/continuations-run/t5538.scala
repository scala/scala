import scala.util.continuations._
import scala.collection.generic.CanBuildFrom

import scala.language.{ implicitConversions }

object Test {

  class ExecutionContext

  implicit def defaultExecutionContext = new ExecutionContext

  case class Future[+T](x:T) {
    final def map[A](f: T => A): Future[A] = new Future[A](f(x))
    final def flatMap[A](f: T => Future[A]): Future[A] = f(x)
  }

  class PromiseStream[A] {
    override def toString = xs.toString

    var xs: List[A] = Nil

    final def +=(elem: A): this.type = { xs :+= elem; this }

    final def ++=(elem: Traversable[A]): this.type = { xs ++= elem; this }

    final def <<(elem: Future[A]): PromiseStream[A] @cps[Future[Any]] =
      shift { cont: (PromiseStream[A] => Future[Any]) => elem map (a => cont(this += a)) }

    final def <<(elem1: Future[A], elem2: Future[A], elems: Future[A]*): PromiseStream[A] @cps[Future[Any]] =
      shift { cont: (PromiseStream[A] => Future[Any]) => Future.flow(this << elem1 << elem2 <<< Future.sequence(elems.toSeq)) map cont }

    final def <<<(elems: Traversable[A]): PromiseStream[A] @cps[Future[Any]] =
      shift { cont: (PromiseStream[A] => Future[Any]) => cont(this ++= elems) }

    final def <<<(elems: Future[Traversable[A]]): PromiseStream[A] @cps[Future[Any]] =
      shift { cont: (PromiseStream[A] => Future[Any]) => elems map (as => cont(this ++= as)) }
  }

  object Future {

    def sequence[A, M[_] <: Traversable[_]](in: M[Future[A]])(implicit cbf: CanBuildFrom[M[Future[A]], A, M[A]], executor: ExecutionContext): Future[M[A]] =
      new Future(in.asInstanceOf[Traversable[Future[A]]].map((f:Future[A])=>f.x)(cbf.asInstanceOf[CanBuildFrom[Traversable[Future[A]], A, M[A]]]))

    def flow[A](body: => A @cps[Future[Any]])(implicit executor: ExecutionContext): Future[A] = reset(Future(body)).asInstanceOf[Future[A]]

  }

  def main(args: Array[String]) {
    val p = new PromiseStream[Int]
    println(Future.flow(p << (Future(1), Future(2), Future(3), Future(4), Future(5))))
  }
}
