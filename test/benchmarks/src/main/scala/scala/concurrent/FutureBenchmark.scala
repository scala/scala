package scala.concurrent

import scala.concurrent.duration._
import java.util.concurrent.{ TimeUnit, Executor, Executors, ExecutorService, ForkJoinPool, CountDownLatch }
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.annotations._
import scala.Try.{Success, Failure}
import scala.annotation.tailrec

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 1000)
@Measurement(iterations = 10000)
@Fork(value = 1, jvmArgsAppend = Array("-Xmx1G", "-Xms1G", "-ea", "-server", "-XX:+UseCompressedOops", "-XX:+AlwaysPreTouch", "-XX:+UseCondCardMark"))
@Threads(value = 1)
abstract class AbstractBaseFutureBenchmark {
  // fjp = ForkJoinPool, fix = FixedThreadPool, fie = FutureInternalExecutor, gbl = GlobalEC
  @Param(Array[String]("fjp", "fix", "fie", "gbl"))
  final var pool: String = _

  @Param(Array[String]("1"))
  final var threads: Int = _

  @Param(Array[String]("1024"))
  final var recursion: Int = _

  final var executorService: ExecutorService = _

  final var executionContext: ExecutionContext = _

  final val timeout = 60.seconds

  @Setup(Level.Trial)
  def startup: Unit = {
    val e = pool match {
      case "fjp" =>
        val fjp = new ForkJoinPool(threads)
        executorService = fjp // we want to close this
        fjp
      case "fix" =>
        val fix = Executors.newFixedThreadPool(threads)
        executorService = fix // we want to close this
        fix
      case "gbl" =>
        // make sure we set the global ec to use the number of threads the bench wants
        System.setProperty("scala.concurrent.context.minThreads", threads.toString)
        System.setProperty("scala.concurrent.context.numThreads", threads.toString)
        System.setProperty("scala.concurrent.context.maxThreads", threads.toString)
        ExecutionContext.global
      case "fie" =>
        scala.concurrent.Future.InternalCallbackExecutor.asInstanceOf[Executor]
    }

    executionContext =
      if (e.isInstanceOf[ExecutionContext]) e.asInstanceOf[ExecutionContext]
      else { // TODO: may want to extend this in the implementations directly
        new ExecutionContext with BatchingExecutor {
          private[this] final val g = e
          override final def unbatchedExecute(r: Runnable) = g.execute(r)
          override final def reportFailure(t: Throwable) = t.printStackTrace(System.err)
        }
      }
  }

  @TearDown(Level.Trial)
  final def shutdown: Unit =
    executorService = executorService match {
      case null => null
      case some =>
        try some.shutdown() finally some.awaitTermination(1, TimeUnit.MINUTES)
        null
    }
}

abstract class OpFutureBenchmark extends AbstractBaseFutureBenchmark {
  type Result = String

  final val aFailure = Failure(new Exception("a failure"))

  final val aSuccess = Success("a success")

  final val pre_s_p: Promise[Result] = Promise.fromTry(aSuccess)

  final val pre_f_p: Promise[Result] = Promise.fromTry(aFailure)

  @inline protected final def await[T](a: Future[T]): Boolean = {
    val v = a.value
    val r = if (v eq None) Await.ready(a, timeout).value else v
    r.get.getClass eq classOf[Success[T]]
  }
}

class NoopFutureBenchmark extends OpFutureBenchmark {
  @tailrec private[this] final def next(i: Int, bh: Blackhole,f: Future[Result])(implicit ec: ExecutionContext): Future[Result] =
      if (i > 0) { next(i - 1, bh, f) } else {  bh.consume(f); f }

  @Benchmark final def pre(bh: Blackhole): Boolean =
    await(next(recursion, bh, pre_s_p.future)(executionContext))

  @Benchmark final def post(bh: Blackhole): Boolean = {
    val post_p = Promise[Result]()
    val f = next(recursion, bh, post_p.future)(executionContext)
    post_p.complete(aSuccess)
    await(f)
  }
}

class MapFutureBenchmark extends OpFutureBenchmark {
  private[this] final val transformationFun = (r: Result) => r

  @tailrec private[this] final def next(i: Int, f: Future[Result])(implicit ec: ExecutionContext): Future[Result] =
      if (i > 0) { next(i - 1, f.map(transformationFun)) } else { f }

  @Benchmark final def pre(): Boolean =
    await(next(recursion, pre_s_p.future)(executionContext))

  @Benchmark final def post(): Boolean = {
    val post_p = Promise[Result]()
    val f = next(recursion, post_p.future)(executionContext)
    post_p.complete(aSuccess)
    await(f)
  }
}

class FilterFutureBenchmark extends OpFutureBenchmark {
  private[this] final val transformationFun = (r: Result) => true

  @tailrec private[this] final def next(from: Int, to: Int, f: Future[Result])(implicit ec: ExecutionContext): Future[Result] =
      if (from < to) { next(from + 1, to, f.filter(transformationFun)) } else { f }

  @Benchmark final def pre(): Boolean =
    await(next(0, recursion, pre_s_p.future)(executionContext))

  @Benchmark final def post(): Boolean = {
    val post_p = Promise[Result]()
    val f = next(0, recursion, post_p.future)(executionContext)
    post_p.complete(aSuccess)
    await(f)
  }
}

class TransformFutureBenchmark extends OpFutureBenchmark {
  private[this] final val transformationFun = (t: Try[Result]) => t

  @tailrec private[this] final def next(i: Int, f: Future[Result])(implicit ec: ExecutionContext): Future[Result] =
      if (i > 0) { next(i - 1, f.transform(transformationFun)) } else { f }

  @Benchmark final def pre(): Boolean =
    await(next(recursion, pre_s_p.future)(executionContext))

  @Benchmark final def post(): Boolean = {
    val post_p = Promise[Result]()
    val f = next(recursion, post_p.future)(executionContext)
    post_p.complete(aSuccess)
    await(f)
  }
}

class TransformWithFutureBenchmark extends OpFutureBenchmark {
  private[this] final val transformationFun = (t: Try[Result]) => Future.fromTry(t)

  @tailrec private[this] final def next(i: Int, f: Future[Result])(implicit ec: ExecutionContext): Future[Result] =
      if (i > 0) { next(i - 1, f.transformWith(transformationFun)) } else { f }

  @Benchmark final def pre(): Boolean =
    await(next(recursion, pre_s_p.future)(executionContext))

  @Benchmark final def post(): Boolean = {
    val post_p = Promise[Result]()
    val f = next(recursion, post_p.future)(executionContext)
    post_p.complete(aSuccess)
    await(f)
  }
}

class FlatMapFutureBenchmark extends OpFutureBenchmark {
  private[this] final val transformationFun = (t: Result) => Future.successful(t)

  @tailrec private[this] final def next(from: Int, to: Int, f: Future[Result])(implicit ec: ExecutionContext): Future[Result] =
      if (from < to) { next(from + 1, to, f.flatMap(transformationFun)) } else { f }

  @Benchmark final def pre(): Boolean =
    await(next(0, recursion, pre_s_p.future)(executionContext))

  @Benchmark final def post(): Boolean = {
    val post_p = Promise[Result]()
    val f = next(0, recursion, post_p.future)(executionContext)
    post_p.complete(aSuccess)
    await(f)
  }
}

class RecoverFutureBenchmark extends OpFutureBenchmark {
  private[this] final val recoverFunStdlib: PartialFunction[Throwable, Result] = { case _ => aFailure.get }

  @tailrec private[this] final def next(i: Int, f: Future[Result])(implicit ec: ExecutionContext): Future[Result] =
      if (i > 0) { next(i - 1, f.recover(recoverFunStdlib)) } else { f }

  @Benchmark final def pre(): Boolean =
    await(next(recursion, pre_f_p.future)(executionContext))

  @Benchmark final def post(): Boolean = {
    val post_p = Promise[Result]()
    val f = next(recursion, post_p.future)(executionContext)
    post_p.complete(aFailure)
    await(f)
  }
}

class RecoverWithFutureBenchmark extends OpFutureBenchmark {
  private[this] final val recoverWithFunStdlib: PartialFunction[Throwable, Future[Result]] = { case _ => pre_f_p.future }

  @tailrec private[this] final def next(i: Int, f: Future[Result])(implicit ec: ExecutionContext): Future[Result] =
      if (i > 0) { next(i - 1, f.recoverWith(recoverWithFunStdlib)) } else { f }

  @Benchmark final def pre(): Boolean =
    await(next(recursion, pre_f_p.future)(executionContext))

  @Benchmark final def post(): Boolean = {
    val post_p = Promise[Result]()
    val f = next(recursion, post_p.future)(executionContext)
    post_p.complete(aFailure)
    await(f)
  }
}


class ZipWithFutureBenchmark extends OpFutureBenchmark {
  private[this] final val transformationFun = (t1: Result, t2: Result) => t2

  @tailrec private[this] final def next(i: Int, f: Future[Result])(implicit ec: ExecutionContext): Future[Result] =
      if (i > 0) { next(i - 1, f.zipWith(f)(transformationFun)) } else { f }

  @Benchmark final def pre(): Boolean =
    await(next(recursion, pre_s_p.future)(executionContext))

  @Benchmark final def post(): Boolean = {
    val post_p = Promise[Result]()
    val f = next(recursion, post_p.future)(executionContext)
    post_p.complete(aSuccess)
    await(f)
  }
}

class AndThenFutureBenchmark extends OpFutureBenchmark {
  private[this] final val effect: PartialFunction[Try[Result], Unit] = { case t: Try[Result] => () }

  @tailrec private[this] final def next(i: Int, f: Future[Result])(implicit ec: ExecutionContext): Future[Result] =
      if (i > 0) { next(i - 1, f.andThen(effect)) } else { f }

  @Benchmark final def pre(): Boolean =
    await(next(recursion, pre_s_p.future)(executionContext))

  @Benchmark final def post(): Boolean = {
    val post_p = Promise[Result]()
    val f = next(recursion, post_p.future)(executionContext)
    post_p.complete(aSuccess)
    await(f)
  }
}

class VariousFutureBenchmark extends OpFutureBenchmark {
  final val mapFun: Result => Result = _.toUpperCase
  final val flatMapFun: Result => Future[Result] = r => Future.successful(r)
  final val filterFun: Result => Boolean = _ ne null
  final val transformFun: Try[Result] => Try[Result] = _ => throw null
  final val recoverFun: PartialFunction[Throwable, Result] = { case _ => "OK" }
  final val keepLeft: (Result, Result) => Result = (a,b) => a

  @tailrec private[this] final def next(i: Int, f: Future[Result])(implicit ec: ExecutionContext): Future[Result] =
      if (i > 0) { next(i - 1, f.map(mapFun).flatMap(flatMapFun).filter(filterFun).zipWith(f)(keepLeft).transform(transformFun).recover(recoverFun)) } else { f }

  @Benchmark final def pre(): Boolean =
    await(next(recursion, pre_s_p.future)(executionContext))

  @Benchmark final def post(): Boolean = {
    val post_p = Promise[Result]()
    val f = next(recursion, post_p.future)(executionContext)
    post_p.complete(aSuccess)
    await(f)
  }
}

class LoopFutureBenchmark extends OpFutureBenchmark {
  val depth = 50
  val size  = 2000

  final def pre_loop(i: Int)(implicit ec: ExecutionContext): Future[Int] =
    if (i % depth == 0) Future.successful(i + 1).flatMap(pre_loop)
    else if (i < size) pre_loop(i + 1).flatMap(Future.successful)
    else Future.successful(i)

  final def post_loop(i: Int)(implicit ec: ExecutionContext): Future[Int] =
    if (i % depth == 0) Future(i + 1).flatMap(post_loop)
    else if (i < size) post_loop(i + 1).flatMap(i => Future(i))
    else Future(i)


  @Benchmark final def pre(): Boolean = {
    implicit val ec = executionContext
    await(pre_s_p.future.flatMap(s => pre_loop(recursion).map(_ => s)))
  }

  @Benchmark final def post(): Boolean = {
    implicit val ec = executionContext
    val post_p = Promise[Result]()
    val f = post_p.future.flatMap(s => post_loop(recursion).map(_ => s))
    post_p.complete(aSuccess)
    await(f)
  }
}

class SequenceFutureBenchmark extends OpFutureBenchmark {
  @Benchmark final def pre(): Boolean = {
    implicit val ec = executionContext
    await(Future.sequence(1 to recursion map { _ => pre_s_p.future }))
  }

  @Benchmark final def post(): Boolean = {
    implicit val ec = executionContext
    val post_p = Promise[Result]()
    val f = Future.sequence(1 to recursion map { _ => post_p.future })
    post_p.complete(aSuccess)
    await(f)
  }
}

class FirstCompletedOfFutureBenchmark extends OpFutureBenchmark {
  @Benchmark final def pre(): Boolean = {
    implicit val ec = executionContext
    await(Future.firstCompletedOf(1 to recursion map { _ => pre_s_p.future }))
  }

  @Benchmark final def post(): Boolean = {
    implicit val ec = executionContext
    val post_p = Promise[Result]()
    val f = Future.firstCompletedOf(1 to recursion map { _ => post_p.future })
    post_p.complete(aSuccess)
    await(f)
  }
}

class CompleteFutureBenchmark extends OpFutureBenchmark {
  @tailrec private[this] final def next(i: Int, p: Promise[Result], r: Try[Result])(implicit ec: ExecutionContext): Future[Result] =
      if (i > 0) { next(i - 1, { p.tryComplete(r); p }, r) } else { p.future }

  @Benchmark final def success(): Boolean = {
    val f = next(recursion, Promise[Result](), aSuccess)(executionContext)
    await(f)
  }

  @Benchmark final def failure(): Boolean = {
    val f = next(recursion, Promise[Result](), aFailure)(executionContext)
    await(f)
  }
}

class CompleteWithFutureBenchmark extends OpFutureBenchmark {
  @tailrec private[this] final def next(i: Int, p: Promise[Result], f: Future[Result])(implicit ec: ExecutionContext): Future[Result] =
      if (i > 0) { next(i - 1, { p.tryCompleteWith(f); p }, f) } else { p.future }

  @Benchmark final def success(): Boolean = {
    val f = next(recursion, Promise[Result](), pre_s_p.future)(executionContext)
    await(f)
  }

  @Benchmark final def failure(): Boolean = {
    val f = next(recursion, Promise[Result](), pre_f_p.future)(executionContext)
    await(f)
  }
}

class CallbackFutureBenchmark extends OpFutureBenchmark {
  final class Callback(recursion: Int) extends CountDownLatch(recursion) with Function1[Try[Result], Unit] {
    override def apply(t:Try[Result]): Unit = this.countDown()
  }

  @tailrec private[this] final def next(i: Int, callback: Callback, f: Future[Result])(implicit ec: ExecutionContext): Future[Result] =
      if (i > 0) { next(i - 1, callback, { f.onComplete(callback); f }) } else { f }

  @Benchmark final def pre(): Unit = {
    val callback = new Callback(recursion)
    next(recursion, callback, pre_s_p.future)(executionContext)
    callback.await()
  }

  @Benchmark final def post(): Unit = {
    val post_p = Promise[Result]()
    val callback = new Callback(recursion)
    next(recursion, callback, post_p.future)(executionContext)
    post_p.complete(aSuccess)
    callback.await()
  }
}