//
//> using options -Xlint:unit-special -Werror

// warn once
final class C[@specialized A, @specialized B](a: A, b: B)

// explicit Unit is ok
final class D[@specialized(Specializable.Arg) A](a: A, u: Unit)

final class F[@specialized A](f: concurrent.Future[A]) {
  def result: A = concurrent.Await.result(f, concurrent.duration.Duration.Inf)
}
