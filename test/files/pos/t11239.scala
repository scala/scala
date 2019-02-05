import scala.language.higherKinds

trait Request[F[_]]
trait Context { type F[_] }
final case class AuthedRequest[F[_], A](authInfo: A, req: Request[F])
final case class HttpRequestContext[C <: Context, Ctx](request: AuthedRequest[C#F, Ctx], context: Ctx)
