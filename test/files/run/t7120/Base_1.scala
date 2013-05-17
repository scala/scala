// This bug doesn't depend on separate compilation,
// in the interests of minimizing the log output during
// debugging this problem, I've split the compilation.

case class Container( v: String )

trait Base[ T <: AnyRef ] {
  type UserType = T
  protected def defect: PartialFunction[ UserType, String ]
}
