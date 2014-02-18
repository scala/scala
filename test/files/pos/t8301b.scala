// cf. pos/t8300-patmat.scala
trait Universe {
  type Name >: Null <: AnyRef with NameApi
  trait NameApi
 
  type TermName >: Null <: TermNameApi with Name
  trait TermNameApi extends NameApi
}
 
object Test extends App {
  val u: Universe = ???
  import u._
 
  val ScalaName: TermName = ???
  locally {
    
    ??? match {
      case Test.ScalaName => ???
    }
    import Test.ScalaName._

    ??? match {
      case ScalaName => ???
    }
    import ScalaName._

    // both the pattern and import led to
    // stable identifier required, but SN found. Note that value SN 
    // is not stable because its type, Test.u.TermName, is volatile.
    val SN = ScalaName
    ??? match {
      case SN => ???
    }
    import SN._
  }
}
