/** Test virtualization of `new Struct { val x_i = v_i }` and subsequent selects on new's result.
 *
 * The test case is a dummy implementation of an query language DSL.
 * A full-blown implementation would make use of our LMS framework to reify the DSL program more precisely,
 * to analyse it and generate queries.
 */
object Test extends EmbeddedControls with App {
  // staging: a Rep[T] is a representation of something that yields a T
  // a typical example of such a representation is an expression tree that computes something of type T
  trait Rep[x] {
    // defined as a member of Rep for convenience, can also be pimped on when Rep is an abstract type
    def selectDynamic[T](n: String): Rep[T] = error("")
  }

  // representation of a statically-known constant
  case class Const[T](x: T) extends Rep[T]

  // automatically lift strings into their representations
  implicit def liftString(x: String): Rep[String] = Const(x)

  implicit def strRepOps(x: Rep[String]) = new {
    // the query below uses this operation
    def <>(y: Rep[String]): Rep[Boolean] = Const(false)
  }

  // to represent the self/this reference in a reified object creation
  case class Self[T] extends Rep[T]

  // this method is called by the virtualizing compiler
  def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = {
    val me = new Self[T]
    new Obj(me, args map {case (n, b, rhs) => (n, rhs(me))} toMap)
  }

  class Obj[T](self: Rep[T], fields: Map[String, Rep[_]]) extends Rep[T] {
    override def selectDynamic[T](n: String): Rep[T] = {
      val res = fields(n)
      println(self +" DOT "+ n + " = "+ res)
      res.asInstanceOf[Rep[T]]
    }
  }

  class Result extends Struct

  case class LineItem(customerName: String)

  val lineItems = List(LineItem("me"), LineItem("that other guy"))

  trait FilterOps[T] {
    def Where(f: Rep[T] => Rep[Boolean]): List[Rep[T]]
  }
  implicit def filterOps[T](x: List[Rep[T]]) = new FilterOps[T] {
    def Where(f: Rep[T] => Rep[Boolean]): List[Rep[T]] = {x map f; x} // just run the function so we get some output
  }

  lineItems map (e => new Result { val customerName = e.customerName }) Where (_.customerName <> "me")
/* this becomes:
 filterOps(lineItems.map{(e: Test.LineItem) => __new[Result{val customerName: String}](("customerName", self => liftString(e.customerName)))}).Where{
  ((x$1: Rep[Result{val customerName: String}]) => strRepOps(x$1.selectDynamic[String]("customerName")).<>(liftString("me")))} */
}
