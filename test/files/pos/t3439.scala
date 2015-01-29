class Base[M](i: Int)
 
// was "implicit modifier not allowed on top level objects"
class D1()(implicit i: Int) extends Base({println(i); 0})

// what "no implicit value of type Int found"
class D2()(implicit i: Int) extends Base(implicitly[Int])


abstract class ParametricMessage[M: Manifest](msg: M) { def message = msg }
case class ParametricMessage1[M: Manifest](msg: M, p1: Class[_]) extends ParametricMessage(msg)


class Wrap {
  class Base[M](i: Int)
   
  // was "implicit modifier not allowed on top level objects"
  class D1()(implicit i: Int) extends Base({println(i); 0})

  // what "no implicit value of type Int found"
  class D2()(implicit i: Int) extends Base(implicitly[Int])


  abstract class ParametricMessage[M: Manifest](msg: M) { def message = msg }
  case class ParametricMessage1[M: Manifest](msg: M, p1: Class[_]) extends ParametricMessage(msg)
}
