//> using options -Yimports:scala,scala.Predef
//
import Predef.{any2stringadd => _, _}

class classic {
  def f[A](x: A) = x + 42
}
