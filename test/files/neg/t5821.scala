import SthImportant._

class Bar

class Foo2 {
  type Sth = Array[Bar]
  def foo(xs: Sth): Bar = if ((xs eq null) || (xs.length == 0)) null else xs(0)
}
