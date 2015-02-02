class Attr { type V ; class Val }
class StrAttr extends Attr { type V = String }
class BoolAttr extends Attr { type V = Boolean }
 
object Main {
  def f(x: Attr) = x match {
    case v: StrAttr  => new v.Val
    case v: BoolAttr => new v.Val
  }
}
