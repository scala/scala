package scala.runtime.matching ;

abstract class PatternTests extends Function2[Int,Any,Boolean]{
  def apply(i:Int, inp:Any): Boolean;
}
