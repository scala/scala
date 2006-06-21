object test {

  def apply[a,b](f: a => b): a => b  =  { x: a => f(x) }

  def twice[a](f: a => a): a => a  =  { x: a => f(f(x)) }

  def main = apply[Int,Int](twice[Int]{x: Int => x})(1);
}

