
//> using options -Werror -Wunused:params

trait T {
  def f(implicit i: Int) = i
  def g(j: Int) = j + f
  def k(j: Int) = { val x = j + f ; 42 }
}

/*
t11643.scala:6: error: could not find implicit value for parameter i: Int
  def g(j: Int) = j + f
                      ^
t11643.scala:6: warning: parameter value j in method g is never used
  def g(j: Int) = j + f
        ^
one warning found
one error found
 */
