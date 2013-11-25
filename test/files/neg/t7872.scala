trait Cov[+A]
trait Inv[-A]

object varianceExploit {  
  type l[-a] = Cov[a]
  type x = {type l[-a] = Cov[a]}
  def foo[M[_]] = ()
  foo[({type l[+a] = Inv[a]})#l]
}
