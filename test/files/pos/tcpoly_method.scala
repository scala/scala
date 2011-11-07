trait Iterable[m[+x], +t] {
  def flatMap[resColl[+x] <: Iterable[resColl, x], s](f: t => resColl[s]): resColl[s]
  
  def foo[a[x]] = "a"
  val x = foo[List]
}
