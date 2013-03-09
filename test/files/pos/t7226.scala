trait HK {
  type Rep[X]

  // okay
  def unzip2[A, B](ps: Rep[List[(A, B)]])
  unzip2(null.asInstanceOf[Rep[List[(Int, String)]]])

  // okay
  def unzipHK[A, B, C[_]](ps: Rep[C[(A, B)]])
  unzipHK(null.asInstanceOf[Rep[List[(Int, String)]]])

  def unzipHKRet0[A, C[_]](ps: C[A]): C[Int]
  def ls: List[String]
  unzipHKRet0(ls)

  // fail
  def unzipHKRet[A, C[_]](ps: Rep[C[A]]): Rep[C[Int]]
  def rls: Rep[List[String]]
  unzipHKRet(rls)
}

trait HK1 {
  type Rep[A]
  def unzip1[A, B, C[_]](ps: Rep[C[(A, B)]]): (Rep[C[A]], Rep[C[B]])
  def doUnzip1[A, B](ps: Rep[List[(A, B)]]) = unzip1(ps)
}
