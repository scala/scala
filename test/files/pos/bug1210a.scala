object Test {
  def id[T](f: T => T): T = error("bla")

  abstract class M[Settings] {
  	type selfType = M[Settings]

    val v: selfType = id[M.this.selfType](x => x.v)
  }
}
