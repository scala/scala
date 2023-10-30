// tests typer errors. t12892b for refchecks errors.
class C {
  // error: type mismatch. found A, required Ba. scala/bug#12892
  def t(b: Ba): Ba = b.m
  def t(c: Ca): Ca = c.m
  def t(d: Da): Da = d.m

  // ok
  val ba: Ba = (x: Int) => x + 1
  val bb: Bb = (x: Int) => x + 1
  val bc: Bc = x => x + 1

  // ok
  val ca: Ca = (x: Int) => x + 1
  val cb: Cb = (x: Int) => x + 1
  val cc: Cc = x => x + 1

  // ok
  val da: Da = (x: Int) => x + 1
  val db: Db = (x: Int) => x + 1
  val dc: Dc = x => x + 1
}
