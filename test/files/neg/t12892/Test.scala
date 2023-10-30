// tests typer errors. t12892b for refchecks errors.
class C {
  // OK
  def t(b: Ba): Ba = b.m
  def t(c: Ca): Ca = c.m

  // TODO error, should compile
  def t(d: Da): Da = d.m


  // OK error: type mismatch. found Function, required Ba. Ba is not a SAM.
  val ba: Ba = (x: Int) => x + 1
  val bb: Bb = (x: Int) => x + 1

  // OK
  val bc: Bc = (x: Int) => x + 1
  val comp: java.util.Comparator[Int] = _ - _

  // OK error: type mismatch. found Function, required Ca. Ca is not a SAM.
  val ca: Ca = (x: Int) => x + 1
  val cb: Cb = (x: Int) => x + 1

  // OK
  val cc: Cc = (x: Int) => x + 1

  // TODO should not compile
  val da: Da = (x: Int) => x + 1

  // TODO should not compile
  val db: Db = (x: Int) => x + 1

  // OK
  val dc: Dc = (x: Int) => x + 1
}
