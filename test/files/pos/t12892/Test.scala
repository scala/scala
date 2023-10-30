class C {
  // error: type mismatch. found A, required Ba. scala/bug#12892
  // def t(b: Ba): Ba = b.m
  // def t(c: Ca): Ca = c.m
  // def t(d: Da): Da = d.m

  val ba: Ba = (x: Int) => x + 1
  val bb: Bb = (x: Int) => x + 1
  val bc: Bc = x => x + 1

  val ca: Ca = (x: Int) => x + 1
  val cb: Cb = (x: Int) => x + 1
  val cc: Cc = x => x + 1

  val da: Da = (x: Int) => x + 1
  val db: Db = (x: Int) => x + 1
  val dc: Dc = x => x + 1

  // error: incompatible type in overriding
  // class Xa extends Ba { def sam(i: Int) = i }

  class Xb extends Bb { def sam(i: Int) = i }
  class Xc extends Bc { def sam(i: Int) = i }

  // error: no implementation found for m
  // class Ya extends Ca { def sam(i: Int) = i }

  // error: no implementation found for m
  // class Yb extends Cb { def sam(i: Int) = i }

  // error: no implementation found for equals
  // class Yc extends Cc { def sam(i: Int) = i }

  // error: no implementation found for m
  // class Za extends Da { def sam(i: Int) = i }

  // error: no implementation found for m
  // class Zb extends Db { def sam(i: Int) = i }

  // error: no implementation found for equals
  // class Zc extends Dc { def sam(i: Int) = i }
}
