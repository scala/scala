// tests refchecks errors. t12892 for typer errors.
class C {
  // error: type mismatch. found A, required Ba. scala/bug#12892
  // def t(b: Ba): Ba = b.m
  // def t(c: Ca): Ca = c.m
  // def t(d: Da): Da = d.m

  // error: incompatible type in overriding
  class Xa extends Ba { def sam(i: Int) = i }

  // ok
  class Xb extends Bb { def sam(i: Int) = i }
  class Xc extends Bc { def sam(i: Int) = i }

  // error: no implementation found for m
  class Ya extends Ca { def sam(i: Int) = i }

  // error: no implementation found for m
  class Yb extends Cb { def sam(i: Int) = i }

  // error: no implementation found for equals
  class Yc extends Cc { def sam(i: Int) = i }

  // error: no implementation found for m
  class Za extends Da { def sam(i: Int) = i }

  // error: no implementation found for m
  class Zb extends Db { def sam(i: Int) = i }

  // error: no implementation found for equals
  class Zc extends Dc { def sam(i: Int) = i }
}
