// tests refchecks errors. t12892 for typer errors.
class C {
  // OK error: incompatible type in overriding
  class Xa extends Ba { def sam(i: Int) = i }

  // TODO should not compile
  class Xb extends Bb { def sam(i: Int) = i }

  // OK
  class Xc extends Bc { def sam(i: Int) = i }

  // OK error: no implementation found for m
  class Ya extends Ca { def sam(i: Int) = i }

  // OK error: no implementation found for m
  class Yb extends Cb { def sam(i: Int) = i }

  // OK error: no implementation found for equals
  class Yc extends Cc { def sam(i: Int) = i }

  // OK error: no implementation found for m
  class Za extends Da { def sam(i: Int) = i }

  // OK error: no implementation found for m
  class Zb extends Db { def sam(i: Int) = i }

  // OK error: no implementation found for equals
  class Zc extends Dc { def sam(i: Int) = i }
}
