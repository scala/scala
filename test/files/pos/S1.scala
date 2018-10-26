/* This is probably no bug, I just don't understand why
** type inference does not find the right instantiation of foo.
** Currently it reports:
**
** S1.scala:12: inferred type arguments [S1] do not conform to
** method foo's type parameter bounds [T <: S1.this.type]
**        foo(this);
**           ^
*/
class S1() {
  def foo[T <: this.type](x: T) = x
  def f = foo[this.type](this)
}

class S2() {
  def foo[T <: this.type](x: T) = x
  def f = foo(this)
}
/*
 *
$ scalac -d /tmp test/files/pos/S1.scala
test/files/pos/S1.scala:17: error: inferred type arguments [S2] do not conform to method foo's type parameter bounds [T <: S2.this.type]
  def f = foo(this)
          ^
test/files/pos/S1.scala:17: error: type mismatch;
 found   : S2
 required: T
  def f = foo(this)
              ^
two errors found
 */
