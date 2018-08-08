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
    def foo[T <: this.type](x: T) = x;
    foo[this.type](this);
}
