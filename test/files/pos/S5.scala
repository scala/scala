/* Here's a fragment of a Scala encoding for the Keris module system;
** the compiler claims:
**
** S5.scala:28: value n in class N of type N.this._N.n
** cannot override value n in class M of type M.this._N.n
**        val system = new M() with N() {}
**                         ^
** To me it seems like the code is perfectly fine...
*/
abstract class M() {
    val _N: N;
    val n: _N.n;
    val _M: M = this;
    val m: _M.m = new _M.m();
    class m() {
        // module body of M
    }
}
trait N {
    val _N: N = this;
    val n: _N.n = new _N.n();
    val _M: M;
    val m: _M.m;
    class n() {
        // module body of N
    }
}
object O {
    val system = new M() with N {}
}
