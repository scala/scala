/* I was wondering for a long time what types x and y have;
** the compiler claims: z.Inner (see commented out line)
** This is strange because z is not in scope.
** Furthermore, compilation of this class yields the message: (why?)
**
** S2.scala:16: illegal cyclic reference involving value t
**         def t = foo(x, y);
**                    ^
*/
object M {
    def foo[T](x: T, y: T): T = x;
    class S2() {
        class Inner() extends S2() {}
        def x = { val z = new S2(); new z.Inner(); }
        def y = { val z = new S2(); new z.Inner(); }
        def t = foo(x, y);
        //def testType: Inner = x;
    }
}
