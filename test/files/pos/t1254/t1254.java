/* Taken from ticket #1254. Tests Java signatures in mirror classes and that
   Nothing is translated to Nothing$.
*/

import scala.None;

// This compiles with javac but fails with Eclipse java compiler:
// 'The type scala.Nothing cannot be resolved. It is indirectly referenced from required .class files'
class NothingBug3 {
    public NothingBug3() {
	scala.Option<?> o = scala.None$.MODULE$;

        test(o);
        None.toLeft(new scala.runtime.AbstractFunction0<Integer>() { 
                public Integer apply() { return 0; }
            });
    }

    public <T>void test(scala.Option<T> f) {}
}

// This compiles with javac but fails with Eclipse java compiler:
// 'The type scala.Nothing cannot be resolved. It is indirectly referenced from required .class files'
class NothingBug4 {
    public NothingBug4() {
	scala.Option o = scala.None$.MODULE$;
    }
}
