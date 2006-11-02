package a;


/** This file is needed for test 'nest.scala'. It should
 *  be compiled with javac and packaged into lib/nest.jar
 */
public class nest {
    public static class best {
        public static class rest {
            public static rest test = new rest();
            public static int  x = 10;
            public int inc(int i) {
                return i + 1;
            }
        }
    }
}
