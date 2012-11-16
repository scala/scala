/*
This test is an unfortunate hack suggested here:
https://groups.google.com/d/msg/scala-internals/FOzNXVoSck8/jkgx-Nz3FsMJ

What we actually want is that partest only compiles this Java file with scalac,
but not javac. Because there seems to be no way to configure that, 
we place this file in files/neg/ and check for javac's failure, which signals
that this snippet went past scalac.

This test will fail when the javac used by partest will start to accept this
code (in Java 8). Then we can move this file to files/pos/ again.
*/

interface Foo {
    int foo() default Bar.bar;
}

class Bar {
    static int bar() { return 42; }
}

/*
Expected compiler error message should be along the lines of:

test/files/neg/java8syntax/DefaultMethods.java:15: cannot find symbol
symbol  : variable bar
location: class Bar
    int foo() default Bar.bar;
                         ^
test/files/neg/java8syntax/DefaultMethods.java:15: default value only allowed in an @interface member
    int foo() default Bar.bar;
        ^
2 errors
*/