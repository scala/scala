class Foo(flag: Boolean) {
    val classpath = 
        if (flag)
            new AClasspath
        else
            new BClasspath
}

class AClasspath extends MergedClasspath[A]

class BClasspath extends MergedClasspath[B]

abstract class MergedClasspath[T]

class A
class B
