//############################################################################
// Bug 142
//############################################################################
// $Id$

import System.out.println;

abstract class Foo1 { class Inner; def foo: Inner; foo; }
abstract class Foo2 { class Inner; def foo: Inner = {println("ok"); null};      }
abstract class Foo3 { type  Inner; def foo: Inner; foo; }
abstract class Foo4 { type  Inner; def foo: Inner = {println("ok"); null.asInstanceOf[Inner]};      }

abstract class Bar1 { type  Inner; def foo: Inner = {println("ok"); null.asInstanceOf[Inner]};      }
abstract class Bar2 { type  Inner; def foo: Inner; foo; }
abstract class Bar3 { class Inner; def foo: Inner = {println("ok"); null};      }
abstract class Bar4 { class Inner; def foo: Inner; foo; }

object Test1 extends Foo1 with Bar1 {def main(args:Array[String]):Unit=();}
object Test2 extends Foo2 with Bar2 {def main(args:Array[String]):Unit=();}
object Test3 extends Foo3 with Bar3 {def main(args:Array[String]):Unit=();}
object Test4 extends Foo4 with Bar4 {def main(args:Array[String]):Unit=();}
object Test5 with    Foo1 with Bar1 {def main(args:Array[String]):Unit=();}
object Test6 with    Foo2 with Bar2 {def main(args:Array[String]):Unit=();}
object Test7 with    Foo3 with Bar3 {def main(args:Array[String]):Unit=();}
object Test8 with    Foo4 with Bar4 {def main(args:Array[String]):Unit=();}

object Test {
  def main(args:Array[String]): Unit = {
    Test1;
    Test2;
    Test3;
    Test4;
    Test5;
    Test6;
    Test7;
    Test8;
    ()
  }
}
