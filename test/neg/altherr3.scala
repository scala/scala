// $Id$

module bug {

  //##########################################################################

  trait Foo[a];

  def foo0[a](x: a*): Foo[a] = foo0();

  val foo1: Int = 0;
  def foo1[a](x: a*): Foo[a] = foo1();

  def bar0[a](x: a): Int = 0;
  def bar1[a](x: Foo[a]): Int = 0;
  def bar2[a](x: Foo[Foo[a]]): Int = 0;
  def bar3[a](x: Foo[Foo[Foo[a]]]): Int = 0;

  //##########################################################################

  bar0(foo0());
  bar0(foo0(foo0()));
  bar0(foo0(foo0(foo0())));
  bar0(foo0(foo0(foo0(foo0()))));

  bar0[Int](foo0());                             // error  1
  bar0[Int](foo0(foo0()));                       // error  2
  bar0[Int](foo0(foo0(foo0())));                 // error  3
  bar0[Int](foo0(foo0(foo0(foo0()))));           // error  4

  bar0[Foo[Int]](foo0());
  bar0[Foo[Int]](foo0(foo0()));                  // error  5
  bar0[Foo[Int]](foo0(foo0(foo0())));            // error  6
  bar0[Foo[Int]](foo0(foo0(foo0(foo0()))));      // error  7

  bar0[Foo[Foo[Int]]](foo0());
  bar0[Foo[Foo[Int]]](foo0(foo0()));
  bar0[Foo[Foo[Int]]](foo0(foo0(foo0())));       // error  8
  bar0[Foo[Foo[Int]]](foo0(foo0(foo0(foo0())))); // error  9

  //##########################################################################

  bar1(foo0());
  bar1(foo0(foo0()));
  bar1(foo0(foo0(foo0())));
  bar1(foo0(foo0(foo0(foo0()))));

  bar1[Int](foo0());
  bar1[Int](foo0(foo0()));                       // error 10
  bar1[Int](foo0(foo0(foo0())));                 // error 11
  bar1[Int](foo0(foo0(foo0(foo0()))));           // error 12

  bar1[Foo[Int]](foo0());
  bar1[Foo[Int]](foo0(foo0()));
  bar1[Foo[Int]](foo0(foo0(foo0())));            // error 13
  bar1[Foo[Int]](foo0(foo0(foo0(foo0()))));      // error 14

  bar1[Foo[Foo[Int]]](foo0());
  bar1[Foo[Foo[Int]]](foo0(foo0()));
  bar1[Foo[Foo[Int]]](foo0(foo0(foo0())));
  bar1[Foo[Foo[Int]]](foo0(foo0(foo0(foo0())))); // error 15

  //##########################################################################

  bar2(foo0());
  bar2(foo0(foo0()));
  bar2(foo0(foo0(foo0())));
  bar2(foo0(foo0(foo0(foo0()))));

  bar2[Int](foo0());
  bar2[Int](foo0(foo0()));
  bar2[Int](foo0(foo0(foo0())));                 // error 16
  bar2[Int](foo0(foo0(foo0(foo0()))));           // error 17

  bar2[Foo[Int]](foo0());
  bar2[Foo[Int]](foo0(foo0()));
  bar2[Foo[Int]](foo0(foo0(foo0())));
  bar2[Foo[Int]](foo0(foo0(foo0(foo0()))));      // error 18

  bar2[Foo[Foo[Int]]](foo0());
  bar2[Foo[Foo[Int]]](foo0(foo0()));
  bar2[Foo[Foo[Int]]](foo0(foo0(foo0())));
  bar2[Foo[Foo[Int]]](foo0(foo0(foo0(foo0()))));

  //##########################################################################

  bar3(foo0());
  bar3(foo0(foo0()));                            // error 19 (inference fails)
  bar3(foo0(foo0(foo0())));
  bar3(foo0(foo0(foo0(foo0()))));

  bar3[Int](foo0());
  bar3[Int](foo0(foo0()));
  bar3[Int](foo0(foo0(foo0())));
  bar3[Int](foo0(foo0(foo0(foo0()))));           // error 20

  bar3[Foo[Int]](foo0());
  bar3[Foo[Int]](foo0(foo0()));
  bar3[Foo[Int]](foo0(foo0(foo0())));
  bar3[Foo[Int]](foo0(foo0(foo0(foo0()))));

  bar3[Foo[Foo[Int]]](foo0());
  bar3[Foo[Foo[Int]]](foo0(foo0()));
  bar3[Foo[Foo[Int]]](foo0(foo0(foo0())));
  bar3[Foo[Foo[Int]]](foo0(foo0(foo0(foo0()))));

  //##########################################################################
  //##########################################################################
  //##########################################################################

  bar0(foo1());
  bar0(foo1(foo1()));
  bar0(foo1(foo1(foo1())));
  bar0(foo1(foo1(foo1(foo1()))));

  bar0[Int](foo1());                             // error 21
  bar0[Int](foo1(foo1()));                       // error 22
  bar0[Int](foo1(foo1(foo1())));                 // error 23
  bar0[Int](foo1(foo1(foo1(foo1()))));           // error 24

  bar0[Foo[Int]](foo1());
  bar0[Foo[Int]](foo1(foo1()));                  // error 25
  bar0[Foo[Int]](foo1(foo1(foo1())));            // error 26
  bar0[Foo[Int]](foo0(foo1(foo1(foo1()))));      // error 27

  bar0[Foo[Foo[Int]]](foo1());
  bar0[Foo[Foo[Int]]](foo1(foo1()));
  bar0[Foo[Foo[Int]]](foo1(foo1(foo1())));       // error 28
  bar0[Foo[Foo[Int]]](foo1(foo0(foo1(foo1())))); // error 29

  //##########################################################################

  bar1(foo1());
  bar1(foo1(foo1()));
  bar1(foo1(foo1(foo1())));
  bar1(foo1(foo1(foo1(foo1()))));

  bar1[Int](foo1());
  bar1[Int](foo1(foo1()));                       // error 30
  bar1[Int](foo1(foo1(foo1())));                 // error 31
  bar1[Int](foo1(foo1(foo1(foo1()))));           // error 32

  bar1[Foo[Int]](foo1());
  bar1[Foo[Int]](foo1(foo1()));
  bar1[Foo[Int]](foo1(foo1(foo1())));            // error 33
  bar1[Foo[Int]](foo1(foo1(foo1(foo1()))));      // error 34

  bar1[Foo[Foo[Int]]](foo1());
  bar1[Foo[Foo[Int]]](foo1(foo1()));
  bar1[Foo[Foo[Int]]](foo1(foo1(foo1())));
  bar1[Foo[Foo[Int]]](foo1(foo1(foo1(foo1())))); // error 35

  //##########################################################################
}
