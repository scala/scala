// Burak's test suite for regular pattern matching

// contains 17 visitors plus X

// analyzer related (no execution)
object bug179 {
  case class One();
  object Foo with Executable {
    def test(xs: List[Any]) = xs match {
      case List(((((One(), One())*) | (One(), One())), One())) =>
	Console.println("case")
      case _ =>
	Console.println("default");
    }
  }
}
// testW? are for recognition only ( no variables )
// testB? are for variables binding

object values  { // test values reused in nearly all test cases

    val s0:List[Char] = Nil ;
    val s1:List[Char] = 'a'::'b'::'c'::Nil ;
    val s2:List[Char] = 'a'::'b'::'c'::s1 ;
    val s3:List[Char] = 'a'::'a'::'a'::Nil ;
    val s4:List[Char] = 'b'::'c'::Nil ;
    val s5:List[Char] = 'b'::Nil ;
    val s6:List[Char] = 'b'::'a'::'a'::'a'::Nil ;

    val s7:List[Int]  = 1::2::7::Nil ;
    val s8:List[Int]  = 1::0::1::0::Nil;
    val s9:List[Int]  = Nil ;

    val s10:List[Char] = '7'::'0'::'1'::'.'::'2'::'4'::Nil ;


}

// matching without binding

// 2do            case [ 'a'; x; y ]   => 100
//                case [ z @ ('a'; x; y) ]   => 100
// 2do             case [ 'a'*; x @ ( _;('a'; 'b')* ); y @ 'b'* ]   => 100
//                case _               => 321 // 20022 // this never happens

object testBK  {

        import values._ ;

        import scala.testing.UnitTest._ ;

    def doit1(e: List[Char]):Int = e.match {

                case List( 'a'*, x @ ( 'a',('a', 'b')* ), y @ ('b'*) )   => { 100 }

                case List( _ * )         => 321
        };

    def test1:Unit = {
                System.out.println("testBK");
                test[List[Char],Int]( doit1, s0, 321);
                test[List[Char],Int]( doit1, s1, 321);
                test[List[Char],Int]( doit1, s2, 321);
                test[List[Char],Int]( doit1, s3, 100);
                test[List[Char],Int]( doit1, s4, 321);
                test[List[Char],Int]( doit1, s5, 321);
                test[List[Char],Int]( doit1, s6, 321)
    };

    def main( args:Array[ String ] ) = {
         test1;
    }

}

// tests with binding

object testBL {

  import scala.testing.UnitTest._ ;

  def preTest(a:String,b:String):boolean = (a==b);

  def doit( x:List[String] ):String = x.match {

    case List( z @ "John" ) => z

  }

        // BEWARE: main type should be specified...
        // often, last thing is not () then you have a problem

  def main(args:Array[String]):Unit = {

       val a = "John";
       val b = "John";

        test2[String,String,boolean](preTest,a,b,true);
        test[List[String],String]( doit, List( b ), "John" )


  }
}
object testBM  {

    import scala.testing.UnitTest._ ;
    import values._ ;

    def doit1(e: List[Char]):List[Char] = e.match {

                case List( 'a'*, x @ ( 'a',('a', 'b')* ), y @ ('b'*) )

                        => { x }

                case List( 'a'*, x @ (('a', 'b')*) , y @ (('a','b','c') *) )

                        => { y }

                case List( _ * )

                        => Nil
        };

    def test1:Unit = {
                System.out.println("testBM");
                test[List[Char],List[Char]]( doit1, s0, Nil);
                test[List[Char],List[Char]]( doit1, s1, s1);
                test[List[Char],List[Char]]( doit1, s2, s2);

                test[List[Char],List[Char]]( doit1, s3, List('a'));
                test[List[Char],List[Char]]( doit1, s4, Nil);
                test[List[Char],List[Char]]( doit1, s5, Nil);
                test[List[Char],List[Char]]( doit1, s6, Nil);

                val t7:List[Char] = 'a'::'a'::'a'::'b'::'b'::'b'::Nil;
                //val t7ex:List[Char] = 'a'::'a'::'b'::Nil; // with longest match policy

                test[List[Char],List[Char]]( doit1, t7, List('a') );
                ()
    };

    def main( args:Array[ String ] ) = {
         test1;
    }

}
object testBN {

    import scala.testing.UnitTest._ ;
    import values._ ;

        class testClass;

        case class testA( arg:List[Char] ) extends testClass;

    def doit1(e: testClass):List[Char] = e.match {
                case testA(List( 'a', x, y ))   => x::y::Nil
                case _                      => Nil
        };

    def test1:Unit = {
                System.out.print("BN preTest: ");
                System.out.println( Nil == Nil );
                System.out.println("testBN");

                test[testClass,List[Char]]
                        ( doit1, testA(s0), Nil);

                test[testClass,List[Char]]
                        ( doit1, testA(s1), 'b'::'c'::Nil);

                test[testClass,List[Char]]
                        ( doit1, testA(s2), Nil);

                test[testClass,List[Char]]
                        ( doit1, testA(s3), 'a'::'a'::Nil);

                test[testClass,List[Char]]
                        ( doit1, testA(s4), Nil);

                test[testClass,List[Char]]
                        ( doit1, testA(s5), Nil);

                test[testClass,List[Char]]
                        ( doit1, testA(s6), Nil);

    };


    def main( args:Array[String] ) = {

                test1

    }

}

object testBO  {

        // example suggested by Matthias
        import scala.testing.UnitTest._ ;


        case class Person( firstname:String, lastname:String );

        def onlyJohn( db:List[ Person ] ):List[ String ] = {

           db.match {

                case List( Person( "John", lastname  ) )

                        => lastname::Nil

                case _
                        => Nil

           }

        }

        /** first longest match policy -> the star is greedy/hungry/...
         */

        def searchFirstJohn( db:List[ Person ] ):String = {

           db.match {

                case List( _ *, Person( "John", lastname  ), _ * )
                        => lastname

                case _
                        => "<not found>"

           }

        }

        /** first longest match policy -> star is a greedy/hungry
         */

        def searchJohns( db:List[Person]):List[String] = {

           db.match {

                case List( _ *, Person( "John", lastname  ), rest@(_ *) )
                        => { //System.out.print("before is : "+before );
                             lastname::searchJohns( rest )
                                }

                case _
                        => Nil

           }

        }

        def main( args:Array[String] ) = {

                val p1 = Person("Albert",  "Camus");
                val p2 = Person("Henry",   "Miller");
                val p3 = Person("John",    "Le Carre");
                val p4 = Person("Herbert", "Franke");
                val p5 = Person("John",    "Smith");
                val p6 = Person("William", "Gibson");

                val db:List[Person] = p1::p2::p3::p4::p5::p6::Nil;

                val db2:List[Person] = p3::Nil;

                System.out.println("testBO");

                test[ List[Person], List[ String ]]
                        ( onlyJohn, db, Nil );

                test[ List[Person], List[ String ]]
                        ( onlyJohn, db2, "Le Carre"::Nil );

                test[ List[Person], String ]
                        ( searchFirstJohn, db, "Le Carre" );

                test[ List[Person], String ]
                        ( searchFirstJohn, db2, "Le Carre" );

                test[ List[Person], List[ String ]]
                        ( searchJohns, db, "Le Carre"::"Smith"::Nil );

                test[ List[Person], List[ String ]]
                        ( searchJohns, db2, "Le Carre"::Nil );

        }

}
object testWR  {

        import values._ ;

        import scala.testing.UnitTest._ ;

    def doit1(e: List[Char]):Int = e.match {
                case List( 'a', 'b', 'c' ) => 100
                case _                     => 321
        };

    def test1:Unit = {
                System.out.println("testWR_1");
                test[List[Char],Int]( doit1, s0, 321);
                test[List[Char],Int]( doit1, s1, 100);
                test[List[Char],Int]( doit1, s2, 321);
                test[List[Char],Int]( doit1, s3, 321);
                test[List[Char],Int]( doit1, s4, 321);
                test[List[Char],Int]( doit1, s5, 321);
                test[List[Char],Int]( doit1, s6, 321)
    };

    def doit2(e: List[Char]):Int = e.match {
                case List( ('a', 'b','c')? ) => 1000
                case _                       => 321
	}

    def test2:Unit = {
                System.out.println("testWR_2");
                test[List[Char],Int]( doit2, s0, 1000);
                test[List[Char],Int]( doit2, s1, 1000);
                test[List[Char],Int]( doit2, s2, 321);
                test[List[Char],Int]( doit2, s3, 321);
                test[List[Char],Int]( doit2, s4, 321);
                test[List[Char],Int]( doit2, s5, 321);
                test[List[Char],Int]( doit2, s6, 321);
    }


    def doit3(e: List[Char]):String = e.match {
                case List( ('a', 'a','a')? ) => "ok"
                case _                   => "fail"
	}

    def test3:Unit = {
                System.out.println("testWR_3");
                test[List[Char],String]( doit3, s0, "ok");
                test[List[Char],String]( doit3, s1, "fail");
                test[List[Char],String]( doit3, s2, "fail");
                test[List[Char],String]( doit3, s3, "ok");
                test[List[Char],String]( doit3, s4, "fail");
                test[List[Char],String]( doit3, s5, "fail");
                test[List[Char],String]( doit3, s6, "fail");
    }

    def doit4(e: List[Char]):String = e.match {
                case List( ('a'|'b')*,('a'|'b'|'c')+ ) => "ga!!!!"
                case _                             => "gu"
	}

    def test4:Unit = {
                System.out.println("testWR_4");
                test[List[Char],String]( doit4, s0, "gu");
                test[List[Char],String]( doit4, s1, "ga!!!!");
                test[List[Char],String]( doit4, s2, "ga!!!!");
                test[List[Char],String]( doit4, s3, "ga!!!!");
                test[List[Char],String]( doit4, s4, "ga!!!!");
                test[List[Char],String]( doit4, s5, "ga!!!!");
                test[List[Char],String]( doit4, s6, "ga!!!!");
    }

    def doit5(e: List[Int]):String = e.match {
                case List( (0|1)+ )               => "binary"
                case _                        => "not binary"
	}

    def test5:Unit = {
                System.out.println("testWR_5");
                test[List[Int],String]( doit5, s7, "not binary");
                test[List[Int],String]( doit5, s8, "binary");
                test[List[Int],String]( doit5, s9, "not binary");
    }

    //  { ('0'..'9')*;'.';('0'..'9');('0'..'9')* ]
    def doit6(e: List[Char]):String = e.match {
                case List( ('0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')*,
                       '.',
                       ('0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'),
                       ('0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')* )

                                              => "decimal number"
                case _                        => "not decimal"
	}

    def test6:Unit = {
                System.out.println("testWR_6");
                test[List[Char],String]( doit6, s3, "not decimal");
                test[List[Char],String]( doit6, s10, "decimal number");
    }

    def main( args:Array[String] ) = {
        test1;
        test2;
        test3;
        test4;
        test5;
        test6
    }



}
object testWS {

        import values._ ;

        import scala.testing.UnitTest._ ;

        /* strings:

            "blabla" == [ "bla";"bla" ] == [ 'b';'l';'a';'b';'l';'a' ]

            [ "blabla";'x';'y'? ] == [ ('b';'l';'a';'b';'l';'a'); 'x'; 'y'? ]

        */

        /*
            def isIdentifierStart(c:char) ;

            case [ ... ; _isIdentifierStart_ ; ... ]

            calls method is..., needs to have type (elementType)Boolean

            translated to pattern

                 [ ... ; Apply(is..., Tree.Empty) ; ... ]

         */

         /* for tree automata:

        [ t0; t1; ...; tn ]  with ti = labeli ( argsi )

        gets translated to

        [ _isTree$0_ ; _isTree$1_ ; ... ; _isTree$n_ ]

        where isTree$i( t ) = t.is[ labeli ] ... (algebraic matcher)

        special case: sequences

        [ ...; seq ; ... ] where seq = [ ... ]

        gets translated to

        [ ...; _seq$0_ ; ...] with seq$0( s ) = t.is[ Sequence ] and
                                                seq$0match( s.newIterator )

        subroutines return
          1) d'abord true or false,
          2) later ( true|false, environment )
                       assume order on variables, enviroment is a tuple/sequence
        */

    def doit1(e: List[Char]):Int = e.match {
                case List( 'a', 'b', 'c' )        => 100
                case List( ('a', 'b','c')? )      => 1004
                case List( ('a', 'a','a')? )      => 50
                case List( ('a'|'b')*,('a'|'b') ) => 700
                case _                        => 321
        };

    def test1:Unit = {
                System.out.println("testWS");
                test[List[Char],Int]( doit1, s0, 1004);
                test[List[Char],Int]( doit1, s1, 100);
                test[List[Char],Int]( doit1, s2, 321);
                test[List[Char],Int]( doit1, s3, 50);
                test[List[Char],Int]( doit1, s4, 321);
                test[List[Char],Int]( doit1, s5, 700);
                test[List[Char],Int]( doit1, s6, 700);
    };

    def main( args:Array[String] ) = {
        test1;
    }



}
object testWT  {

        import values._ ;

        import scala.testing.UnitTest._ ;

    def doit1(e: List[Char]):Int = e.match {
                case List( 'a', _, _ )   => 100
                case List( _ * )         => 321
                case _               => 20022 // this never happens
        };

    def test1:Unit = {
                System.out.println("testWT");
                test[List[Char],Int]( doit1, s0, 321);
                test[List[Char],Int]( doit1, s1, 100);
                test[List[Char],Int]( doit1, s2, 321);
                test[List[Char],Int]( doit1, s3, 100);
                test[List[Char],Int]( doit1, s4, 321);
                test[List[Char],Int]( doit1, s5, 321);
                test[List[Char],Int]( doit1, s6, 321)
    };

    def main( args:Array[ String ] ) = {
         test1;
    }

}
object testWV {

        import values._ ;

        import scala.testing.UnitTest._ ;

        class testClass;

        case class testA( arg:List[Char] ) extends testClass;

    def doit1( e: testClass ):Int = e.match {

                case testA( List( 'a', 'b', 'c' ))        => 100
                case testA( List( ('a', 'b','c')? ))      => 1004
                case testA( List( ('a', 'a','a')? ))      => 50
                case testA( List( ('a'|'b')*,('a'|'b') )) => 700
                case testA( _ )                        => 321

        };

    def test1:Unit = {
                System.out.println("testWV");
                test[testClass,Int]( doit1, testA(s0), 1004);
                test[testClass,Int]( doit1, testA(s1), 100);
                test[testClass,Int]( doit1, testA(s2), 321);
                test[testClass,Int]( doit1, testA(s3), 50);
                test[testClass,Int]( doit1, testA(s4), 321);
                test[testClass,Int]( doit1, testA(s5), 700);
                test[testClass,Int]( doit1, testA(s6), 700);
    };


    def main( args:Array[String] ) = {

                test1

    }

}

object testWW {

        import values._ ;

        import scala.testing.UnitTest._ ;

        class testClass;
        case class testA( arg:List[Char] ) extends testClass;

        def doit1(e: List[testClass]):Int = e.match {

                case List( testA(List()), testA( List( 'a', 'b' )) )        => 100
                case _                                                      => 321

        };

        def test1:Unit = {
                val x1 = List( testA(s0) );

                System.out.println("testWW");

                test[List[testClass],Int]( doit1, x1, 321 );

                val x2 = List( testA(Nil), testA('a'::'b'::Nil) );

                test[List[testClass],Int]( doit1, x2, 100 );

        }

        def main( args:Array[String] ) = {
                test1;
        }

}

object testMZ {
        import scala.testing.UnitTest.test ;
  class Expr;
  case class One(xs: List[Expr]) extends Expr;
  case class Two() extends Expr;
  def testFoo(xs: List[Expr]) = xs match { //bug#132
    case List(Two()?,a,Two()?) => "a = " + a;
    case List(Two()*,b,Two()*) => "b = " + b;
    case List(_*) => "no match";
  }
  case class OneN();
  def bind(xs: List[Any]):String = xs match { // bug#133b
    case List(x@(OneN()*), y@(OneN())) => "case";
    case _ => "default";
  }
  case class On();
  case class Tw();
  def testBar(xs: List[Any]) = xs match {
    case List(((On(), Tw())* | (On(), On())), On()) => "case"
    case _ => "default";
  }

  def main:Unit = {
                System.out.println("testMZ - bugs #132 #133b #180");
    test[List[Expr],String](testFoo, List(Two(),Two(),Two(),Two()), "b = Two");
    test[List[Expr],String](testFoo, List(Two(),Two(),Two()), "a = Two");
    test[List[Expr],String](testFoo, List(Two(),Two()), "a = Two");
    test[List[Expr],String](testFoo, List(Two()), "a = Two");
    test[List[Expr],String](testFoo, List(), "no match");
    test[List[Any],String](bind, List(OneN(),OneN()), "case");
    test[List[Any],String](testBar, List(), "default");
    test[List[Any],String](testBar, List(On()), "case");
    test[List[Any],String](testBar, List(On(), On()), "default");
    test[List[Any],String](testBar, List(On(), On(), On()), "case");
    test[List[Any],String](testBar, List(On(), On(), On(), On()), "default");
    test[List[Any],String](testBar, List(On(), On(), On(), On(), On()), "default");

    ()
  }

}

object testNN {
 import scala.testing.UnitTest._ ;
  abstract class K;
  case class F(x:K*) extends K;
  case class G() extends K;

  def mtch(k:K):boolean = k.match {
      case F(F(G()*),G(),F(G()*)) => true;
      case _ => false;
  }

  def main:Unit = {
                System.out.println("testNN");
    test[K,boolean](mtch, F(F(G()),G(),F(G())), true);
    test[K,boolean](mtch, F(F(),G(),F(G(),G(),G(),G())), true);
    test[K,boolean](mtch, G(), false);
    test[K,boolean](mtch, F(G()), false);
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    testWR.main( args );
    testWS.main( args );
    testWT.main( args );
    testWV.main( args );
    testWW.main( args );
    testBK.main( args );
    testBL.main( args );
    testBM.main( args );
    testBN.main( args );
    testBO.main( args );
    testMZ.main;
    testNN.main;
    ()
  }
}
