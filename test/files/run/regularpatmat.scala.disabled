// Burak's test suite for regular pattern matching

//import java.lang.System; // to avoid name clash with .NET's library

object Test {
  def main(args: Array[String]): Unit = {
    Console.println("pretest");
    val L = List(1,2,3);
    scala.testing.UnitTest.assertEquals( L, L match { case List(xs@_*) => xs; } ) ;

    testWR.main(args);
    testWS.main(args);
    testWT.main(args);
    testWV.main(args);
    //testWW.main(args);
    testBK.main(args);
    testBL.main(args);
    testBM.main(args);
    testBN.main(args);
    testBO.main(args);
    testMZ.main;
    //testNN.main;
    //testBugSequenceApply.main;
  }
}

// contains 17 visitors plus X

// analyzer related (no execution)
object bug179 {
  case class One();
  object Foo {
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

object values { // test values reused in nearly all test cases

    val s0: List[Char] = Nil ;
    val s1: List[Char] = 'a'::'b'::'c'::Nil ;
    val s2: List[Char] = 'a'::'b'::'c'::s1 ;
    val s3: List[Char] = 'a'::'a'::'a'::Nil ;
    val s4: List[Char] = 'b'::'c'::Nil ;
    val s5: List[Char] = 'b'::Nil ;
    val s6: List[Char] = 'b'::'a'::'a'::'a'::Nil ;

    val s7: List[Int]  = 1::2::7::Nil ;
    val s8: List[Int]  = 1::0::1::0::Nil;
    val s9: List[Int]  = Nil ;

    val s10: List[Char] = '7'::'0'::'1'::'.'::'2'::'4'::Nil ;

}

// matching without binding

// 2do            case [ 'a'; x; y ]   => 100
//                case [ z @ ('a'; x; y) ]   => 100
// 2do             case [ 'a'*; x @ ( _;('a'; 'b')* ); y @ 'b'* ]   => 100
//                case _               => 321 // 20022 // this never happens

object testBK {

  import values._ ;
  import scala.testing.UnitTest._ ;

  def doit1(e: List[Char]): Int = e match {
    case List( 'a'*, x @ ( 'a',('a', 'b')* ), y @ ('b'*) ) => 100
    case List( _ * )         => 321
  }

  def test1: Unit = {
    Console.println("testBK");
    //test[List[Char],Int]( doit1, s0, 321);
    assertEquals( doit1( s0 ), 321);
    assertEquals( doit1( s1 ),321);
    assertEquals( doit1( s2 ),321);
    assertEquals( doit1( s3 ),100);
    assertEquals( doit1( s4 ),321);
    assertEquals( doit1( s5 ),321);
    assertEquals( doit1( s6 ),321)
  }

  def main(args: Array[String]): Unit = {
    test1;
  }

}

// tests with binding

object testBL {

  import scala.testing.UnitTest._ ;

  def preTest(a:String,b:String):boolean = (a==b);
  
  def doit(x: List[String]): String = x match {
    case List( z @ "John" ) => z
  }
  
  // BEWARE: main type should be specified... 
  // often, last thing is not () then you have a problem

  def main(args: Array[String]): Unit = {
    val a = "John"; 
    val b = "John";
    
    assertEquals(a == b, true);
    assertEquals(doit(List(b)), "John")
  }
}

object testBM {

  import scala.testing.UnitTest._ ;
  import values._ ;

  def doit1(e: List[Char]): List[Char] = e match {
    case List( 'a'*, x @ ( 'a',('a', 'b')* ), y @ ('b'*) )
      => { x.toList }

    case List( 'a'*, x @ (('a', 'b')*) , y @ (('a','b','c') *) )
      => { y.toList }

    case List( _ * )
      => Nil
  }

  def test1: Unit = {
    Console.println("testBM");
    assertEquals( doit1( s0 ), Nil);
    assertEquals( doit1( s1 ), s1);
    assertEquals( doit1( s2 ), s2); 

    assertEquals( doit1( s3 ), List('a'));
    assertEquals( doit1( s4 ), Nil);
    assertEquals( doit1( s5 ), Nil);
    assertEquals( doit1( s6 ), Nil);

    val t7:List[Char] = 'a'::'a'::'a'::'b'::'b'::'b'::Nil;
    //val t7ex:List[Char] = 'a'::'a'::'b'::Nil; // with longest match policy

    assertEquals( doit1( t7 ), List('a') );
  }

  def main(args: Array[String]) = {
    test1
  }

}

object testBN {

  import scala.testing.UnitTest._ ;
  import values._ ;
        
  class testClass;

  case class testA(arg: List[Char]) extends testClass;

  def doit1(e: testClass): List[Char] = e match {
    case testA(List( 'a', x, y )) => x::y::Nil
    case _                        => Nil
  }

  def test1:Unit = {
    Console.print("BN preTest: ");
    Console.println(Nil == Nil);
    Console.println("testBN");

    assertEquals(doit1(testA(s0)), Nil);
    assertEquals(doit1(testA(s1)), 'b'::'c'::Nil);
    assertEquals(doit1(testA(s2)), Nil);
    assertEquals(doit1(testA(s3)), 'a'::'a'::Nil);
    assertEquals(doit1(testA(s4)), Nil);
    assertEquals(doit1(testA(s5)), Nil);
    assertEquals(doit1(testA(s6)), Nil);
  }

  def main(args: Array[String]) = {
    test1
  }

}

object testBO  {

  // example suggested by Matthias
  import scala.testing.UnitTest._ ;

  case class Person(firstname: String, lastname: String);

  def onlyJohn(db: List[Person]): List[String] = db match {  
    case List(Person("John", lastname)) => lastname::Nil
    case _ => Nil
  }

  /** first longest match policy -> the star is greedy/hungry/...
   */
  def searchFirstJohn(db: List[Person]): String = db match {
    case List( _ *, Person("John", lastname), _ * ) => lastname
    case _ => "<not found>"
  }

  /** first longest match policy -> star is a greedy/hungry
   */
  def searchJohns(db: List[Person]): List[String] = db match {
    case List( _ *, Person("John", lastname), rest@(_ *)) => {
      //Console.print("before is : " + before);
      lastname::searchJohns(rest.toList)
    }
    case _ => Nil
  }

  def main(args: Array[String]): Unit = {
    val p1 = Person("Albert",  "Camus");
    val p2 = Person("Henry",   "Miller");
    val p3 = Person("John",    "Le Carre");
    val p4 = Person("Herbert", "Franke");
    val p5 = Person("John",    "Smith");
    val p6 = Person("William", "Gibson");

    val db: List[Person] = p1::p2::p3::p4::p5::p6::Nil;

    val db2: List[Person] = p3::Nil;

    Console.println("testBO");

    assertEquals(onlyJohn(db),         Nil);
    assertEquals(onlyJohn(db2),        "Le Carre"::Nil);
    assertEquals(searchFirstJohn(db),  "Le Carre");
    assertEquals(searchFirstJohn(db2), "Le Carre");
    assertEquals(searchJohns(db),      "Le Carre"::"Smith"::Nil);
    assertEquals(searchJohns(db2),     "Le Carre"::Nil);
  }

}

object testWR  {

  import values._ ;
  import scala.testing.UnitTest._ ;

  def doit1(e: List[Char]): Int = e match {
    case List( 'a', 'b', 'c' ) => 100
    case _                     => 321
  }

  def test1: Unit = {
    Console.println("testWR_1");
    assertEquals( doit1( s0 ), 321);
    assertEquals( doit1( s1 ), 100);
    assertEquals( doit1( s2 ), 321);
    assertEquals( doit1( s3 ), 321);
    assertEquals( doit1( s4 ), 321);
    assertEquals( doit1( s5 ), 321);
    assertEquals( doit1( s6 ), 321)
  }

  def doit2(e: List[Char]):Int = e match {
    case List( ('a', 'b','c')? ) => 1000
    case _                       => 321
  }

  def test2: Unit = {
    Console.println("testWR_2");
    assertEquals( doit2( s0 ), 1000);
    assertEquals( doit2( s1 ), 1000);
    assertEquals( doit2( s2 ),  321);
    assertEquals( doit2( s3 ),  321);
    assertEquals( doit2( s4 ),  321);
    assertEquals( doit2( s5 ),  321);
    assertEquals( doit2( s6 ),  321);
  }

  def doit3(e: List[Char]): String = e match {
    case List( ('a', 'a','a')? ) => "ok"
    case _                   => "fail"
  }

  def test3: Unit = {
    Console.println("testWR_3");
    assertEquals(doit3(s0), "ok");
    assertEquals(doit3(s1), "fail");
    assertEquals(doit3(s2), "fail");
    assertEquals(doit3(s3), "ok");  
    assertEquals(doit3(s4), "fail");
    assertEquals(doit3(s5), "fail");
    assertEquals(doit3(s6), "fail");
  }

  def doit4(e: List[Char]): String = e match {
    case List( ('a'|'b')*,('a'|'b'|'c')+ ) => "ga!!!!"
    case _                                 => "gu"
  }

  def test4: Unit = {
    Console.println("testWR_4");
    assertEquals( doit4( s0 ), "gu");
    assertEquals( doit4( s1 ), "ga!!!!");
    assertEquals( doit4( s2 ), "ga!!!!");
    assertEquals( doit4( s3 ), "ga!!!!");
    assertEquals( doit4( s4 ), "ga!!!!");
    assertEquals( doit4( s5 ), "ga!!!!");
    assertEquals( doit4( s6 ), "ga!!!!");
  }

  def doit5(e: List[Int]): String = e match {
    case List( (0|1)+ ) => "binary"
    case _              => "not binary"
  }

  def test5: Unit = {
    Console.println("testWR_5");
    assertEquals( doit5( s7 ), "not binary");
    assertEquals( doit5( s8 ), "binary");
    assertEquals( doit5( s9 ), "not binary");
  }               

  //  { ('0'..'9')*;'.';('0'..'9');('0'..'9')* ]
  def doit6(e: List[Char]): String = e match {
    case List( ('0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')*,
                '.',
               ('0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'),
               ('0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9')* )

                                              => "decimal number"
                case _                        => "not decimal"
  }

  def test6: Unit = {
    Console.println("testWR_6");
    assertEquals(doit6(s3),  "not decimal");
    assertEquals(doit6(s10), "decimal number");
  }               

  def test8: Unit = {
    Console.println("testWR_8");
      
    assertTrue( List('d','c') match {
      case List('a'*, 'd'|'e', 'c'*) => true
      case _                         => false
    });
  }

  def test7:Unit = {
    Console.println("testWR_7");
    assertEquals( List(1,2) match {
      case List(1,(3*,2))=> true; // test normalization (pattern normalizer)
      case _ => false;
    }, true);
  }

  def main(args: Array[String]): Unit = {
    test1;
    test2;
    test3;
    test4;
    test5;
    test6;
    test7;
    test8;
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

    def doit1(e: List[Char]):Int = e match {
                case List( 'a', 'b', 'c' )        => 100
                case List( ('a', 'b','c')? )      => 1004
                case List( ('a', 'a','a')? )      => 50
                case List( ('a'|'b')*,('a'|'b') ) => 700
                case _                        => 321
        };      

  def test1: Unit = {
    Console.println("testWS");
    assertEquals( doit1( s0 ), 1004);
    assertEquals( doit1( s1 ),  100);
    assertEquals( doit1( s2 ),  321);
    assertEquals( doit1( s3 ),  50);
    assertEquals( doit1( s4 ), 321);
    assertEquals( doit1( s5 ), 700);
    assertEquals( doit1( s6 ), 700);
  }

  def main(args: Array[String]): Unit = {
    test1;
  }

}

object testWT  {

  import values._ ;
  import scala.testing.UnitTest._ ;

  def doit1(e: List[Char]): Int = e match {
    case List('a', _, _) => 100
    case List(_ *)       => 321
    case _               => 20022 // this never happens
  }

  def test1: Unit = {
    Console.println("testWT");
    assertEquals( doit1( s0 ),321);
    assertEquals( doit1( s1 ),100);
    assertEquals( doit1( s2 ),321);
    assertEquals( doit1( s3 ),100);
    assertEquals( doit1( s4 ),321);
    assertEquals( doit1( s5 ),321);
    assertEquals( doit1( s6 ),321)
  }

  def main(args: Array[String]): Unit = {
    test1;
  }

}
object testWV {

  import values._ ;
  import scala.testing.UnitTest._ ;
        
  class testClass;

  case class testA( arg:List[Char] ) extends testClass;

  def doit1(e: testClass):Int = e match {
    case testA(List( 'a', 'b', 'c' ))        => 100
    case testA(List( ('a', 'b','c')? ))      => 1004
    case testA(List( ('a', 'a','a')? ))      => 50
    case testA(List( ('a'|'b')*,('a'|'b') )) => 700
    case testA( _ )                          => 321
  }

  def test1: Unit = {
    Console.println("testWV");
    assertEquals(doit1(testA(s0)), 1004);
    assertEquals(doit1(testA(s1)),  100);
    assertEquals(doit1(testA(s2)),  321);
    assertEquals(doit1(testA(s3)),   50);
    assertEquals(doit1(testA(s4)),  321);
    assertEquals(doit1(testA(s5)),  700);
    assertEquals(doit1(testA(s6)),  700);
  }

  def main(args: Array[String]) = {
    test1
  }

}
/*
object testWW {

        import values._ ;

        import scala.testing.UnitTest._ ;
        
        class testClass;
        case class testA( arg:List[Char] ) extends testClass;

        def doit1(e: List[testClass]):Int = e match {
        
                case List( testA(List()), testA( List( 'a', 'b' )) )        => 100
                case _                                                      => 321

        };      

        def test1:Unit = {
                val x1 = List( testA(s0) );

                Console.println("testWW");

                assertEquals( doit1( x1 ), 321 );

                val x2 = List( testA(Nil), testA('a'::'b'::Nil) );
                                                  
                assertEquals( doit1( x2 ), 100 );

        }

        def main( args:Array[String] ) = {
                test1;
        }

}
*/
object testMZ {
        import scala.testing.UnitTest.assertEquals ;
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
  def testBar(xs: List[Any]) = xs match { // bug#180
    case List(((On(), Tw())* | (On(), On())), On()) => "caseBar"
    case _ => "default";
  }


  def mat195(x:Expr) = x match { // bug#195	
    case One(x@List(_*)) =>
    	"x = " + x;

	case _ =>"default";

  }

  def mat196(xs: List[Any]) = xs match { // bug#196	
    case List(b@(()|())) =>
    	"case, b = " + b;

	case _ =>"default";

  }

  def mat398(xs:List[Any]) = xs match { // bug#398
    case List(1) => "one"
    case x::xs   => "two"
  }

  def mat406() = {
    class Type;
    case class A() extends Type;
    case class B() extends Type;
    case class C() extends Type;
    
    def foo(x: Type, y: Type): String = Pair(x, y) match {
      case Pair(A(), A()) 
      | Pair(A(), B()) 
      | Pair(B(), A()) 
      | Pair(B(), B()) => "3"
      case Pair(C(), C()) => "4"
      case Pair(A(), _)
      | Pair(B(), _) => "7"
      case _ => "8"
    }
    
    foo(A(), C())
  }

  def mat441() = {
    val tata = 1;
    val titi = 0.8 + Math.random();
    try {
      tata match {
        case 1 if (titi < 0.5) => "a"
        case 0 | 1             => "b"
      }
    } catch {
      case _ => "c"
    }
  }

  /* this will crash
  def matSymbolCloning = {
    2 match {
      case 3 | 4 =>
        class Foo extends scala.xml.Atom[Int](3) {
          def bar = 7;
        }
        null
    }
  }
  */

  def main:Unit = {  
                Console.println("testMZ - bugs #132 #133b #180 #195 #196 #398 #406 #441");
    assertEquals(testFoo( List(Two(),Two(),Two(),Two()) ),"b = Two");
    assertEquals(testFoo( List(Two(),Two(),Two()) ),"a = Two");
    assertEquals(testFoo( List(Two(),Two()) ),"a = Two");
    assertEquals(testFoo( List(Two()) ),"a = Two");
    assertEquals(testFoo( List() ),"no match");
    assertEquals(bind( List(OneN(),OneN()) ),"case");
    assertEquals(testBar( List() ),"default");
    assertEquals(testBar( List(On()) ),"caseBar");
    assertEquals(testBar( List(On(), On())), "default");
    assertEquals(testBar( List(On(), On(), On()) ),"caseBar");
    assertEquals(testBar( List(On(), On(), On(), On()) ),"default");
    assertEquals(testBar( List(On(), On(), On(), On(), On()) ),"default");
    assertEquals(mat195( One(List(Two(),Two())) ),"x = List(Two,Two)");
    assertEquals(mat195( One(List()) ),"x = List()");
    assertEquals(mat195( Two() ),"default");
    assertEquals(mat196( List(1) ),"default");
    assertEquals(mat196( List() ),"case, b = List()");
    assertEquals(mat398( List(2) ),"two");
    assertEquals(mat398( List(2) ),"two");
    assertEquals(mat406(), "7");
    assertEquals(mat441(), "b");
    ()
  }
  
}
/*
object testNN {
 import scala.testing.UnitTest._ ;
  abstract class K;
  case class F(x:K*) extends K;
  case class G() extends K;

  def mtch(k:K):boolean = k match {
      case F(F(G()*),G(),F(G()*)) => true;
      case _ => false;
  }

  def main:Unit = {
    Console.println("testNN");
    assertEquals(mtch( F(F(G()),G(),F(G()))), true);
    assertEquals(mtch( F(F(),G(),F(G(),G(),G(),G())) ), true);    
    assertEquals(mtch( G() ), false);    
    assertEquals(mtch( F(G()) ), false);    
  }
}
*/
object testNO {   // this does not need to be run, only compiled

  trait Operator;
  case class Increment() extends Operator;
  case class Decrement() extends Operator;
  
  trait Expression {
    def eval = this match {
      case Operation (v: Value, o: Increment) => v
      case Operation (v: Value, d: Decrement) => v
    }
  }
  
  case class Value() extends Expression;
  case class Operation (e: Expression, o: Operator) extends Expression;


}

/** see comments in scala.tools.scalac.transformer.matching.PatternMatcher::isSeqApply 2005-02-17
 */

/*
object testBugSequenceApply {

  val x = List(1,2,3);

  case class ThreeBars extends Seq[Int] {
    override def length = 3;
    def elements = x.elements;
    def apply(i:Int) = x.apply(i);
  }

  // this works
  def main:Unit = {  
    Console.print("testBugSequenceApply ");
    val z: Seq[Int] = new ThreeBars();
    Console.print(z match {
      case Seq(1,2,3) => "hello" // but ThreeBars is a case class...
    });
    
    Console.print(ThreeBars() match {
      case Seq(1,2,3) => " hello" // but ThreeBars is a case class...
    });
  }
}
*/
