//############################################################################
// Run Time Bugs & Test Cases
//############################################################################
// $Id$

import java.lang.System; // to avoid name clash with .NET's library

//############################################################################
// Test 1 - Runtime Matching creation

object Test1Test {
  import scala.collection.{immutable,Set} ;
  import scala.runtime.matching._ ;
  case class Foo(i:Int,c:Char,d:List[Foo]) ;

  val t2 = TreeNT(2);
  val tr1 = AnyTreeRule( ANYTREE );
  val tr2 = TreeRule( t2, 1, ANYHEDGE );
  val h1 = new HedgeNT(1, true);
  val hr1 = HedgeRule( ANYHEDGE, ANYTREE, ANYHEDGE );
  val treeTrans =
    new immutable.TreeMap[Int,Set[TRule]]()
    .update( 1, immutable.ListSet.Empty[TRule] + tr1 )
    .update( 2, immutable.ListSet.Empty[TRule] + tr2 );
  val hedgeTrans = new immutable.TreeMap[Int,Set[HRule]]()
    .update( 1, immutable.ListSet.Empty[HRule] + hr1 );

  val gram = new Grammar( treeTrans, hedgeTrans, null ) { val
    treeInitials = immutable.ListSet.Empty[TreeNT] + t2; val
    hedgeInitials = immutable.ListSet.Empty[HedgeNT]; def test( i:Int,
    inp:Any ) = { if( i==1 ) inp.isInstanceOf[List[Int]] else false; } }

  def main(args:Array[String]): Unit =
    Console.println( new Matcher(gram).matches( List(1,2,3) ));
}

//############################################################################
// Main

object Test  {
  var errors: Int = 0;
  def test(bug: String, def test: Unit): Unit = {
    System.out.println("<<< bug " + bug);
    try {
      test;
    } catch {
      case exception => {
        val name: String = Thread.currentThread().getName();
        System.out.print("Exception in thread \"" + name + "\" ");
        exception.printStackTrace();
        System.out.println();
        errors = errors + 1;
      }
    }
    System.out.println(">>> bug " + bug);
    System.out.println();
  }

  def main(args: Array[String]): Unit = {

    test("Test1"  , Test1Test.main(args));

    if (errors > 0) {
      System.out.println();
      System.out.println(errors + " error" + (if (errors > 1) "s" else ""));
    }

  }
}

//############################################################################
