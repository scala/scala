package scala ;

object UnitTest {

            def test[b,a]( doit:b => a,
                              input: b,
                              expectedResult:a ):Unit = {

                if( doit(input) == expectedResult )
                        {
                         System.out.println("passed ok")
                        }
                else
                        {
                         System.out.print("failed! we got");
                         System.out.print( "\""+doit(input).toString()+"\"" );
                         System.out.println(" but expected "+expectedResult)
                        }

            } // testAlg

            def test2[c,b,a]( doit:(c,b) => a,
                              in1: c,
                              in2: b,
                              expectedResult:a ):Unit = {

                if( doit(in1,in2) == expectedResult )
                        {
                         System.out.println("passed ok")
                        }
                else
                        {
                         System.out.print("failed! we got");
                         System.out.print( "\""+doit(in1,in2).toString()+"\"" );
                         System.out.println(" but expected "+expectedResult)
                        }

            } // testAlg

} // unitTest


