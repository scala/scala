import scala.xml._ ;

object Test with Application {

  val testValue = "This is a test.";

  val testValue2 = 42;

  val page = <testTag value={ testValue } ab="bkla" />;

  val page2 = <testTag value={testValue2.toString()} bla="foo"></testTag>;

  Console.println( page.toString() );

  Console.println( page2.toString() );

}
