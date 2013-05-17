/**
 * Demonstration of issue with Extractors. If lines 15/16 are not present, get at runtime:
 *
 * Exception in thread "main" java.lang.VerifyError: (class: ExtractorIssue$$, method: convert signature: (LTopProperty;)LMyProp;) Accessing value from uninitialized register 5
 *	at ExtractorIssue.main(ExtractorIssue.scala)
 *	at com.intellij.rt.execution.application.AppMain.main(AppMain.java:115)]
 *
 * If lines 15/16 are present, the compiler crashes:
 *
 * fatal error (server aborted): not enough arguments for method body%3: (val p: MyProp[java.lang.String])MyProp[_33].
 * Unspecified value parameter p.
 */
object Test {

  def main(args: Array[String]) {
    convert(new SubclassProperty)
  }

  def convert(prop: TopProperty): MyProp[_] = {
    prop match {

        ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
        //case SubclassSecondMatch(p) => p // if these lines are present, the compiler crashes. If commented, unsafe byte
        //case SecondMatch(p) => p         // byte code is generated, which causes a java.lang.VerifyError at runtime
        ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

        case SubclassMatch(p) => p
        case StandardMatch(p) => p
    }
  }
}

class TopProperty

class StandardProperty extends TopProperty
class SubclassProperty extends StandardProperty

class SecondProperty extends TopProperty
class SubclassSecondProperty extends StandardProperty

trait MyProp[T]
case class MyPropImpl[T]() extends MyProp[T]

object SubclassMatch {

  def unapply(prop: SubclassProperty) : Option[MyProp[String]] = {
    Some(new MyPropImpl)
  }

  def apply(prop: MyProp[String]) : SubclassProperty = {
    new SubclassProperty()
  }
}

object StandardMatch {

  def unapply(prop: StandardProperty) : Option[MyProp[String]] = {
    Some(new MyPropImpl)
  }

  def apply(prop: MyProp[String]) : StandardProperty = {
    new StandardProperty()
  }
}

object SubclassSecondMatch {

  def unapply(prop: SubclassSecondProperty) : Option[MyProp[BigInt]] = {
    Some(new MyPropImpl)
  }

  def apply(prop: MyProp[String]) : SubclassSecondProperty = {
    new SubclassSecondProperty()
  }
}

object SecondMatch {

  def unapply(prop: SecondProperty) : Option[MyProp[BigInt]] = {
    Some(new MyPropImpl)
  }

  def apply(prop: MyProp[String]) : SecondProperty = {
    new SecondProperty()
  }
}