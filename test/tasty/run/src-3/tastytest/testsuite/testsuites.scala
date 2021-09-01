package tastytest.testsuite

import scala.concurrent.Future

class AsyncSuite extends TestSuite {
  final type TestBody = Future[Any]

  def testsuiteTests(): Seq[Test] = ???
}

abstract class TestSuite {

  type TestBody
  final type Test = TestImpl[TestBody]

  def testsuiteTests(): Seq[Test]

}

class TestImpl[T]

class MySuite extends AsyncSuite
