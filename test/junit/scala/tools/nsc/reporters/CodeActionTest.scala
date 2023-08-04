package scala.tools.nsc.reporters

import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class CodeActionTest extends AbstractCodeActionTest {
  override def compilerArgs: String = "-Ystop-after:refchecks -deprecation -Xlint"
}
