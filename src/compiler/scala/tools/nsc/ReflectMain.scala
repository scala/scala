package scala.tools.nsc

object ReflectMain extends Driver {

  override def newCompiler(): Global = new ReflectGlobal(settings, reporter)

}