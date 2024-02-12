package tastytest.printing

trait Env[+I] { self =>
  val env: I
  final def evalInPrinter[O](printer: => Printer[I, O]): O = Env.runPrinter(printer)(self)
}

object Env {
  type JVMEnv = AnyHash & AnyClassName
  def runPrinter[I,O](printer: => Printer[I, O])(env: Env[I]): O = printer.println(env.env)
}

trait JVMEnvLive extends Env[AnyClassName & AnyHash] {
  final val env: AnyClassName & AnyHash = new AnyClassName with AnyHash
}

trait AnyHashLive extends Env[AnyHash] {
  final val env: AnyHash = new AnyHash{}
}

trait Printer[-I, +O] {
  def println(in: I): O
}

object Printer {
  object ObjectToString extends Printer[AnyClassName & AnyHash, Any => String] {
    def println(in: AnyClassName & AnyHash): Any => String = x =>
      s"${in.anyClassName(x)}@${in.anyHash(x)}"
  }
  object HashToString extends Printer[AnyHash, Any => String] {
    def println(in: AnyHash): Any => String = x =>
      s"@${in.anyHash(x)}"
  }
}

trait AnyClassName {
  def anyClassName[A](a: A): String = a.getClass.getName
}

trait AnyHash {
  def anyHash[A](a: A): Int = a.hashCode
}
