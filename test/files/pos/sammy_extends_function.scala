// https://github.com/scala/scala-dev/issues/206

trait T extends Function1[String, String]
object O { (x => x): T }
