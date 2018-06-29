
class Deprecation extends Deprecated {
  final val annotationType = classOf[Deprecated]
}

class Suppression extends SuppressWarnings {
  final val annotationType = classOf[SuppressWarnings]

  def value = Array("unchecked")
}

class Retention(runtime: Boolean) extends java.lang.annotation.Retention {
  final val annotationType = classOf[Retention]

  def value =
    if (runtime) java.lang.annotation.RetentionPolicy.RUNTIME
    else java.lang.annotation.RetentionPolicy.SOURCE
}

object Test extends App {
  new Deprecation
  new Suppression
  new Retention(true)
}
