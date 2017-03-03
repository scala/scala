sealed abstract class TB
case object B extends TB
case object B2 extends TB

case class CC(tb: TB)
