//scalac: -Yannotations:scala.annotation
package p

// use nowarn without prefix
@nowarn
trait C {
  def f: nowarn  // but only in annotation context
}
