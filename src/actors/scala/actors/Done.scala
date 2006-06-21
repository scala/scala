package scala.actors;

/**
 * @author Philipp Haller
 */
class Done extends Throwable {
  override def fillInStackTrace(): Throwable =
    this;
}

class ContinueException extends Throwable {
  override def fillInStackTrace(): Throwable =
    this;
}

class AbortException extends Throwable {
  override def fillInStackTrace(): Throwable =
    this;
}
