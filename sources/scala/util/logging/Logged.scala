package scala.util.logging;

/**
 *  Mixing in the trait Logged indicates that a class provides support
 *  for logging.
 */
trait Logged {
  /** this method should log the message given as argument somewhere
   * as a side-effect
   */
  def log(msg:String): Unit;
}
