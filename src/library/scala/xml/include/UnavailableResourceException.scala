package scala.xml.include

/**
 * <p>
 * An <code>UnavailableResourceException</code> is thrown when
 * an included document cannot be found or loaded.
 * </p>
 *
 */
class UnavailableResourceException(message: String)
extends XIncludeException(message) {
  def this() = this(null);
}
