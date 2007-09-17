package scala.xml.include

/**
 * <p>
 * A <code>CircularIncludeException</code> is thrown when
 * an included document attempts to include itself or
 * one of its ancestor documents.
 * </p>
 */
class CircularIncludeException(message: String) extends XIncludeException {

    /**
     * Constructs a <code>CircularIncludeException</code> with <code>null</code>
     * as its error detail message.
     */
    def this() = this(null);

}
