
package example

/** There is nothing extraordinary about this code,
 *  but the file name includes backticks, which must be encoded in a URI.
 */
final case class `X-Upload-Content-Type` private (contentType: String)
