package scala.tools.servlet;

/** status constants for servlets
 *  @todo i18n
 */
object Status {

  final val OKAY            = 50;
  final val NOT_IMPLEMENTED = 60;
  final val FORBIDDEN       = 70;
  final val NOT_FOUND       = 80;
  final val BAD_REQUEST     = 90;
  final val INTERNAL_ERROR  = 99;

  def getMessage (code:int): String = code match {

    case OKAY            =>  "OK"
    case NOT_IMPLEMENTED =>  "Not Implemented"
    case FORBIDDEN       =>  "Forbidden"
    case NOT_FOUND       =>  "Not Found"
    case BAD_REQUEST     =>  "Bad Request"
    case INTERNAL_ERROR  =>  "Server Internal Error"
    case _ =>  "Unknown Code (" + code + ")"

  }
}
