package scala.tools.servlet;

import java.io._;
import java.util._;
import java.net._;

object SERVLET {

  final val  STATUS_OKAY  = 50;
  final val  STATUS_NOT_IMPLEMENTED= 60;
  final val STATUS_FORBIDDEN=70;
  final val STATUS_NOT_FOUND=80;
  final val  STATUS_BAD_REQUEST= 90;
  final val STATUS_INTERNAL_ERROR =99;



  def getCodeMessage (code:int): String = code match {

    case STATUS_OKAY =>  "OK"
    case  STATUS_BAD_REQUEST=> "Bad Request"
    case STATUS_FORBIDDEN =>  "Forbidden"
    case STATUS_NOT_FOUND =>  "Not Found"
    case STATUS_INTERNAL_ERROR =>  "Server Internal Error"
    case STATUS_NOT_IMPLEMENTED =>  "Not Implemented"
    case _ =>  "Unknown Code (" + code + ")"

  }
}
