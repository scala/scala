package scala.tools.servlet.http ;

import java.io._;
/**
le processing actuel de la requête du client est reporté aux classes qui implemetent le trait HttpProcessor, ce trait decrit la façon avec laquelle la classe Httpd peut appeler le processeur pour repondre à la requête du client.
*/
 trait HttpProcessor {
   def processRequest(out: HttpOutputStream): Unit ={};
}
