/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:Database.scala 6853 2006-03-20 16:58:47 +0100 (Mon, 20 Mar 2006) dubochet $


package scala.dbc
package syntax;


import java.net.URI;

object Database {

  def database (server:String, username:String, password:String): dbc.Database = {
    val uri = new URI(server);
    // Java 1.5 if (uri.toString().contains("postgres")) {
          if (uri.toString().indexOf("postgres") != -1) {
      new dbc.Database(new vendor.PostgreSQL {
        val uri = new URI(server);
        val user = username;
        val pass = password;
      })
    } else {
      throw new Exception("No DBMS vendor support could be found for the given URI");
    }
  }

}
