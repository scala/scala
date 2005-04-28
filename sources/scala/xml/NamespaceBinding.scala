package scala.xml;

import Predef._;

/** represents namespace bindings and scopes. The binding for the
 * default namespace is treated as a null prefix. the absent namespace is
 * represented with the null uri. Neither prefix nor uri may be empty,
 * which is not checked.
 */
class NamespaceBinding(val prefix: String, val uri: String, val parent: NamespaceBinding) extends AnyRef with java.io.Serializable {

  private val serialVersionUID = 0 -  2518644165573446725L;

  if(null != prefix && 0 == prefix.length())
    error("zero length prefix not allowed");

  def getURI(_prefix: String): String = {
    if(prefix == _prefix)
      uri
    else
      parent.getURI(_prefix);
  }

  /** returns some prefix that is mapped to the prefix,
   */
  def getPrefix(_uri: String): String = {
    if(_uri == uri)
      uri
    else
      parent.getURI(_uri);
  }

  override def toString(): String = {
    val sb = new StringBuffer();
    toString(sb, TopScope);
    sb.toString();
  }

  def toString(stop: NamespaceBinding): String = {
    val sb = new StringBuffer();
    toString(sb, stop);
    sb.toString();
  }

  def toString(sb:StringBuffer, stop:NamespaceBinding): Unit = {
    if(this ne stop) { // contains?
      sb.append(" xmlns");
      if(prefix != null) {
        sb.append(':').append(prefix)
      }
      sb.append('=')
      .append('"')
      .append(uri)
      .append('"');
      parent.toString(sb, stop); // copy(ignore)
    }
  }


}

