package scala.xml;

/** represents namespace bindings and scopes. The binding for the
 * default namespace is treated as a null prefix. the absent namespace is
 * represented with the null uri. Neither prefix nor uri may be empty,
 * which is not checked.
 */
class NamespaceBinding(val prefix: String, val uri: String, val parent: NamespaceBinding) {

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

}

