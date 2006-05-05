<!--
function setWindowTitle(title) {
    parent.document.title = title;
}

var api_root = 'http://java.sun.com/j2se/1.5.0/docs/api/';
//var api_root = 'file:///home/linuxsoft/apps/java/docs/api/';

// hashtable
var table = new Array()

function init() {
  // initialize hashtable
  table['java_lang_Long']             = true;
  table['java_lang_Object']           = true;
  table['java_lang_String']           = true;
  table['java_net_URI']               = true;
  table['java_sql_Connection']        = true;
  table['java_sql_ResultSetMetaData'] = true;
  table['java_util_Properties']       = true;
  table['scala_Unit'] = true;

  var elems = document.getElementsByTagName('a');
  for (i = 0; i < elems.length; i++) { 
    try {
      key = elems[i].getAttribute('class');
      if (table[key] != null) {
        value = api_root + key.replace(/_/g, "/") + ".html";
        elems[i].setAttribute('href', value);
      }
    }
    catch (e) {
      // do nothing
    }
  } 
}
-->
