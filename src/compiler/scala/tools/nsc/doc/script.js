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
  table['java_io_BufferedReader']     = true;
  table['java_io_BufferedWriter']     = true;
  table['java_io_DataInputStream']    = true;
  table['java_io_DataOutputStream']   = true;
  table['java_io_File']               = true;
  table['java_io_FileDescriptor']     = true;
  table['java_io_FileReader']         = true;
  table['java_io_FileWriter']         = true;
  table['java_io_InputStream']        = true;
  table['java_io_OutputStream']       = true;
  table['java_io_PrintStream']        = true;
  table['java_io_PrintWriter']        = true;
  table['java_io_Reader']             = true;
  table['java_io_StringReader']       = true;
  table['java_io_StringWriter']       = true;
  table['java_io_Writer']             = true;
  table['java_lang_Exception']        = true;
  table['java_lang_Long']             = true;
  table['java_lang_Object']           = true;
  table['java_lang_String']           = true;
  table['java_math_BigDecimal']       = true;
  table['java_math_BigInteger']       = true;
  table['java_math_MathContext']      = true;
  table['java_net_InetAddress']       = true;
  table['java_net_ServerSocket']      = true;
  table['java_net_Socket']            = true;
  table['java_net_SocketPermission']  = true;
  table['java_net_URI']               = true;
  table['java_net_URL']               = true;
  table['java_sql_Connection']        = true;
  table['java_sql_ResultSet']         = true;
  table['java_sql_ResultSetMetaData'] = true;
  table['java_util_LinkedHashMap']    = true;
  table['java_util_LinkedHashSet']    = true;
  table['java_util_Map']              = true;
  table['java_util_Properties']       = true;
  table['java_util_Random']           = true;
  table['java_util_Set']              = true;
  table['java_util_WeakHashMap']      = true;
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
