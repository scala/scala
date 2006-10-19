<!--
function setWindowTitle(title) {
  parent.document.title = title;
}

var java_api_root = 'http://java.sun.com/j2se/1.5.0/docs/api/';
//var java_api_root = 'http://lamp.epfl.ch/~linuxsoft/java/jdk1.5/docs/api/';

var scala_doc_url = parent.document.URL;
var scala_api_root = scala_doc_url.substring(0, scala_doc_url.lastIndexOf("/")+1);

var ant_api_root = 'http://lamp.epfl.ch/~linuxsoft/ant/manual/api/';
//var ant_api_root = 'http://www.net-freaks.org/doc/ant-1.6.5/manual/api/';

var eclipse_api_root = 'http://help.eclipse.org/help32/topic/org.eclipse.platform.doc.isv/reference/api/';

// hashtable_classes
var table = new Array()

function init() {
  // initialize Java classes
  table['java_io_BufferedReader']     = java_api_root;
  table['java_io_BufferedWriter']     = java_api_root;
  table['java_io_DataInputStream']    = java_api_root;
  table['java_io_DataOutputStream']   = java_api_root;
  table['java_io_File']               = java_api_root;
  table['java_io_FileDescriptor']     = java_api_root;
  table['java_io_FileReader']         = java_api_root;
  table['java_io_FileWriter']         = java_api_root;
  table['java_io_IOException']        = java_api_root;
  table['java_io_InputStream']        = java_api_root;
  table['java_io_OutputStream']       = java_api_root;
  table['java_io_PrintStream']        = java_api_root;
  table['java_io_PrintWriter']        = java_api_root;
  table['java_io_Reader']             = java_api_root;
  table['java_io_StringReader']       = java_api_root;
  table['java_io_StringWriter']       = java_api_root;
  table['java_io_Writer']             = java_api_root;
  table['java_lang_Exception']        = java_api_root;
  table['java_lang_Long']             = java_api_root;
  table['java_lang_Object']           = java_api_root;
  table['java_lang_String']           = java_api_root;
  table['java_math_BigDecimal']       = java_api_root;
  table['java_math_BigInteger']       = java_api_root;
  table['java_math_MathContext']      = java_api_root;
  table['java_net_InetAddress']       = java_api_root;
  table['java_net_ServerSocket']      = java_api_root;
  table['java_net_Socket']            = java_api_root;
  table['java_net_SocketPermission']  = java_api_root;
  table['java_net_URI']               = java_api_root;
  table['java_net_URL']               = java_api_root;
  table['java_sql_Connection']        = java_api_root;
  table['java_sql_ResultSet']         = java_api_root;
  table['java_sql_ResultSetMetaData'] = java_api_root;
  table['java_util_LinkedHashMap']    = java_api_root;
  table['java_util_LinkedHashSet']    = java_api_root;
  table['java_util_Map']              = java_api_root;
  table['java_util_Properties']       = java_api_root;
  table['java_util_Random']           = java_api_root;
  table['java_util_Set']              = java_api_root;
  table['java_util_WeakHashMap']      = java_api_root;

  // initialize Scala primitive classes
  table['scala_Any']                  = scala_api_root;
  table['scala_Boolean']              = scala_api_root;
  table['scala_Byte']                 = scala_api_root;
  table['scala_Char']                 = scala_api_root;
  table['scala_Double']               = scala_api_root;
  table['scala_Float']                = scala_api_root;
  table['scala_Int']                  = scala_api_root;
  table['scala_Null']                 = scala_api_root;
  table['scala_Long']                 = scala_api_root;
  table['scala_Null']                 = scala_api_root;
  table['scala_Short']                = scala_api_root;
  table['scala_Unit']                 = scala_api_root;
  table['scala_runtime_BoxedInt']     = scala_api_root;
  table['scala_runtime_BoxeFloat']    = scala_api_root;
  table['scala_runtime_BoxedNumber']  = scala_api_root;

  // initialize Ant classes
  table['org_apache_tools_ant_BuildEvent']                = ant_api_root;
  table['org_apache_tools_ant_DirectoryScanner']          = ant_api_root;
  table['org_apache_tools_ant_Project']                   = ant_api_root;
  table['org_apache_tools_ant_Target']                    = ant_api_root;
  table['org_apache_tools_ant_Task']                      = ant_api_root;
  table['org_apache_tools_ant_taskdefs_Ant']              = ant_api_root;
  table['org_apache_tools_ant_taskdefs_Available']        = ant_api_root;
  table['org_apache_tools_ant_taskdefs_MatchingTask']     = ant_api_root;
  table['org_apache_tools_ant_types_FileSet']             = ant_api_root;
  table['org_apache_tools_ant_types_FilterChain']         = ant_api_root;
  table['org_apache_tools_ant_types_FilterSet']           = ant_api_root;
  table['org_apache_tools_ant_types_Path']                = ant_api_root;
  table['org_apache_tools_ant_types_Reference']           = ant_api_root;
  table['org_apache_tools_ant_types_Resource']            = ant_api_root;
  table['org_apache_tools_ant_util_FileUtils']            = ant_api_root;
  table['org_apache_tools_mail_MailMessage']              = ant_api_root;
  table['org_apache_tools_zip_ZipFile']                   = ant_api_root;

  // initialize Eclipse classes
  table['org_eclipse_ant_core_Property']                  = eclipse_api_root;
  table['org_eclipse_ant_core_Task']                      = eclipse_api_root;
  table['org_eclipse_ant_core_Type']                      = eclipse_api_root;
  table['org_eclipse_core_runtime_Assert']                = eclipse_api_root;
  table['org_eclipse_core_runtime_AssertFailedException'] = eclipse_api_root;
  table['org_eclipse_core_runtime_CoreException']         = eclipse_api_root;
  table['org_eclipse_core_runtime_Path']                  = eclipse_api_root;
  table['org_eclipse_core_runtime_Platform']              = eclipse_api_root;
  table['org_eclipse_core_runtime_Plugin']                = eclipse_api_root;
  table['org_eclipse_core_runtime_Preferences']           = eclipse_api_root;
  table['org_eclipse_core_runtime_RegistryFactory']       = eclipse_api_root;
  table['org_eclipse_core_runtime_Status']                = eclipse_api_root;
  table['org_eclipse_debug_core_ILaunch']                 = eclipse_api_root;
  table['org_eclipse_debug_core_ILaunchConfiguration']    = eclipse_api_root;
  table['org_eclipse_debug_core_ILaunchManager']          = eclipse_api_root;
  table['org_eclipse_debug_core_ILaunchMode']             = eclipse_api_root;
  table['org_eclipse_debug_core_IProcessFactory']         = eclipse_api_root;
  table['org_eclipse_jdt_core_IAccessRule']               = eclipse_api_root;
  table['org_eclipse_jdt_core_IBuffer']                   = eclipse_api_root;
  table['org_eclipse_jdt_core_IClassFile']                = eclipse_api_root;
  table['org_eclipse_jdt_core_IClasspathEntry']           = eclipse_api_root;
  table['org_eclipse_jdt_core_ICodeAssist']               = eclipse_api_root;
  table['org_eclipse_jdt_core_ICodeFormatter']            = eclipse_api_root;
  table['org_eclipse_jdt_core_ICompilationUnit']          = eclipse_api_root;
  table['org_eclipse_jdt_core_IField']                    = eclipse_api_root;
  table['org_eclipse_jdt_core_IImportContainer']          = eclipse_api_root;
  table['org_eclipse_jdt_core_IInitializer']              = eclipse_api_root;
  table['org_eclipse_jdt_core_IJavaElement']              = eclipse_api_root;
  table['org_eclipse_jdt_core_IMember']                   = eclipse_api_root;
  table['org_eclipse_jdt_core_IMethod']                   = eclipse_api_root;
  table['org_eclipse_jdt_core_IOpenable']                 = eclipse_api_root;
  table['org_eclipse_jdt_core_IParent']                   = eclipse_api_root;
  table['org_eclipse_jdt_core_ISourceRange']              = eclipse_api_root;
  table['org_eclipse_jdt_core_IType']                     = eclipse_api_root;
  table['org_eclipse_jdt_core_IWorkingCopy']              = eclipse_api_root;
  table['org_eclipse_jdt_launching_JavaRuntime']          = eclipse_api_root;
  table['org_eclipse_jdt_launching_LibraryLocation']      = eclipse_api_root;
  table['org_eclipse_jdt_launching_PropertyChangeEvent']  = eclipse_api_root;
  table['org_eclipse_jdt_launching_SocketUtil']           = eclipse_api_root;
  table['org_eclipse_jdt_launching_VMStandin']            = eclipse_api_root;

  var elems = document.getElementsByTagName('a');
  for (i = 0; i < elems.length; i++) {
    try {
      key = elems[i].getAttribute('class');
      api_root = table[key];
      if (api_root != null) {
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
