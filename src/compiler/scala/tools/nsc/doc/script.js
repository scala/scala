<!--
function setWindowTitle(title) {
  parent.document.title = title;
}

var java_api_root = 'http://java.sun.com/javase/6/docs/api/';
//var java_api_root = 'http://java.sun.com/j2se/1.5.0/docs/api/';
//var java_api_root = 'http://lamp.epfl.ch/~linuxsoft/java/jdk1.5/docs/api/';

var scala_doc_url = parent.document.URL;
var scala_api_root = scala_doc_url.substring(0, scala_doc_url.lastIndexOf("/")+1);

var ant_api_root = 'http://lamp.epfl.ch/~linuxsoft/ant/manual/api/';
//var ant_api_root = 'http://www.net-freaks.org/doc/ant-1.6.5/manual/api/';

var eclipse_api_root = 'http://help.eclipse.org/help32/topic/org.eclipse.platform.doc.isv/reference/api/';

var fjbg_api_root = 'http://lamp.epfl.ch/~linuxsoft/fjbg/api/';

// hashtable_classes
var table = new Array()

function init() {
  // initialize Java classes
  table['java_io_BufferedReader']                   = java_api_root;
  table['java_io_BufferedWriter']                   = java_api_root;
  table['java_io_Console']                          = java_api_root; //(1.6)
  table['java_io_DataInputStream']                  = java_api_root;
  table['java_io_DataOutputStream']                 = java_api_root;
  table['java_io_File']                             = java_api_root;
  table['java_io_FileDescriptor']                   = java_api_root;
  table['java_io_FileReader']                       = java_api_root;
  table['java_io_FileWriter']                       = java_api_root;
  table['java_io_Flushable']                        = java_api_root; //interface (1.5)
  table['java_io_IOException']                      = java_api_root;
  table['java_io_InputStream']                      = java_api_root;
  table['java_io_OutputStream']                     = java_api_root;
  table['java_io_PrintStream']                      = java_api_root;
  table['java_io_PrintWriter']                      = java_api_root;
  table['java_io_Reader']                           = java_api_root;
  table['java_io_StringReader']                     = java_api_root;
  table['java_io_StringWriter']                     = java_api_root;
  table['java_io_Writer']                           = java_api_root;
  table['java_lang_ArithmeticException']            = java_api_root;
  table['java_lang_ArrayIndexOutOfBoundsException'] = java_api_root;
  table['java_lang_Boolean']                        = java_api_root;
  table['java_lang_Byte']                           = java_api_root;
  table['java_lang_Character']                      = java_api_root;
  table['java_lang_Class']                          = java_api_root;
  table['java_lang_ClassCastException']             = java_api_root;
  table['java_lang_Clonable']                       = java_api_root; //interface
  table['java_lang_CloneNotSupportedException']     = java_api_root;
  table['java_lang_Comparable']                     = java_api_root; //interface
  table['java_lang_Exception']                      = java_api_root;
  table['java_lang_Float']                          = java_api_root;
  table['java_lang_IllegalAccessException']         = java_api_root;
  table['java_lang_IllegalArgumentException']       = java_api_root;
  table['java_lang_IndexOutOfBoundsException']      = java_api_root;
  table['java_lang_InterruptedException']           = java_api_root;
  table['java_lang_Long']                           = java_api_root;
  table['java_lang_Math']                           = java_api_root;
  table['java_lang_NullPointerException']           = java_api_root;
  table['java_lang_NumberFormatException']          = java_api_root;
  table['java_lang_Object']                         = java_api_root;
  table['java_lang_Runnable']                       = java_api_root; //interface
  table['java_lang_Runtime']                        = java_api_root;
  table['java_lang_RuntimeException']               = java_api_root;
  table['java_lang_RuntimePermission']              = java_api_root;
  table['java_lang_Short']                          = java_api_root;
  table['java_lang_StrictMath']                     = java_api_root;
  table['java_lang_String']                         = java_api_root;
  table['java_lang_System']                         = java_api_root;
  table['java_lang_Thread']                         = java_api_root;
  table['java_lang_ThreadGroup']                    = java_api_root;
  table['java_lang_ThreadLocal']                    = java_api_root;
  table['java_lang_Throwable']                      = java_api_root;
  table['java_lang_Void']                           = java_api_root;
  table['java_math_BigDecimal']                     = java_api_root;
  table['java_math_BigInteger']                     = java_api_root;
  table['java_math_MathContext']                    = java_api_root;
  table['java_net_Authenticator']                   = java_api_root;
  table['java_net_BindException']                   = java_api_root;
  table['java_net_HttpURLConnection']               = java_api_root;
  table['java_net_IDN']                             = java_api_root; //(1.6)
  table['java_net_Inet4Address']                    = java_api_root;
  table['java_net_Inet6Address']                    = java_api_root;
  table['java_net_InetAddress']                     = java_api_root;
  table['java_net_NetPermission']                   = java_api_root;
  table['java_net_ProtocolException']               = java_api_root;
  table['java_net_Proxy']                           = java_api_root;
  table['java_net_ServerSocket']                    = java_api_root;
  table['java_net_Socket']                          = java_api_root;
  table['java_net_SocketException']                 = java_api_root;
  table['java_net_SocketPermission']                = java_api_root;
  table['java_net_URI']                             = java_api_root;
  table['java_net_URL']                             = java_api_root;
  table['java_net_URLClassLoader']                  = java_api_root;
  table['java_net_URLConnection']                   = java_api_root;
  table['java_net_URLDecoder']                      = java_api_root;
  table['java_net_URLStreamHandler']                = java_api_root;
  table['java_nio_Buffer']                          = java_api_root;
  table['java_nio_ByteBuffer']                      = java_api_root;
  table['java_nio_BufferOverflowException']         = java_api_root;
  table['java_nio_ByteOrder']                       = java_api_root;
  table['java_nio_CharBuffer']                      = java_api_root;
  table['java_nio_DoubleBuffer']                    = java_api_root;
  table['java_nio_FloatBuffer']                     = java_api_root;
  table['java_nio_IntBuffer']                       = java_api_root;
  table['java_nio_LongBuffer']                      = java_api_root;
  table['java_nio_MappedByteBuffer']                = java_api_root;
  table['java_nio_ShortBuffer']                     = java_api_root;
  table['java_nio_channels_Channels']               = java_api_root;
  table['java_nio_channels_DatagramChannel']        = java_api_root;
  table['java_nio_channels_FileChannel']            = java_api_root;
  table['java_nio_channels_FileLock']               = java_api_root;
  table['java_nio_channels_Pipe']                   = java_api_root;
  table['java_nio_channels_ReadableByteChannel']    = java_api_root;
  table['java_nio_channels_Selector']               = java_api_root;
  table['java_nio_channels_ServerSocketChannel']    = java_api_root;
  table['java_nio_channels_SocketChannel']          = java_api_root;
  table['java_nio_charset_CharsetDecoder']          = java_api_root;
  table['java_rmi_AccessException']                 = java_api_root;
  table['java_rmi_MarshalledObject']                = java_api_root;
  table['java_rmi_Naming']                          = java_api_root;
  table['java_rmi_RMISecurityManager']              = java_api_root;
  table['java_rmi_Remote']                          = java_api_root; //interface
  table['java_sql_Array']                           = java_api_root; //interface
  table['java_sql_Blob']                            = java_api_root; //interface
  table['java_sql_CallableStatement']               = java_api_root; //interface
  table['java_sql_Connection']                      = java_api_root; //interface
  table['java_sql_Date']                            = java_api_root;
  table['java_sql_DriverManager']                   = java_api_root;
  table['java_sql_ResultSet']                       = java_api_root; //interface
  table['java_sql_ResultSetMetaData']               = java_api_root; //interface
  table['java_sql_SQLPermission']                   = java_api_root;
  table['java_sql_Time']                            = java_api_root;
  table['java_sql_Timestamp']                       = java_api_root;
  table['java_sql_Types']                           = java_api_root;
  table['java_text_MessageFormat']                  = java_api_root;
  table['java_text_Annotation']                     = java_api_root;
  table['java_text_AttributedCharacterIterator']    = java_api_root;
  table['java_text_AttributedString']               = java_api_root;
  table['java_text_Bidi']                           = java_api_root;
  table['java_text_BreakIterator']                  = java_api_root;
  table['java_text_CharacterIterator']              = java_api_root; //interface
  table['java_text_ChoiceFormat']                   = java_api_root;
  table['java_text_CollationKey']                   = java_api_root;
  table['java_text_Collator']                       = java_api_root;
  table['java_text_DateFormat']                     = java_api_root;
  table['java_text_DecimalFormat']                  = java_api_root;
  table['java_text_FieldPosition']                  = java_api_root;
  table['java_text_Format']                         = java_api_root;
  table['java_text_Formatter']                      = java_api_root;
  table['java_text_MessageFormat']                  = java_api_root;
  table['java_text_Normalizer']                     = java_api_root;
  table['java_text_NumberFormat']                   = java_api_root;
  table['java_text_ParseException']                 = java_api_root;
  table['java_text_ParsePosition']                  = java_api_root;
  table['java_text_SimpleDateFormat']               = java_api_root;
  table['java_text_StringCharacterIterator']        = java_api_root;
  table['java_util_Arrays']                         = java_api_root;
  table['java_util_BitSet']                         = java_api_root;
  table['java_util_Calendar']                       = java_api_root;
  table['java_util_Collection']                     = java_api_root; //interface
  table['java_util_Collections']                    = java_api_root;
  table['java_util_Comparator']                     = java_api_root; //interface
  table['java_util_Currency']                       = java_api_root;
  table['java_util_Date']                           = java_api_root;
  table['java_util_Dictionary']                     = java_api_root;
  table['java_util_Formatter']                      = java_api_root;
  table['java_util_HashMap']                        = java_api_root;
  table['java_util_HashSet']                        = java_api_root;
  table['java_util_Hashtable']                      = java_api_root;
  table['java_util_IllegalFormatException']         = java_api_root;
  table['java_util_LinkedHashMap']                  = java_api_root;
  table['java_util_LinkedHashSet']                  = java_api_root;
  table['java_util_LinkedList']                     = java_api_root;
  table['java_util_List']                           = java_api_root; //interface
  table['java_util_Map']                            = java_api_root;
  table['java_util_Observable']                     = java_api_root;
  table['java_util_PriorityQueue']                  = java_api_root;
  table['java_util_Properties']                     = java_api_root;
  table['java_util_Random']                         = java_api_root;
  table['java_util_ResourceBundle']                 = java_api_root;
  table['java_util_Scanner']                        = java_api_root; //(1.5)
  table['java_util_Set']                            = java_api_root;
  table['java_util_Stack']                          = java_api_root;
  table['java_util_StringTokenizer']                = java_api_root;
  table['java_util_Timer']                          = java_api_root;
  table['java_util_TimerTask']                      = java_api_root;
  table['java_util_TimeZone']                       = java_api_root;
  table['java_util_TreeMap']                        = java_api_root;
  table['java_util_TreeSet']                        = java_api_root;
  table['java_util_UUID']                           = java_api_root;
  table['java_util_Vector']                         = java_api_root;
  table['java_util_WeakHashMap']                    = java_api_root;

  // initialize Scala primitive classes
  table['scala_Any']                             = scala_api_root;
  table['scala_AnyRef']                          = scala_api_root;
  table['scala_AnyVal']                          = scala_api_root;
  table['scala_Boolean']                         = scala_api_root;
  table['scala_Byte']                            = scala_api_root;
  table['scala_Char']                            = scala_api_root;
  table['scala_Double']                          = scala_api_root;
  table['scala_Float']                           = scala_api_root;
  table['scala_Int']                             = scala_api_root;
  table['scala_Null']                            = scala_api_root;
  table['scala_Long']                            = scala_api_root;
  table['scala_Nothing']                         = scala_api_root;
  table['scala_Null']                            = scala_api_root;
  table['scala_Short']                           = scala_api_root;
  table['scala_Unit']                            = scala_api_root;
  table['scala_runtime_BoxedBoolean']            = scala_api_root;
  table['scala_runtime_BoxedByte']               = scala_api_root;
  table['scala_runtime_BoxedChar']               = scala_api_root;
  table['scala_runtime_BoxedDouble']             = scala_api_root;
  table['scala_runtime_BoxedInt']                = scala_api_root;
  table['scala_runtime_BoxedFloat']              = scala_api_root;
  table['scala_runtime_BoxedLong']               = scala_api_root;
  table['scala_runtime_BoxedNumber']             = scala_api_root;
  table['scala_runtime_BoxedShort']              = scala_api_root;

  // initialize Ant classes
  table['org_apache_tools_ant_AntClassLoader']            = ant_api_root;
  table['org_apache_tools_ant_AntTypeDefinition']         = ant_api_root;
  table['org_apache_tools_ant_BuildEvent']                = ant_api_root;
  table['org_apache_tools_ant_ComponentHelper']           = ant_api_root;
  table['org_apache_tools_ant_DefaultLogger']             = ant_api_root;
  table['org_apache_tools_ant_DemuxInputStream']          = ant_api_root;
  table['org_apache_tools_ant_DemuxOutputStream']         = ant_api_root;
  table['org_apache_tools_ant_Diagnostics']               = ant_api_root;
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

  // initialize FJBG classes
  table['ch_epfl_lamp_fjbg_FJBGContext']                  = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JAccessFlags']                 = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JArrayType']                   = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JAttribute']                   = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JAttributeFactory']            = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JClass']                       = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JCode']                        = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JCodeAttribute']               = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JConstantPool']                = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JExtendedCode']                = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JField']                       = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JFieldOrMethod']               = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JInnerClassesAttribute']       = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JLabel']                       = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JLineNumberTableAttribute']    = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JLocalVariable']               = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JMember']                      = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JMethod']                      = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JMethodType']                  = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JObjectType']                  = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JOpcode']                      = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JOtherAttribute']              = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JReferenceType']               = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JSourceFileAttribute']         = fjbg_api_root;
  table['ch_epfl_lamp_fjbg_JType']                        = fjbg_api_root;
  table['ch_epfl_lamp_util_ByteArray']                    = fjbg_api_root;

  var elems = document.getElementsByTagName('a');
  for (i = 0; i < elems.length; i++) {
    try {
      key = elems[i].getAttribute('class');
      href = elems[i].getAttribute('href');
      api_root = table[key];
      if (api_root != null) {
        href1 = href.substring(href.lastIndexOf("#"))
        value = api_root + key.replace(/_/g, "/") + ".html" + href1;
        elems[i].setAttribute('href', value);
      }
    }
    catch (e) {
      // do nothing
    }
  }
}

function getLocation() {
  kinds = parent.navigationFrame.document.getElementById("kinds");
  oldLocation = parent.classesFrame.window.location.href;
  //alert("oldLocation="+oldLocation);
  pos = oldLocation.lastIndexOf("#");
  classesURL = (pos > 0) ? oldLocation.substring(0, pos) : oldLocation;
  newLocation = classesURL + kinds.options[kinds.selectedIndex].value;
  //alert("newLocation="+newLocation);
  return newLocation;
}

function gotoKind() {
  parent.classesFrame.window.location = getLocation();
}

function resetKind() {
  kinds = parent.navigationFrame.document.getElementById("kinds");
  kinds.selectedIndex = 0;
}

function gotoName(letter) {
  parent.classesFrame.window.location = getLocation() + "_" + letter;
}
-->

