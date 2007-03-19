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

var svn_root = 'http://scalasvn.epfl.ch/cgi-bin/viewvc.cgi/scala/branches/2.4.0-RC2';
var lib_svn_root = svn_root + '/src/library/';
var comp_svn_root = svn_root + '/src/compiler/';
var actors_svn_root = svn_root + '/src/actors/';

// hash tables
var api = new Array()
var src = new Array()

function init() {
  // initialize Java classes
  api['java/io/BufferedReader']                   = java_api_root;
  api['java/io/BufferedWriter']                   = java_api_root;
  api['java/io/Console']                          = java_api_root; //(1.6)
  api['java/io/DataInputStream']                  = java_api_root;
  api['java/io/DataOutputStream']                 = java_api_root;
  api['java/io/File']                             = java_api_root;
  api['java/io/FileDescriptor']                   = java_api_root;
  api['java/io/FileReader']                       = java_api_root;
  api['java/io/FileWriter']                       = java_api_root;
  api['java/io/Flushable']                        = java_api_root; //interface (1.5)
  api['java/io/IOException']                      = java_api_root;
  api['java/io/InputStream']                      = java_api_root;
  api['java/io/OutputStream']                     = java_api_root;
  api['java/io/PrintStream']                      = java_api_root;
  api['java/io/PrintWriter']                      = java_api_root;
  api['java/io/Reader']                           = java_api_root;
  api['java/io/StringReader']                     = java_api_root;
  api['java/io/StringWriter']                     = java_api_root;
  api['java/io/Writer']                           = java_api_root;
  api['java/lang/ArithmeticException']            = java_api_root;
  api['java/lang/ArrayIndexOutOfBoundsException'] = java_api_root;
  api['java/lang/Boolean']                        = java_api_root;
  api['java/lang/Byte']                           = java_api_root;
  api['java/lang/Character']                      = java_api_root;
  api['java/lang/Class']                          = java_api_root;
  api['java/lang/ClassCastException']             = java_api_root;
  api['java/lang/Clonable']                       = java_api_root; //interface
  api['java/lang/CloneNotSupportedException']     = java_api_root;
  api['java/lang/Comparable']                     = java_api_root; //interface
  api['java/lang/Exception']                      = java_api_root;
  api['java/lang/Float']                          = java_api_root;
  api['java/lang/IllegalAccessException']         = java_api_root;
  api['java/lang/IllegalArgumentException']       = java_api_root;
  api['java/lang/IndexOutOfBoundsException']      = java_api_root;
  api['java/lang/InterruptedException']           = java_api_root;
  api['java/lang/Long']                           = java_api_root;
  api['java/lang/Math']                           = java_api_root;
  api['java/lang/NullPointerException']           = java_api_root;
  api['java/lang/NumberFormatException']          = java_api_root;
  api['java/lang/Object']                         = java_api_root;
  api['java/lang/Runnable']                       = java_api_root; //interface
  api['java/lang/Runtime']                        = java_api_root;
  api['java/lang/RuntimeException']               = java_api_root;
  api['java/lang/RuntimePermission']              = java_api_root;
  api['java/lang/Short']                          = java_api_root;
  api['java/lang/StrictMath']                     = java_api_root;
  api['java/lang/String']                         = java_api_root;
  api['java/lang/System']                         = java_api_root;
  api['java/lang/Thread']                         = java_api_root;
  api['java/lang/ThreadGroup']                    = java_api_root;
  api['java/lang/ThreadLocal']                    = java_api_root;
  api['java/lang/Throwable']                      = java_api_root;
  api['java/lang/Void']                           = java_api_root;
  api['java/math/BigDecimal']                     = java_api_root;
  api['java/math/BigInteger']                     = java_api_root;
  api['java/math/MathContext']                    = java_api_root;
  api['java/net/Authenticator']                   = java_api_root;
  api['java/net/BindException']                   = java_api_root;
  api['java/net/HttpURLConnection']               = java_api_root;
  api['java/net/IDN']                             = java_api_root; //(1.6)
  api['java/net/Inet4Address']                    = java_api_root;
  api['java/net/Inet6Address']                    = java_api_root;
  api['java/net/InetAddress']                     = java_api_root;
  api['java/net/NetPermission']                   = java_api_root;
  api['java/net/ProtocolException']               = java_api_root;
  api['java/net/Proxy']                           = java_api_root;
  api['java/net/ServerSocket']                    = java_api_root;
  api['java/net/Socket']                          = java_api_root;
  api['java/net/SocketException']                 = java_api_root;
  api['java/net/SocketPermission']                = java_api_root;
  api['java/net/URI']                             = java_api_root;
  api['java/net/URL']                             = java_api_root;
  api['java/net/URLClassLoader']                  = java_api_root;
  api['java/net/URLConnection']                   = java_api_root;
  api['java/net/URLDecoder']                      = java_api_root;
  api['java/net/URLStreamHandler']                = java_api_root;
  api['java/nio/Buffer']                          = java_api_root;
  api['java/nio/ByteBuffer']                      = java_api_root;
  api['java/nio/BufferOverflowException']         = java_api_root;
  api['java/nio/ByteOrder']                       = java_api_root;
  api['java/nio/CharBuffer']                      = java_api_root;
  api['java/nio/DoubleBuffer']                    = java_api_root;
  api['java/nio/FloatBuffer']                     = java_api_root;
  api['java/nio/IntBuffer']                       = java_api_root;
  api['java/nio/LongBuffer']                      = java_api_root;
  api['java/nio/MappedByteBuffer']                = java_api_root;
  api['java/nio/ShortBuffer']                     = java_api_root;
  api['java/nio/channels/Channels']               = java_api_root;
  api['java/nio/channels/DatagramChannel']        = java_api_root;
  api['java/nio/channels/FileChannel']            = java_api_root;
  api['java/nio/channels/FileLock']               = java_api_root;
  api['java/nio/channels/Pipe']                   = java_api_root;
  api['java/nio/channels/ReadableByteChannel']    = java_api_root;
  api['java/nio/channels/Selector']               = java_api_root;
  api['java/nio/channels/ServerSocketChannel']    = java_api_root;
  api['java/nio/channels/SocketChannel']          = java_api_root;
  api['java/nio/charset/CharsetDecoder']          = java_api_root;
  api['java/rmi/AccessException']                 = java_api_root;
  api['java/rmi/MarshalledObject']                = java_api_root;
  api['java/rmi/Naming']                          = java_api_root;
  api['java/rmi/RMISecurityManager']              = java_api_root;
  api['java/rmi/Remote']                          = java_api_root; //interface
  api['java/sql/Array']                           = java_api_root; //interface
  api['java/sql/Blob']                            = java_api_root; //interface
  api['java/sql/CallableStatement']               = java_api_root; //interface
  api['java/sql/Connection']                      = java_api_root; //interface
  api['java/sql/Date']                            = java_api_root;
  api['java/sql/DriverManager']                   = java_api_root;
  api['java/sql/ResultSet']                       = java_api_root; //interface
  api['java/sql/ResultSetMetaData']               = java_api_root; //interface
  api['java/sql/SQLPermission']                   = java_api_root;
  api['java/sql/Time']                            = java_api_root;
  api['java/sql/Timestamp']                       = java_api_root;
  api['java/sql/Types']                           = java_api_root;
  api['java/text/MessageFormat']                  = java_api_root;
  api['java/text/Annotation']                     = java_api_root;
  api['java/text/AttributedCharacterIterator']    = java_api_root;
  api['java/text/AttributedString']               = java_api_root;
  api['java/text/Bidi']                           = java_api_root;
  api['java/text/BreakIterator']                  = java_api_root;
  api['java/text/CharacterIterator']              = java_api_root; //interface
  api['java/text/ChoiceFormat']                   = java_api_root;
  api['java/text/CollationKey']                   = java_api_root;
  api['java/text/Collator']                       = java_api_root;
  api['java/text/DateFormat']                     = java_api_root;
  api['java/text/DecimalFormat']                  = java_api_root;
  api['java/text/FieldPosition']                  = java_api_root;
  api['java/text/Format']                         = java_api_root;
  api['java/text/Formatter']                      = java_api_root;
  api['java/text/MessageFormat']                  = java_api_root;
  api['java/text/Normalizer']                     = java_api_root;
  api['java/text/NumberFormat']                   = java_api_root;
  api['java/text/ParseException']                 = java_api_root;
  api['java/text/ParsePosition']                  = java_api_root;
  api['java/text/SimpleDateFormat']               = java_api_root;
  api['java/text/StringCharacterIterator']        = java_api_root;
  api['java/util/Arrays']                         = java_api_root;
  api['java/util/BitSet']                         = java_api_root;
  api['java/util/Calendar']                       = java_api_root;
  api['java/util/Collection']                     = java_api_root; //interface
  api['java/util/Collections']                    = java_api_root;
  api['java/util/Comparator']                     = java_api_root; //interface
  api['java/util/Currency']                       = java_api_root;
  api['java/util/Date']                           = java_api_root;
  api['java/util/Dictionary']                     = java_api_root;
  api['java/util/Formatter']                      = java_api_root;
  api['java/util/HashMap']                        = java_api_root;
  api['java/util/HashSet']                        = java_api_root;
  api['java/util/Hashapi']                      = java_api_root;
  api['java/util/IllegalFormatException']         = java_api_root;
  api['java/util/LinkedHashMap']                  = java_api_root;
  api['java/util/LinkedHashSet']                  = java_api_root;
  api['java/util/LinkedList']                     = java_api_root;
  api['java/util/List']                           = java_api_root; //interface
  api['java/util/Map']                            = java_api_root;
  api['java/util/Observable']                     = java_api_root;
  api['java/util/PriorityQueue']                  = java_api_root;
  api['java/util/Properties']                     = java_api_root;
  api['java/util/Random']                         = java_api_root;
  api['java/util/ResourceBundle']                 = java_api_root;
  api['java/util/Scanner']                        = java_api_root; //(1.5)
  api['java/util/Set']                            = java_api_root;
  api['java/util/Stack']                          = java_api_root;
  api['java/util/StringTokenizer']                = java_api_root;
  api['java/util/Timer']                          = java_api_root;
  api['java/util/TimerTask']                      = java_api_root;
  api['java/util/TimeZone']                       = java_api_root;
  api['java/util/TreeMap']                        = java_api_root;
  api['java/util/TreeSet']                        = java_api_root;
  api['java/util/UUID']                           = java_api_root;
  api['java/util/Vector']                         = java_api_root;
  api['java/util/WeakHashMap']                    = java_api_root;

  // initialize Scala primitive classes
  api['scala/Any']                             = scala_api_root;
  api['scala/AnyRef']                          = scala_api_root;
  api['scala/AnyVal']                          = scala_api_root;
  api['scala/Boolean']                         = scala_api_root;
  api['scala/Byte']                            = scala_api_root;
  api['scala/Char']                            = scala_api_root;
  api['scala/Double']                          = scala_api_root;
  api['scala/Float']                           = scala_api_root;
  api['scala/Int']                             = scala_api_root;
  api['scala/Null']                            = scala_api_root;
  api['scala/Long']                            = scala_api_root;
  api['scala/Nothing']                         = scala_api_root;
  api['scala/Null']                            = scala_api_root;
  api['scala/Short']                           = scala_api_root;
  api['scala/Unit']                            = scala_api_root;
  api['scala/runtime/BoxedBoolean']            = scala_api_root;
  api['scala/runtime/BoxedByte']               = scala_api_root;
  api['scala/runtime/BoxedChar']               = scala_api_root;
  api['scala/runtime/BoxedDouble']             = scala_api_root;
  api['scala/runtime/BoxedInt']                = scala_api_root;
  api['scala/runtime/BoxedFloat']              = scala_api_root;
  api['scala/runtime/BoxedLong']               = scala_api_root;
  api['scala/runtime/BoxedNumber']             = scala_api_root;
  api['scala/runtime/BoxedShort']              = scala_api_root;

  // initialize Ant classes
  api['org/apache/tools/ant/AntClassLoader']            = ant_api_root;
  api['org/apache/tools/ant/AntTypeDefinition']         = ant_api_root;
  api['org/apache/tools/ant/BuildEvent']                = ant_api_root;
  api['org/apache/tools/ant/ComponentHelper']           = ant_api_root;
  api['org/apache/tools/ant/DefaultLogger']             = ant_api_root;
  api['org/apache/tools/ant/DemuxInputStream']          = ant_api_root;
  api['org/apache/tools/ant/DemuxOutputStream']         = ant_api_root;
  api['org/apache/tools/ant/Diagnostics']               = ant_api_root;
  api['org/apache/tools/ant/DirectoryScanner']          = ant_api_root;
  api['org/apache/tools/ant/Project']                   = ant_api_root;
  api['org/apache/tools/ant/Target']                    = ant_api_root;
  api['org/apache/tools/ant/Task']                      = ant_api_root;
  api['org/apache/tools/ant/taskdefs/Ant']              = ant_api_root;
  api['org/apache/tools/ant/taskdefs/Available']        = ant_api_root;
  api['org/apache/tools/ant/taskdefs/MatchingTask']     = ant_api_root;
  api['org/apache/tools/ant/types/FileSet']             = ant_api_root;
  api['org/apache/tools/ant/types/FilterChain']         = ant_api_root;
  api['org/apache/tools/ant/types/FilterSet']           = ant_api_root;
  api['org/apache/tools/ant/types/Path']                = ant_api_root;
  api['org/apache/tools/ant/types/Reference']           = ant_api_root;
  api['org/apache/tools/ant/types/Resource']            = ant_api_root;
  api['org/apache/tools/ant/util/FileUtils']            = ant_api_root;
  api['org/apache/tools/mail/MailMessage']              = ant_api_root;
  api['org/apache/tools/zip/ZipFile']                   = ant_api_root;

  // initialize Eclipse classes
  api['org/eclipse/ant/core/Property']                  = eclipse_api_root;
  api['org/eclipse/ant/core/Task']                      = eclipse_api_root;
  api['org/eclipse/ant/core/Type']                      = eclipse_api_root;
  api['org/eclipse/core/runtime/Assert']                = eclipse_api_root;
  api['org/eclipse/core/runtime/AssertFailedException'] = eclipse_api_root;
  api['org/eclipse/core/runtime/CoreException']         = eclipse_api_root;
  api['org/eclipse/core/runtime/Path']                  = eclipse_api_root;
  api['org/eclipse/core/runtime/Platform']              = eclipse_api_root;
  api['org/eclipse/core/runtime/Plugin']                = eclipse_api_root;
  api['org/eclipse/core/runtime/Preferences']           = eclipse_api_root;
  api['org/eclipse/core/runtime/RegistryFactory']       = eclipse_api_root;
  api['org/eclipse/core/runtime/Status']                = eclipse_api_root;
  api['org/eclipse/debug/core/ILaunch']                 = eclipse_api_root;
  api['org/eclipse/debug/core/ILaunchConfiguration']    = eclipse_api_root;
  api['org/eclipse/debug/core/ILaunchManager']          = eclipse_api_root;
  api['org/eclipse/debug/core/ILaunchMode']             = eclipse_api_root;
  api['org/eclipse/debug/core/IProcessFactory']         = eclipse_api_root;
  api['org/eclipse/jdt/core/IAccessRule']               = eclipse_api_root;
  api['org/eclipse/jdt/core/IBuffer']                   = eclipse_api_root;
  api['org/eclipse/jdt/core/IClassFile']                = eclipse_api_root;
  api['org/eclipse/jdt/core/IClasspathEntry']           = eclipse_api_root;
  api['org/eclipse/jdt/core/ICodeAssist']               = eclipse_api_root;
  api['org/eclipse/jdt/core/ICodeFormatter']            = eclipse_api_root;
  api['org/eclipse/jdt/core/ICompilationUnit']          = eclipse_api_root;
  api['org/eclipse/jdt/core/IField']                    = eclipse_api_root;
  api['org/eclipse/jdt/core/IImportContainer']          = eclipse_api_root;
  api['org/eclipse/jdt/core/IInitializer']              = eclipse_api_root;
  api['org/eclipse/jdt/core/IJavaElement']              = eclipse_api_root;
  api['org/eclipse/jdt/core/IMember']                   = eclipse_api_root;
  api['org/eclipse/jdt/core/IMethod']                   = eclipse_api_root;
  api['org/eclipse/jdt/core/IOpenable']                 = eclipse_api_root;
  api['org/eclipse/jdt/core/IParent']                   = eclipse_api_root;
  api['org/eclipse/jdt/core/ISourceRange']              = eclipse_api_root;
  api['org/eclipse/jdt/core/IType']                     = eclipse_api_root;
  api['org/eclipse/jdt/core/IWorkingCopy']              = eclipse_api_root;
  api['org/eclipse/jdt/launching/JavaRuntime']          = eclipse_api_root;
  api['org/eclipse/jdt/launching/LibraryLocation']      = eclipse_api_root;
  api['org/eclipse/jdt/launching/PropertyChangeEvent']  = eclipse_api_root;
  api['org/eclipse/jdt/launching/SocketUtil']           = eclipse_api_root;
  api['org/eclipse/jdt/launching/VMStandin']            = eclipse_api_root;

  // initialize FJBG classes
  api['ch/epfl/lamp/fjbg/FJBGContext']                  = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JAccessFlags']                 = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JArrayType']                   = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JAttribute']                   = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JAttributeFactory']            = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JClass']                       = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JCode']                        = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JCodeAttribute']               = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JConstantPool']                = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JExtendedCode']                = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JField']                       = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JFieldOrMethod']               = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JInnerClassesAttribute']       = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JLabel']                       = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JLineNumberapiAttribute']      = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JLocalVariable']               = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JMember']                      = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JMethod']                      = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JMethodType']                  = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JObjectType']                  = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JOpcode']                      = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JOtherAttribute']              = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JReferenceType']               = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JSourceFileAttribute']         = fjbg_api_root;
  api['ch/epfl/lamp/fjbg/JType']                        = fjbg_api_root;
  api['ch/epfl/lamp/util/ByteArray']                    = fjbg_api_root;

  // scala-library-src.jar
  src['scala/Annotation']                               = lib_svn_root;
  src['scala/Application']                              = lib_svn_root;
  src['scala/Array']                                    = lib_svn_root;
  src['scala/Attribute']                                = lib_svn_root;
  src['scala/BigInt']                                   = lib_svn_root;
  src['scala/BufferedIterator']                         = lib_svn_root;
  src['scala/Cell']                                     = lib_svn_root;
  src['scala/Console']                                  = lib_svn_root; // object
  src['scala/Enumeration']                              = lib_svn_root;
  src['scala/List']                                     = lib_svn_root;
  src['scala/MatchError']                               = lib_svn_root;
  src['scala/Math']                                     = lib_svn_root; // object
  src['scala/Nil']                                      = lib_svn_root; // object
  src['scala/None']                                     = lib_svn_root; // object
  src['scala/Option']                                   = lib_svn_root;
  src['scala/Predef']                                   = lib_svn_root; // object
  src['scala/Seq']                                      = lib_svn_root;
  src['scala/Some']                                     = lib_svn_root;
  src['scala/Stream']                                   = lib_svn_root;
  src['scala/Symbol']                                   = lib_svn_root;
  src['scala/cloneable']                                = lib_svn_root;
  src['scala/deprecated']                               = lib_svn_root;
  src['scala/remote']                                   = lib_svn_root;
  src['scala/serializable']                             = lib_svn_root;
  src['scala/throws']                                   = lib_svn_root;
  src['scala/transient']                                = lib_svn_root;
  src['scala/unchecked']                                = lib_svn_root;
  src['scala/unsealed']                                 = lib_svn_root;
  src['scala/volatile']                                 = lib_svn_root;

  src['scala/collection/BitSet']                        = lib_svn_root;
  src['scala/collection/Map']                           = lib_svn_root;
  src['scala/collection/MapProxy']                      = lib_svn_root;
  src['scala/collection/Ranged']                        = lib_svn_root;
  src['scala/collection/Set']                           = lib_svn_root;
  src['scala/collection/SetProxy']                      = lib_svn_root;
  src['scala/collection/Sorted']                        = lib_svn_root;
  src['scala/collection/SortedMap']                     = lib_svn_root;
  src['scala/collection/SortedSet']                     = lib_svn_root;
  src['scala/collection/immutable/BitSet']              = lib_svn_root;
  src['scala/collection/immutable/HashMap']             = lib_svn_root;
  src['scala/collection/immutable/HashSet']             = lib_svn_root;
  src['scala/collection/mutable/ArrayBuffer']           = lib_svn_root;
  src['scala/collection/mutable/BitSet']                = lib_svn_root;
  src['scala/collection/mutable/HashMap']               = lib_svn_root;
  src['scala/collection/mutable/HashSet']               = lib_svn_root;

  src['scala/util/Fluid']                               = lib_svn_root;
  src['scala/util/RichSorting']                         = lib_svn_root;
  src['scala/util/Sorting']                             = lib_svn_root;

  src['scala/xml/Atom']                                 = lib_svn_root;
  src['scala/xml/Comment']                              = lib_svn_root;
  src['scala/xml/Document']                             = lib_svn_root;
  src['scala/xml/Elem']                                 = lib_svn_root;
  src['scala/xml/Entityef']                             = lib_svn_root;
  src['scala/xml/Group']                                = lib_svn_root;
  src['scala/xml/HasKeyValue']                          = lib_svn_root;
  src['scala/xml/Node']                                 = lib_svn_root;
  src['scala/xml/NodeBuffer']                           = lib_svn_root;
  src['scala/xml/NodeSeq']                              = lib_svn_root;
  src['scala/xml/NodeTraverser']                        = lib_svn_root;
  src['scala/xml/Null']                                 = lib_svn_root;
  src['scala/xml/PrettyPrinter']                        = lib_svn_root;
  src['scala/xml/SpecialNode']                          = lib_svn_root;
  src['scala/xml/Text']                                 = lib_svn_root;
  src['scala/xml/TextBuffer']                           = lib_svn_root;
  src['scala/xml/TypeSymbol']                           = lib_svn_root;
  src['scala/xml/Unparsed']                             = lib_svn_root;
  src['scala/xml/UnprefixedAttribute']                  = lib_svn_root;

  // scala-actors-src.jar
  src['scala/actors/Actor']                             = actors_svn_root;
  src['scala/actors/Debug']                             = actors_svn_root;
  // declared in 'Scheduler'
  //src['scala/actors/IScheduler']                        = actors_svn_root;
  src['scala/actors/Reaction']                          = actors_svn_root;
  src['scala/actors/Scheduler']                         = actors_svn_root;

  src['scala/actors/remote/JavaSerizalizer']            = actors_svn_root;
  src['scala/actors/remote/Service']                    = actors_svn_root;

  // scala-compiler-src.jar
  src['scala/tools/nsc/Global']                         = comp_svn_root;
  src['scala/tools/nsc/symtab/Definitions']             = comp_svn_root;

  var elems = document.getElementsByTagName('a');
  for (i = 0; i < elems.length; i++) {
    try {
      key = elems[i].getAttribute('class');
      href = elems[i].getAttribute('href');
      api_root = api[key];
      if (api_root != null) {
        href1 = href.substring(href.lastIndexOf("#"))
        value = api_root + key + ".html" + href1;
        elems[i].setAttribute('href', value);
      }
      src_root = src[key];
      if (src_root != null) {
        value = src_root + key + '.scala?view=markup';
        elems[i].setAttribute('href', value);
        elems[i].setAttribute('target' , '_top');
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

