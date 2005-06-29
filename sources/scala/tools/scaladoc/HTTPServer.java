/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2005, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scaladoc;

import java.net.*;
import java.io.*;
import java.util.*;
import java.util.regex.*;

/**
 * A Java server that can be invoked via HTTP.
 */
abstract class Servlet {

    /**
     * Name of the servlet.
     */
    public abstract String name();

    /**
     * Give the servlet a request and a place where to write its
     * answer.
     */
    public abstract void apply(Map req, Writer out);
}

/**
 * A servlet that echoes the query to its sender.
 */
class EchoServlet extends Servlet {

    public String name() {
        return "echoServlet";
    }

    public void apply(Map req, Writer out) {
        printMap(req, out);
    }

    /** Print a map. */
    public static void printMap(Map map, Writer out) {
        Iterator i = map.keySet().iterator();
        while(i.hasNext()) {
            String key = (String) i.next();
            String value = (String) map.get(key);
            try {
                out.write(key + " -> " + value + "\n");
            } catch (IOException e) {}
        }
    }
}

/**
 * HTTP server. It is in fact an extension of an example found in the
 * book "Java Network Programming" by Elliotte Rusty Harold that can
 * handle servlet invokations.
 */
public class HTTPServer extends Thread {

    private File documentRootDirectory;
    private String indexFileName = "index.html";
    private ServerSocket server;
    private int numThreads = 50;

    private Map/*<String,Servlet>*/ servletNamed;

    public HTTPServer(File documentRootDirectory,
                      int port,
                      String indexFileName) throws IOException {

        this.documentRootDirectory =
            documentRootDirectory == null ?
            new File(".") :
            documentRootDirectory;

        if (!this.documentRootDirectory.isDirectory()) {
            throw new IOException(this.documentRootDirectory
                                  + " does not exist as a directory");
        }
        this.indexFileName = indexFileName;
        this.server = new ServerSocket(port);
    }

    public HTTPServer(File documentRootDirectory, int port)
        throws IOException {
        this(documentRootDirectory, port, "index.html");
    }

    public HTTPServer(File documentRootDirectory) throws IOException {
        this(documentRootDirectory, 80, "index.html");
    }

    /**
     * Launch the server with a specified list of servlets.
     */
    public HTTPServer(File documentRootDirectory,
                      int port,
                      Servlet[] servlets) throws IOException {
        this(documentRootDirectory, port, "index.html");
        servletNamed = new HashMap();
        for (int i = 0; i < servlets.length; i++)
            servletNamed.put("/" + servlets[i].name(), servlets[i]);
    }

    public void run() {

        for (int i = 0; i < numThreads; i++) {
            Thread t = new Thread(
                                  new RequestProcessor(documentRootDirectory,
                                                       indexFileName,
                                                       servletNamed));
            t.start();
        }
        System.out.println("Accepting connections on port "
                           + server.getLocalPort());
        System.out.println("Document Root: " + documentRootDirectory);
        while (true) {
            try {
                Socket request = server.accept();
                InetSocketAddress addr = (InetSocketAddress) request.getRemoteSocketAddress();
                if (addr != null)
                    System.out.println("Connection from: " + addr.getAddress().getCanonicalHostName());
                RequestProcessor.processRequest(request);
            }
            catch (IOException e) {
            }
        }
    }

    public static void main(String[] args) {

        // get the Document root
        File docroot;
        try {
            docroot = new File(args[0]);
        }
        catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("Usage: java HTTPServer docroot port indexfile");
            return;
        }

        // set the port to listen on
        int port;
        try {
            port = Integer.parseInt(args[1]);
            if (port < 0 || port > 65535) port = 80;
        }
        catch (Exception e) {
            port = 80;
        }

        try {
            Servlet[] servlets = new Servlet[] { new EchoServlet() };
            HTTPServer webserver = new HTTPServer(docroot, port, servlets);
            webserver.start();
        }
        catch (IOException e) {
            System.out.println("Server could not start because of an "
                               + e.getClass());
            System.out.println(e);
        }
    }
}

class RequestProcessor implements Runnable {

    private static final String MIME_APPLICATION_OCTET_STREAM =
        "application/octet-stream";
    private static final String MIME_APPLICATION_PDF =
        "application/pdf";
    private static final String MIME_APPLICATION_POSTSCRIPT =
        "application/postscript";

    private static final String MIME_IMAGE_GIF  = "image/gif";
    private static final String MIME_IMAGE_JPEG = "image/jpeg";
    private static final String MIME_IMAGE_PNG  = "image/png";

    private static final String MIME_TEXT_HTML  = "text/html";
    private static final String MIME_TEXT_PLAIN = "text/plain";

    private final static String ENCODING_SCHEME = "iso-8859-1"; // "UTF-8"

    private static List pool = new LinkedList();
    private File documentRootDirectory;
    private String indexFileName = "index.html";
    private Map servletNamed;

    public RequestProcessor(File documentRootDirectory,
                            String indexFileName,
                            Map servletNamed) {

        if (documentRootDirectory.isFile()) {
            throw new IllegalArgumentException(
            "documentRootDirectory must be a directory, not a file");
        }
        this.documentRootDirectory = documentRootDirectory;
        try {
            this.documentRootDirectory
                = documentRootDirectory.getCanonicalFile();
        }
        catch (IOException e) {
        }
        if (indexFileName != null) this.indexFileName = indexFileName;
        this.servletNamed = servletNamed;
    }

    public static void processRequest(Socket request) {
        synchronized (pool) {
            pool.add(pool.size(), request);
            pool.notifyAll();
        }
    }

    /**
     * Parse a non-null query and put the bindings (key, value) into
     * the result map.
     */
    public static Map parseQuery(String query) {
        Map/*<String,String>*/ map = new HashMap();
        try {
            String[] bindings = query.split("\\&");
            String regexp = "([^=]*)=([^=]*)";
            Pattern p = Pattern.compile(regexp);
            for (int i = 0; i < bindings.length; i++) {
                Matcher m = p.matcher(bindings[i]);
                if (m.find()) {
                    String key = URLDecoder.decode(m.group(1), ENCODING_SCHEME);
                    String value = URLDecoder.decode(m.group(2), ENCODING_SCHEME);
                    map.put(key, value);
                }
            }
        }
        catch (UnsupportedEncodingException e) {
            System.out.println("Une exception: " + e);
        }
        return map;
    }

    private static String mkMIMEHeader(int statusCode, String statusMessage,
                                       String contentType, int contentLength) {
        StringBuffer buf = new StringBuffer();
        buf.append("HTTP/1.0 " + statusCode + " " + statusMessage + "\r\n");
        buf.append("Date: " + new Date() + "\r\n");
        buf.append("Server: HTTPServer 1.0\r\n");
        buf.append("Content-length: " + contentLength + "\r\n");
        buf.append("Content-type: " + contentType + "\r\n\r\n");
        return buf.toString();
    }

    private static void write(Writer out, String version,
                              int statusCode, String statusMessage,
                              String contentType, String contentData) {
        try {
            if (version.startsWith("HTTP/")) {  // send a MIME header
                String header = mkMIMEHeader(statusCode, statusMessage,
                                             contentType, contentData.length());
                out.write(header);
            }
            out.write(contentData);
            out.flush();
        }
        catch (IOException e) {
        }
    }

    private static void writeData(OutputStream out, String version,
                              int statusCode, String statusMessage,
                              String contentType, File theFile) {
        try {
            DataInputStream fis = new DataInputStream(new FileInputStream(theFile));
            byte[] theData = new byte[(int) theFile.length()];
            fis.readFully(theData);
            fis.close();
            if (version.startsWith("HTTP/")) {  // send a MIME header
                String header = mkMIMEHeader(statusCode, statusMessage,
                                             contentType, theData.length);
                out.write(header.getBytes());
            }
            out.write(theData);
            out.flush();
        }
        catch (IOException e) {
        }
    }

    private static void writeError(Writer out, String version,
                                   int statusCode, String statusMessage) {
        StringBuffer buf = new StringBuffer();
        buf.append("<html>\r\n");
        buf.append("<head><title>" + statusMessage + "</title>\r\n");
        buf.append("</head>\r\n");
        buf.append("<body>");
        buf.append("<h1>HTTP Error " + statusCode + ": " + statusMessage + "</h1>\r\n");
        buf.append("</body></html>\r\n");
        write(out, version, statusCode, statusMessage, MIME_TEXT_HTML, buf.toString());
    }

    private static void writeServlet(Writer out, String version,
                                     Servlet servlet, URI uri) {
        String query = uri.getRawQuery();
        Map map = (query != null) ? parseQuery(query) : new HashMap();
        write(out, version, 200, "OK", MIME_TEXT_HTML, "<!-- generated with Scala Servlet -->\r\n");
        servlet.apply(map, out);
        try {
            out.write("\n");
            out.flush();
        }
        catch (IOException e) {
        }
    }

    // See http://www.oreilly.com/catalog/javanp2/chapter/ch11.html
    public void run() {

        // for security checks
        String root = documentRootDirectory.getPath();

        while (true) {
            Socket connection;
            synchronized (pool) {
                while (pool.isEmpty()) {
                    try {
                        pool.wait();
                    }
                    catch (InterruptedException e) {
                    }
                }
                connection = (Socket) pool.remove(0);
            }

            try {
                String filename;
                String contentType;
                OutputStream raw =
                    new BufferedOutputStream(
                                             connection.getOutputStream());
                Writer out = new OutputStreamWriter(raw);
                Reader in =
                    new InputStreamReader(
                    new BufferedInputStream(connection.getInputStream()),
                    "ASCII");
                StringBuffer requestLine = new StringBuffer();
                int c;
                while (true) {
                    c = in.read();
                    if (c == '\r' || c == '\n') break;
                    requestLine.append((char) c);
                }

                String get = requestLine.toString();

                // log the request
                System.out.println(get);

                StringTokenizer st = new StringTokenizer(get);
                String method = st.nextToken();
                String version = "";
                if (method.equals("GET")) {
                    filename = st.nextToken();

                    URI uri = new URI(filename);
                    String resource = uri.getPath();
                    Servlet servlet = (Servlet) servletNamed.get(resource);

                    if (servlet != null) {
                        // servlet invokation
                        writeServlet(out, "HTTP/", servlet, uri);
                    }
                    else { // not a query
                        if (filename.endsWith("/")) filename += indexFileName;
                        contentType = guessContentTypeFromName(filename);
                        if (st.hasMoreTokens()) {
                            version = st.nextToken();
                        }

                        File theFile = new File(documentRootDirectory,
                                                filename.substring(1,filename.length()));
                        if (theFile.canRead()
                            // Don't let clients outside the document root
                            && theFile.getCanonicalPath().startsWith(root)) {
                            writeData(raw, version, 200, "OK", contentType, theFile);
                        }
                        else {
                            // can't find the file
                            writeError(out, version, 404, "File Not Found");
                        }
                    }
                }
                else {
                    // method does not equal "GET"
                    writeError(out, version, 501, "Not Implemented");
                }
            }
            catch (URISyntaxException e) {
                System.out.println("Une exception: " + e);
            }
            catch (IOException e) {
                System.out.println("Une exception: " + e);
            }
            finally {
                try {
                    connection.close();
                }
                catch (IOException e) {}
            }

        } // end while

    } // end run

    public static String guessContentTypeFromName(String name) {
        int pos = name.lastIndexOf('.');
        if (pos >= 0) {
            String suffix = name.substring(pos + 1).toLowerCase();
            String mime = (String) mimes.get(suffix);
            return (mime == null) ? MIME_TEXT_PLAIN : mime;
        }
        return MIME_TEXT_PLAIN;
    }

    private static final Hashtable/*<String,String>*/ mimes = new Hashtable();

    static {
        // See http://www.webmaster-toolkit.com/mime-types.shtml
        mimes.put("c"    , MIME_TEXT_PLAIN);
        mimes.put("cc"   , MIME_TEXT_PLAIN);
        mimes.put("cpp"  , MIME_TEXT_PLAIN);
        mimes.put("class", MIME_APPLICATION_OCTET_STREAM);
        mimes.put("gif"  , MIME_IMAGE_GIF);
        mimes.put("h"    , MIME_TEXT_PLAIN);
        mimes.put("htm"  , MIME_TEXT_HTML);
        mimes.put("html" , MIME_TEXT_HTML);
        mimes.put("lst"  , MIME_TEXT_PLAIN);
        mimes.put("java" , MIME_TEXT_PLAIN);
        mimes.put("jpe"  , MIME_IMAGE_JPEG);
        mimes.put("jpg"  , MIME_IMAGE_JPEG);
        mimes.put("jpeg" , MIME_IMAGE_JPEG);
        mimes.put("pdf"  , MIME_APPLICATION_PDF);
        mimes.put("pl"   , MIME_TEXT_PLAIN);
        mimes.put("ps"   , MIME_APPLICATION_POSTSCRIPT);
        mimes.put("png"  , MIME_IMAGE_PNG);
        mimes.put("scala", MIME_TEXT_PLAIN);
        mimes.put("text" , MIME_TEXT_PLAIN);
        mimes.put("txt"  , MIME_TEXT_PLAIN);
    }

} // end RequestProcessor
