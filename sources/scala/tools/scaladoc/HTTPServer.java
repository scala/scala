/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
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
            } catch(IOException e) {}
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

    private Map/*[String,Servlet]*/ servletNamed;

    public HTTPServer(File documentRootDirectory,
                      int port,
                      String indexFileName) throws IOException {

        if (!documentRootDirectory.isDirectory()) {
            throw new IOException(documentRootDirectory
                                  + " does not exist as a directory");
        }
        this.documentRootDirectory = documentRootDirectory;
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
        for(int i = 0; i < servlets.length; i++)
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
        Map map = new HashMap();
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

    public static void fileNotFound(String version, Writer out) {
        try {
            if (version.startsWith("HTTP/")) {  // send a MIME header
                out.write("HTTP/1.0 404 File Not Found\r\n");
                Date now = new Date();
                out.write("Date: " + now + "\r\n");
                out.write("Server: HTTPServer 1.0\r\n");
                out.write("Content-type: text/html\r\n\r\n");
            }
            out.write("<HTML>\r\n");
            out.write("<HEAD><TITLE>File Not Found</TITLE>\r\n");
            out.write("</HEAD>\r\n");
            out.write("<BODY>");
            out.write("<H1>HTTP Error 404: File Not Found</H1>\r\n");
            out.write("</BODY></HTML>\r\n");
            out.flush();
        }
        catch(IOException e) {

        }
    }

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
                    if (servlet != null) { // servlet invokation
                        String query = uri.getRawQuery();
                        Map map =
                            (query != null) ?
                            parseQuery(query) :
                            new HashMap();
                            servlet.apply(map, out);
                            out.write("\n");
                            out.flush();
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
                            DataInputStream fis =
                                new DataInputStream(
                                                    new BufferedInputStream(
                                                                            new FileInputStream(theFile)));
                            byte[] theData = new byte[(int) theFile.length()];
                            fis.readFully(theData);
                            fis.close();
                            if (version.startsWith("HTTP/")) {  // send a MIME header
                                out.write("HTTP/1.0 200 OK\r\n");
                                Date now = new Date();
                                out.write("Date: " + now + "\r\n");
                                out.write("Server: HTTPServer 1.0\r\n");
                                out.write("Content-length: " + theData.length + "\r\n");
                                out.write("Content-type: " + contentType + "\r\n\r\n");
                                out.flush();
                            }  // end try

                            // send the file; it may be an image or other binary data
                            // so use the underlying output stream
                            // instead of the writer
                            raw.write(theData);
                            raw.flush();
                        }  // end if
                        else {  // can't find the file
                            fileNotFound(version, out);
                        }
                    }
                }
                else {  // method does not equal "GET"
                    if (version.startsWith("HTTP/")) {  // send a MIME header
                        out.write("HTTP/1.0 501 Not Implemented\r\n");
                        Date now = new Date();
                        out.write("Date: " + now + "\r\n");
                        out.write("Server: HTTPServer 1.0\r\n");
                        out.write("Content-type: text/html\r\n\r\n");
                    }
                    out.write("<HTML>\r\n");
                    out.write("<HEAD><TITLE>Not Implemented</TITLE>\r\n");
                    out.write("</HEAD>\r\n");
                    out.write("<BODY>");
                    out.write("<H1>HTTP Error 501: Not Implemented</H1>\r\n");
                    out.write("</BODY></HTML>\r\n");
                    out.flush();
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
        if (name.endsWith(".html") || name.endsWith(".htm")) {
            return "text/html";
        }
        else if (name.endsWith(".txt") ||
                 name.endsWith(".java") ||
                 name.endsWith(".scala")) {
            return "text/plain";
        }
        else if (name.endsWith(".gif")) {
            return "image/gif";
        }
        else if (name.endsWith(".class")) {
            return "application/octet-stream";
        }
        else if (name.endsWith(".jpg") || name.endsWith(".jpeg")) {
            return "image/jpeg";
        }
        else if (name.endsWith(".png")) {
            return "image/png";
        }
        else return "text/plain";
    }

} // end RequestProcessor
