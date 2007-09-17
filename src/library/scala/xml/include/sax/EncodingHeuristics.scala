package scala.xml.include.sax;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.InputStream;

/**
 * <p>
 * <code>EncodingHeuristics</code> reads from a stream
 * (which should be buffered) and attempts to guess
 * what the encoding of the text in the stream is.
 * Byte order marks are stripped from the stream.
 * If it fails to determine the type of the encoding,
 * it returns the default UTF-8.
 * </p>
 *
 * translated from Elliotte Rusty Harold's Java source
 * @author Burak Emir
 */
object EncodingHeuristics {

  /**
    * <p>
    * This utility method ????.
    * </p>
    *
    * @param in   <code>InputStream</code> to read from.
    * @return String  The name of the encoding.
    * @throws IOException if the stream cannot be reset back to where it was when
    *                     the method was invoked.
    */
  def readEncodingFromStream(in: InputStream): String = {
    //System.err.println("EncodingHeuristics::readEncodingFromStream");
    // This may fail if there are a lot of space characters before the end
    // of the encoding declaration
    in.mark(1024);
    var ret: String = null;
    try {
      // lots of things can go wrong here. If any do, I just return null
      // so that we'll fall back on the encoding declaration or the
      // UTF-8 default
      val byte1 = in.read();
      val byte2 = in.read();
      if (byte1 == 0xFE && byte2 == 0xFF) {
        // don't reset because the byte order mark should not be included????
        ret =  "UnicodeBig"; // name for big-endian????
      }
      else if (byte1 == 0xFF && byte2 == 0xFE) {
        // don't reset because the byte order mark should not be included????
        // will the reader throw away the byte order mark or will it return it????
        ret =  "UnicodeLittle";
      }

      /* In accordance with the Character Model [Character Model],
       when the text format is a Unicode encoding, the XInclude
       processor must fail the inclusion when the text in the
       selected range is non-normalized. When transcoding characters
       to a Unicode encoding from a legacy encoding, a normalizing transcoder must be used. */

      val byte3 = in.read();
      // check for UTF-8 byte order mark
      if (byte1 == 0xEF && byte2 == 0xBB && byte3 == 0xBF) {
        // don't reset because the byte order mark should not be included????
        // in general what happens if text document includes non-XML legal chars????
        ret =  "UTF-8";
      }

      val byte4 = in.read();
      if (byte1 == 0x00 && byte2 == 0x00 && byte3 == 0xFE && byte4 == 0xFF) {
        // don't reset because the byte order mark should not be included????
        ret =  "UCS-4"; // right name for big-endian UCS-4 in Java 1.4????
      }
      else if (byte1 == 0x00 && byte2 == 0x00 && byte3 == 0xFF && byte4 == 0xFE) {
        // don't reset because the byte order mark should not be included????
        ret =  "UCS-4"; // right name for little-endian UCS-4 in Java 1.4????
      }

      // no byte order mark present; first character must be
      // less than sign or white space
      // Let's look for less-than signs first
      if (byte1 == 0x00 && byte2 == 0x00 && byte3 == 0x00 && byte4 == '<') {
        in.reset();
        ret =  "UCS-4"; // right name for big-endian UCS-4 in Java 1.4????
      }
      else if (byte1 == '<' && byte2 == 0x00 && byte3 == 0x00 && byte4 == 0x00) {
        in.reset();
        ret =  "UCS-4"; // right name for little-endian UCS-4 in Java 1.4????
      }
      else if (byte1 == 0x00 && byte2 == '<' && byte3 == 0x00 && byte4 == '?') {
        in.reset();
        ret =  "UnicodeBigUnmarked";
      }
      else if (byte1 == '<' && byte2 == 0x00 && byte3 == '?' && byte4 == 0x00) {
        in.reset();
        ret =  "UnicodeLittleUnmarked";
      }
      else if (byte1 == '<' && byte2 == '?' && byte3 == 'x' && byte4 == 'm') {
        // ASCII compatible, must read encoding declaration
        // 1024 bytes will be far enough to read most XML declarations
        val data = new Array[byte](1024);
        data(0) = byte1.asInstanceOf[byte];
        data(1) = byte2.asInstanceOf[byte];;
        data(2) = byte3.asInstanceOf[byte];;
        data(3) = byte4.asInstanceOf[byte];;
        val length = in.read(data, 4, 1020) + 4;
        // Use Latin-1 (ISO-8859-1) because it's ASCII compatible and
        // all byte sequences are legal Latin-1 sequences so I don't have
        // to worry about encoding errors if I slip past the
        // end of the XML/text declaration
        val declaration = new String(data, 0, length, "8859_1");
        // if any of these throw a StringIndexOutOfBoundsException
        // we just fall into the catch bloclk and return null
        // since this can't be well-formed XML
        var position = declaration.indexOf("encoding") + 8;
        var c: char = '\0'; // bogus init value
        // get rid of white space before equals sign
        do {
          c = declaration.charAt(position);
          position = position + 1;
        } while (c == ' ' || c == '\t' || c == '\r' || c == '\n') ;
        if (c != '=') { // malformed
          in.reset();
          ret =  "UTF-8";
        }
        // get rid of white space after equals sign
        do {
          c = declaration.charAt(position);
          position = position + 1;
        } while (c == ' ' || c == '\t' || c == '\r' || c == '\n') ;
        var delimiter: char  = c;
        if (delimiter != '\'' && delimiter != '"') { // malformed
          in.reset();
          ret =  "UTF-8";
        }
        // now positioned to read encoding name
        val encodingName = new StringBuffer();
        do {
          c = declaration.charAt(position);
          position = position + 1;
          encodingName.append(c);
        } while(c != delimiter);
        encodingName.setLength(encodingName.length() - 1); // rm delim
        in.reset();
        ret =  encodingName.toString();

      }
        else if (byte1 == 0x4C && byte2 == 0x6F && byte3 == 0xA7 && byte4 == 0x94) {
          // EBCDIC compatible, must read encoding declaration
          // ????
        }

    } catch {
      case e: Exception => in.reset();
      ret =  "UTF-8";
    }

    // no XML or text declaration present
    //System.err.println("exit EncodingHeuristics::readEncodingFromStream");

    if (ret != null)
      return ret
    else {
      in.reset();
      return "UTF-8";
    }
  }
}
