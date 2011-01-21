/*
 * Copyright (C) 2009 the original author(s).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * MODIFICATIONS: methods to deal with wrapping the output stream.
 */

package scala.tools.jline;

import org.fusesource.jansi.AnsiConsole;
import org.fusesource.jansi.AnsiOutputStream;
import org.fusesource.jansi.WindowsAnsiOutputStream;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;

/**
 * ANSI-supported {@link WindowsTerminal}.
 *
 * @since 2.0
 */
public class AnsiWindowsTerminal
    extends WindowsTerminal
{
    private final boolean ansiSupported = detectAnsiSupport();

    @Override
    public OutputStream wrapOutIfNeeded(OutputStream out) {
        return wrapOutputStream(out);
    }

    /**
     * Returns an ansi output stream handler. We return whatever was
     * passed if we determine we cannot handle ansi based on Kernel32 calls.
     *
     * @return an @{link AltWindowAnsiOutputStream} instance or the passed
     * stream.
     */
    private static OutputStream wrapOutputStream(final OutputStream stream) {
        String os = System.getProperty("os.name");
        if( os.startsWith("Windows") ) {
            // On windows we know the console does not interpret ANSI codes..
            try {
                return new WindowsAnsiOutputStream(stream);
            } catch (Throwable ignore) {
                // this happens when JNA is not in the path.. or
                // this happens when the stdout is being redirected to a file.
            }
            // Use the ANSIOutputStream to strip out the ANSI escape sequences.
            return new AnsiOutputStream(stream);
        }
        return stream;
    }

    private static boolean detectAnsiSupport() {
        OutputStream out = AnsiConsole.wrapOutputStream(new ByteArrayOutputStream());
        try {
            out.close();
        }
        catch (Exception e) {
            // ignore;
        }
        return out instanceof WindowsAnsiOutputStream;
    }

    public AnsiWindowsTerminal() throws Exception {
        super();
    }

    @Override
    public boolean isAnsiSupported() {
        return ansiSupported;
    }

    @Override
    public boolean hasWeirdWrap() {
        return false;
    }
}
