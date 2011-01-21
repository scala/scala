/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */

package scala.tools.jline.internal;

import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.text.MessageFormat;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Provides access to terminal line settings via <tt>stty</tt>.
 *
 * @author <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 * @author <a href="mailto:dwkemp@gmail.com">Dale Kemp</a>
 * @author <a href="mailto:jason@planet57.com">Jason Dillon</a>
 * @author <a href="mailto:jbonofre@apache.org">Jean-Baptiste Onofré</a>
 * @since 2.0
 */
public final class TerminalLineSettings
{
    public static final String JLINE_STTY = "jline.stty";

    public static final String DEFAULT_STTY = "stty";

    public static final String JLINE_SH = "jline.sh";

    public static final String DEFAULT_SH = "sh";

    private static String sttyCommand = Configuration.getString(JLINE_STTY, DEFAULT_STTY);

    private static String shCommand = Configuration.getString(JLINE_SH, DEFAULT_SH);

    private String config;

    private long configLastFetched;

    public TerminalLineSettings() throws IOException, InterruptedException {
        config = get("-a");
        configLastFetched = System.currentTimeMillis();

        Log.debug("Config: ", config);

        // sanity check
        if (config.length() == 0) {
            throw new IOException(MessageFormat.format("Unrecognized stty code: {0}", config));
        }
    }

    public String getConfig() {
        return config;
    }

    public void restore() throws IOException, InterruptedException {
        set("sane");
    }

    public String get(final String args) throws IOException, InterruptedException {
        return stty(args);
    }

    public void set(final String args) throws IOException, InterruptedException {
        stty(args);
    }

    /**
     * <p>
     * Get the value of a stty property, including the management of a cache.
     * </p>
     *
     * @param name the stty property.
     * @return the stty property value.
     */
    public int getProperty(String name) {
        assert name != null;
        try {
            // tty properties are cached so we don't have to worry too much about getting term widht/height
            if (config == null || System.currentTimeMillis() - configLastFetched > 1000 ) {
                config = get("-a");
                configLastFetched = System.currentTimeMillis();
            }
            return this.getProperty(name, config);
        } catch (Exception e) {
            Log.warn("Failed to query stty ", name, e);
            return -1;
        }
    }

    /**
     * <p>
     * Parses a stty output (provided by stty -a) and return the value of a given property.
     * </p>
     *
     * @param name property name.
     * @param stty string resulting of stty -a execution.
     * @return value of the given property.
     */
    protected int getProperty(String name, String stty) {
        // try the first kind of regex
        Pattern pattern = Pattern.compile(name + "\\s+=\\s+([^;]*)[;\\n\\r]");
        Matcher matcher = pattern.matcher(stty);
        if (!matcher.find()) {
            // try a second kind of regex
            pattern = Pattern.compile(name + "\\s+([^;]*)[;\\n\\r]");
            matcher = pattern.matcher(stty);
            if (!matcher.find()) {
                // try a second try of regex
                pattern = Pattern.compile("(\\S*)\\s+" + name);
                matcher = pattern.matcher(stty);
                if (!matcher.find()) {
                    return -1;
                }
            }
        }
        return parseControlChar(matcher.group(1));
    }

    private int parseControlChar(String str) {
        // under
        if ("<undef>".equals(str)) {
            return -1;
        }
        // octal
        if (str.charAt(0) == '0') {
            return Integer.parseInt(str, 8);
        }
        // decimal
        if (str.charAt(0) >= '1' && str.charAt(0) <= '9') {
            return Integer.parseInt(str, 10);
        }
        // control char
        if (str.charAt(0) == '^') {
            if (str.charAt(1) == '?') {
                return 127;
            } else {
                return str.charAt(1) - 64;
            }
        } else if (str.charAt(0) == 'M' && str.charAt(1) == '-') {
            if (str.charAt(2) == '^') {
                if (str.charAt(3) == '?') {
                    return 127 + 128;
                } else {
                    return str.charAt(3) - 64 + 128;
                }
            } else {
                return str.charAt(2) + 128;
            }
        } else {
            return str.charAt(0);
        }
    }

    private static String stty(final String args) throws IOException, InterruptedException {
        assert args != null;
        return exec(String.format("%s %s < /dev/tty", sttyCommand, args));
    }

    private static String exec(final String cmd) throws IOException, InterruptedException {
        assert cmd != null;
        return exec(shCommand, "-c", cmd);
    }

    private static String exec(final String... cmd) throws IOException, InterruptedException {
        assert cmd != null;

        ByteArrayOutputStream bout = new ByteArrayOutputStream();

        Log.trace("Running: ", cmd);

        Process p = Runtime.getRuntime().exec(cmd);

        InputStream in = null;
        InputStream err = null;
        OutputStream out = null;
        try {
            int c;
            in = p.getInputStream();
            while ((c = in.read()) != -1) {
                bout.write(c);
            }
            err = p.getErrorStream();
            while ((c = err.read()) != -1) {
                bout.write(c);
            }
            out = p.getOutputStream();
            p.waitFor();
        }
        finally {
            close(in, out, err);
        }

        String result = bout.toString();

        Log.trace("Result: ", result);

        return result;
    }

    private static void close(final Closeable... closeables) {
        for (Closeable c : closeables) {
            try {
                c.close();
            }
            catch (Exception e) {
                // Ignore
            }
        }
    }
}