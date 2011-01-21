/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package scala.tools.jline.internal;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * Provides access to configuration values.
 *
 * @author <a href="mailto:jason@planet57.com">Jason Dillon</a>
 * @since 2.4
 */
public final class Configuration
{
    public static final String JLINE_RC = ".jline.rc";

    private static final Properties userprops;

    static {
        Properties props = new Properties();

        File file = new File(getUserHome(), JLINE_RC);
        if (file.exists() && file.canRead()) {
            try {
                InputStream input = new BufferedInputStream(new FileInputStream(file));
                try {
                    props.load(input);
                    Log.debug("Loaded user configuration: ", file);
                }
                finally {
                    input.close();
                }
            }
            catch (IOException e) {
                Log.warn("Unable to read user configuration: ", file, e);
            }
        }
        else {
            Log.trace("User configuration file missing or unreadable: ", file);
        }

        userprops = props;
    }

    private static boolean isEmpty(final String value) {
        return value == null || value.trim().length() == 0;
    }

    public static String getString(final String name, final String defaultValue) {
        assert name != null;

        String value;

        // Check sysprops first, it always wins
        value = System.getProperty(name);

        if (isEmpty(value)) {
            // Next try userprops
            value = userprops.getProperty(name);

            if (isEmpty(value)) {
                // else use the default
                value = defaultValue;
            }
        }

        return value;
    }

    public static String getString(final String name) {
        return getString(name, null);
    }

    public static Boolean getBoolean(final String name, final Boolean defaultValue) {
        String value = getString(name);
        if (isEmpty(value)) {
            return defaultValue;
        }
        return Boolean.valueOf(value);
    }

    public static Boolean getBoolean(final String name) {
        return getBoolean(name, null);
    }

    //
    // System property helpers
    //

    public static File getUserHome() {
        return new File(System.getProperty("user.home"));
    }

    public static String getOsName() {
        return System.getProperty("os.name").toLowerCase();
    }

    public static String getFileEncoding() {
        return System.getProperty("file.encoding");
    }

    public static String getInputEncoding() {
        return System.getProperty("input.encoding", "UTF-8");
    }
}