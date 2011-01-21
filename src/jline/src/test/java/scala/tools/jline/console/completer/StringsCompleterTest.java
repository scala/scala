/*
 * Copyright (C) 2010 the original author or authors.
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
 */
package scala.tools.jline.console.completer;

import scala.tools.jline.console.ConsoleReaderTestSupport;
import scala.tools.jline.console.completer.StringsCompleter;
import org.junit.Test;

/**
 * Tests for {@link jline.console.completer.StringsCompleter}.
 *
 * @author <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 */
public class StringsCompleterTest
    extends ConsoleReaderTestSupport
{
    @Test
    public void test1() throws Exception {
        console.addCompleter(new StringsCompleter("foo", "bar", "baz"));

        assertBuffer("foo ", new Buffer("f").tab());
        // single tab completes to unambiguous "ba"
        assertBuffer("ba", new Buffer("b").tab());
        assertBuffer("ba", new Buffer("ba").tab());
        assertBuffer("baz ", new Buffer("baz").tab());
    }
}