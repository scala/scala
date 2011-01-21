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
import scala.tools.jline.console.completer.NullCompleter;
import org.junit.Test;

/**
 * Tests for {@link NullCompleter}.
 *
 * @author <a href="mailto:jason@planet57.com">Jason Dillon</a>
 */
public class NullCompleterTest
    extends ConsoleReaderTestSupport
{
    @Test
    public void test1() throws Exception {
        console.addCompleter(NullCompleter.INSTANCE);

        assertBuffer("f", new Buffer("f").tab());
        assertBuffer("ba", new Buffer("ba").tab());
        assertBuffer("baz", new Buffer("baz").tab());
    }
}