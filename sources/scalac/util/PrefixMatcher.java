/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.util;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;

public class PrefixMatcher {

    public static class Entry {

        private Entry prev;
        private Entry next;

        public final String key;
        public final Object value;
        public final String argument;
        public final String description;

        public Entry(String key, Object value, String argument,
            String description)
        {
            this.key = key;
            this.value = value;
            this.argument = argument;
            this.description = description;
        }
    }

    private final Map entries;
    private Entry first;
    private Entry last;

    public PrefixMatcher() {
        this.entries = new HashMap();
    }

    public void insert(String key, Object value) {
        insert(key, value, null, null);
    }

    public void insert(String key, Object value, String description) {
        insert(key, value, null, description);
    }

    public void insert(String key, Object value, String argument,
        String description)
    {
        assert key != null && !entries.containsKey(key) : key;
        Entry entry = new Entry(key, value, argument, description);
        if (first == null) {
            first = last = entry;
        } else {
            last.next = entry;
            entry.prev = last;
            last = entry;
        }
        entries.put(key, entry);
    }

    public Entry[] lookup(String key) {
        Object value = entries.get(key);
        if (value != null) return new Entry[] { (Entry)value };
        List list = new ArrayList();
        for (Entry i = first; i != null; i = i.next) {
            if (i.key.startsWith(key)) list.add(i);
        }
        return (Entry[])list.toArray(new Entry[list.size()]);
    }

    public String getErrorMessage(String key, Entry[] entries, String what) {
        switch (entries.length) {
        case 0:
            return "unknown " + what + " '" + key + "'";
        case 1:
            return null;
        case 2:
            return "ambigous " + what + " '" + key + "', both '" +
                entries[0].key + "' and '" + entries[1].key  + "' match";
        default:
            StringBuffer buffer = new StringBuffer();
            buffer.append("ambigous ").append(what);
            buffer.append(" '").append(key).append("'");
            for (int i = 0; i < entries.length; i++) {
                buffer.append(i < entries.length - 1 ? ", " : " and ");
                buffer.append('\'').append(entries[i].key).append('\'');
            }
            buffer.append(" match");
            return buffer.toString();
        }
    }

    public List getHelpStrings(String separator1, String separator2) {
        List strings = new ArrayList();
        for (Entry entry = first; entry != null; entry = entry.next) {
            if (entry.description != null)
                if (entry.argument != null)
                    strings.add(entry.key + separator1 + entry.argument +
                        separator2 + entry.description);
                else
                    strings.add(entry.key + separator2 + entry.description);
        }
        return strings;
    }

}
