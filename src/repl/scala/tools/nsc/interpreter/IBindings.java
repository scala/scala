/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Raphael Jolly
 */

package scala.tools.nsc.interpreter;

import java.util.Map;
import java.util.AbstractMap;
import java.util.Set;
import java.util.AbstractSet;
import java.util.Iterator;
import java.util.NoSuchElementException;
import javax.script.Bindings;

abstract class IBindings extends AbstractMap<String, Object> implements Bindings {
    public Set<Map.Entry<String, Object>> entrySet() {
        return new AbstractSet<Map.Entry<String, Object>>() {
            public int size() {
                return 0;
            }

            public Iterator<Map.Entry<String, Object>> iterator() {
                return new Iterator<Map.Entry<String, Object>>() {
                    public boolean hasNext() {
                        return false;
                    }

                    public Map.Entry<String, Object> next() {
                        throw new NoSuchElementException();
                    }

                    public void remove() {
                        throw new UnsupportedOperationException();
                    }
                };
            }

            public boolean add(Map.Entry<String, Object> e) {
                IBindings.this.put(e.getKey(), e.getValue());
                return true;
            }
        };
    }
}
