/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc.backend.bcode;

import scala.tools.asm.tree.MethodNode;
import scala.tools.asm.tree.AbstractInsnNode;
import scala.tools.asm.tree.LabelNode;

import scala.tools.asm.tree.analysis.Analyzer;
import scala.tools.asm.tree.analysis.AnalyzerException;
import scala.tools.asm.tree.analysis.Frame;
import scala.tools.asm.tree.analysis.BasicInterpreter;
import scala.tools.asm.tree.analysis.BasicValue;

/**
 * Detects and removes unreachable code.
 *
 * Should be used last in a transformation chain, before stack map frames are computed.
 * The Java 6 verifier demands frames be available even for dead code.
 * Those frames are tricky to compute, http://asm.ow2.org/doc/developer-guide.html#deadcode
 * The problem is avoided altogether by not emitting unreachable code in the first place.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
public class UnreachableCode {

    /** after transform() has run, this field records whether
     *  at least one pass of this transformer modified something. */
    public boolean changed = false;

    public void transform(final String owner, final MethodNode mnode) throws AnalyzerException {

        changed = false;

        Analyzer<BasicValue> a = new Analyzer<BasicValue>(new BasicInterpreter());
        a.analyze(owner, mnode);

        Frame<BasicValue>[] frames = a.getFrames();
        AbstractInsnNode[]  insns  = mnode.instructions.toArray();

        int i = 0;
        while (i < insns.length) {
          if (frames[i] == null &&
              insns[i]  != null &&
              !(insns[i] instanceof LabelNode)) {
            mnode.instructions.remove(insns[i]);
            changed = true;
          }
          i += 1;
        }

    }

}

