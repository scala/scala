/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */

// $Id$

package ch.epfl.lamp.compiler.msil.emit

import ch.epfl.lamp.compiler.msil.Type

/**
 * Represents a label in the instruction stream. Label is used in conjunction
 * with the ILGenerator class.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
abstract class Label protected {
    import Label._
    def isInitialized(): Boolean
    def getKind(): Kind
    def getAddress(): Int
    def getStacksize(): Int
    def setStacksize(stacksize: Int): Unit
    def incStacksize(): Unit
    def mergeWith(that: Label): Unit
}

object Label {
    final val DUMMY: Int = -((1<<31)-1)

    //##########################################################################

    final class NormalLabel(_address: Int, _stacksize: Int) extends Label {

        //##########################################################################
        // protected constructors

        //the position of the label
        private var address: Int = _address

        //the stacksize at the label
        private var stacksize: Int = _stacksize

	def this() {
          this(-1, DUMMY)
        }

        def this(that: NormalLabel) {
          this(that.getAddress(), that.getStacksize())
        }

        //##########################################################################
        // instrumental methods only used by ILGenerator

        def isInitialized() = (getAddress() != -1) || (stacksize != DUMMY)

        def getAddress() = address

        def getStacksize() = stacksize

        def setStacksize(stacksize: Int) {
            assert(stacksize >= 0)
            this.stacksize = stacksize
        }

        def incStacksize() {
            stacksize = stacksize + 1
        }

        def getKind(): Kind = Kind.Normal

        def mergeWith(that: Label) {
            //assert address < 0 : "this.address = " + address + " that.address = " + that.address
            address = that.getAddress()

            // 	assert stacksize == that.stacksize
            // 	    : "this.stacksize = " + stacksize + " that.stacksize = "
            // 	    + that.stacksize
            // 	stacksize = that.stacksize
            val ss: Int = Math.max(stacksize, that.getStacksize())
            stacksize = ss
            that.setStacksize(ss)
        }

        //##########################################################################
        //

        /**
         * the toString Method return the label name
         * it's "IL" + address
         */
        override def toString(): String = {
            var pad: String = ""
            if (address < 16) pad = "000"
            else if (address < 256) pad = "00"
            else if (address < 4096) pad = "0"
            return "IL_"  + pad + Integer.toHexString(address)
        }

        def getString(): String = {
            val name = super.toString()
            val i: Int = name.lastIndexOf('.')
            return name.substring(i+1, name.length())
        }
    }

    //########################################################################
    // Special Labels

    final class SpecialLabel(_kind: Label.Kind) extends Label {
        private final var kind: Label.Kind = _kind
        def isInitialized() = true
        def getAddress(): Int = { throw new RuntimeException("" + kind.toString()) }
        def getStacksize(): Int = { throw new RuntimeException("" + kind.toString()) }
        def setStacksize(stacksize: Int) { throw new RuntimeException(kind.toString()) }
        def incStacksize() { throw new RuntimeException(kind.toString()) }
        def getKind(): Kind = kind
        def mergeWith(that: Label) { throw new RuntimeException(kind.toString()) }
        override def toString(): String = "Label(" + kind.toString() + ")"
    }

    final val NewScope: Label = new SpecialLabel(Kind.NewScope)
    final val EndScope: Label = new SpecialLabel(Kind.EndScope)
    final val Try: Label = new SpecialLabel(Kind.Try)
    final val Catch: Label = new SpecialLabel(Kind.Catch)
    final val Filter: Label = new SpecialLabel(Kind.Filter)
    final val EndFilter: Label = new SpecialLabel(Kind.EndFilter)
    final val Finally: Label = new SpecialLabel(Kind.Finally)
    final val EndTry: Label = new SpecialLabel(Kind.EndTry)

    final class Kind() {}

    final object Kind {
        final val Normal: Kind = new Kind()

	final val NewScope: Kind = new Kind()
	final val EndScope: Kind = new Kind()

        final val Try: Kind = new Kind()
        final val Catch: Kind = new Kind()
        final val Filter: Kind = new Kind()
        final val EndFilter: Kind = new Kind()
        final val Finally: Kind = new Kind()
        final val EndTry: Kind = new Kind()
    }

    //##########################################################################
}
