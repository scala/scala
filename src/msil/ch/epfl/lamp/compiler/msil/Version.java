/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

// $Id$

package ch.epfl.lamp.compiler.msil;


/**
 * Represents the version number for a common language runtime assembly
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public final class Version {

    //##########################################################################
    // public interface

    /**
     * Gets the value of the major component of the version
     * number for this instance.
     */
    public final int  Major;

    /**
     * Gets the value of the minor component of the version
     * number for this instance.
     */
    public final int Minor;

    /**
     * Gets the value of the build component of the version
     * number for this instance.
     */
    public final int Build;

    /**
     * Gets the value of the revision component of the version
     * number for this instance.
     */
    public final int Revision;

    /**
     * Initializes a new instance of the Version class.
     */
    public Version() {
	this(0,0,0,0);
    }

    /**
     * Initializes a new instance of the Version class with
     * the specified major, minor, build, and revision numbers.
     */
    public Version(int major, int minor, int build, int revision) {
	this.Major = major;
	this.Minor = minor;
	this.Build = build;
	this.Revision = revision;
    }

    /**
     * Converts the value of this instance to its equivalent String representation
     */
    public String toString() {
	return "" + Major + "." + Minor + "." + Build + "." +  Revision;
    }

    //##########################################################################

} // class Version
