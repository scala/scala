/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */


package ch.epfl.lamp.compiler.msil;

import javax.crypto.Mac;

import java.security.MessageDigest;

import ch.epfl.lamp.compiler.msil.util.Table;

/**
 * Fully describes an assembly's unique identity.
 * Right now it's only the name
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public class AssemblyName {

    //##########################################################################
    // public interface

    /** The simple, unencrypted name of the assembly. */
    public String Name;

    /**
     * Gets or sets the major, minor, revision, and build numbers
     * of the assembly.
     */
    public Version Version;

    /**
     * Gets a strong name consisting of a public key, a given name,
     * and version parts.
     */
    public byte[] GetPublicKeyToken() {
	return publicKeyToken == null ? null : (byte[]) publicKeyToken.clone();
    }

    /**
     * Sets a strong name consisting of a public key, a given name,
     * and version parts.
     */
    public void SetPublicKeyToken(byte[] key) {
	this.publicKeyToken = key.length == 0 ? null : (byte[]) key.clone();
    }

    /**
     * Returns the public key identifying the originator of the assembly.
     */
    public byte[] GetPublicKey() {
	return publicKey == null ? null : (byte[]) publicKey.clone();
    }

    /**
     * Sets the public key identifying the originator of the assembly.
     */
    public void SetPublicKey(byte[] key) {
	if (key.length > 0) {
	    this.publicKey = (byte[]) key.clone();
	    byte[] hash = sha.digest(key);
	    byte[] keyToken = new byte[8];
	    for (int i = 0; i < keyToken.length; i++)
		keyToken[i] = hash[hash.length - 1 - i];
	    this.publicKeyToken = keyToken;
	    //System.out.println("Pubic key and key token of assembly " + this + ":");
	    //System.out.println("\tPublic key = " + Table.bytes2hex(key));
	    //System.out.println("\tKey token  = " + Table.bytes2hex(keyToken));
	}
    }

    public String toString() {
	return Name + ", Version=" + Version;
    }

    //##########################################################################

    private byte[] publicKeyToken;

    private byte[] publicKey;

    private static final MessageDigest sha;
    static {
	MessageDigest md = null;
	try {
	    md = MessageDigest.getInstance("SHA");
	} catch (java.security.NoSuchAlgorithmException e) {}
	sha = md;
    }

    //##########################################################################

} // class AssemblyName
