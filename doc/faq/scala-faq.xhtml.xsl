<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">
  <xsl:include href="xhtml.templates.xsl"/>
  <xsl:output method="html"/>

  <xsl:template match="/">
    <html>
      <body bgcolor="#FFFFFF">
        <xsl:apply-templates/>
      </body>
    </html>

  </xsl:template>

  <xsl:template match="faq">
    <table>
      <tr><th>Table of Contents</th></tr>
    <xsl:for-each select="./section">
      <tr>
        <td>
          <a>
            <xsl:attribute name="href">
              <xsl:value-of select="generate-id(.)"/>
            </xsl:attribute>
            <xsl:number/>. <xsl:value-of select="@title"/>
            
          </a>
        </td>
      </tr>
      <xsl:for-each select="./entry">
      <tr>
        <td>
          <a>
            <xsl:attribute name="href">
              <xsl:call-template name="entry.getId"/> 
            </xsl:attribute>
            <xsl:call-template name="entry.fullName"/>
          </a>
        </td>
      </tr>        
      </xsl:for-each>
    </xsl:for-each>
    </table>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="section">
    <p/><p/>
  <xsl:param name="id"/>
  <a>
    <xsl:attribute name="name">
      <xsl:value-of select="generate-id()"/>
    </xsl:attribute>
    <xsl:number/>. <xsl:value-of select="@title"/>
  </a>
  <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="entry">
    <p>
      <a>
        <xsl:attribute name="name">
          <xsl:call-template name="entry.getId"/> 
        </xsl:attribute>
        <xsl:call-template name="entry.fullName"/>
      </a>
      <br/>
      <xsl:apply-templates select="./answer"/>
    </p>
  </xsl:template>

  <xsl:template match="seealso">
    <a>
      <xsl:attribute name="href">
        <xsl:value-of select="concat('#',@ref)"/>
      </xsl:attribute>
      <xsl:for-each select="id(@ref)">
        <xsl:call-template name="entry.fullName"/>
      </xsl:for-each>
    </a>
  </xsl:template>

  <xsl:template name="entry.fullName">
    <xsl:number level="multiple" count="section|entry"/>.
    <xsl:value-of select="./question"/>
  </xsl:template>

  <xsl:template name="entry.getId">
    <xsl:choose>
      <xsl:when test="string(@id)">
        <xsl:value-of select="@id"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="generate-id()"/>        
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


</xsl:stylesheet>
