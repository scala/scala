<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="xml" version="1.0" encoding="UTF-8"
    doctype-public="-//W3C//DTD XHTML 1.1//EN"
    doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"
    indent="yes"/>

  <!-- ##################### Match Rules ####################### -->

  <xsl:template match="/">
    <xsl:apply-templates/>
  </xsl:template>


  <!-- ELEMENT object -->

  <xsl:template match="object">
    <xsl:text disable-output-escaping="yes"><![CDATA[<object]]></xsl:text>
    <xsl:for-each select="@*">
      <xsl:text disable-output-escaping="yes"><![CDATA[ ]]></xsl:text>
      <xsl:value-of select="name()"/>
      <xsl:text disable-output-escaping="yes"><![CDATA[="]]></xsl:text><xsl:value-of select="."/><xsl:text disable-output-escaping="yes"><![CDATA["]]></xsl:text>
    </xsl:for-each>
    <xsl:choose>
    <xsl:when test=".!=''">
      <xsl:text disable-output-escaping="yes"><![CDATA[>]]></xsl:text>
      <xsl:apply-templates/>
      <xsl:text disable-output-escaping="yes"><![CDATA[</object>]]></xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text disable-output-escaping="yes"><![CDATA[/>]]></xsl:text>
    </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <!-- ELEMENT property -->

  <xsl:template match="property">
    <xsl:choose>
    <xsl:when test="@name='productVersionMajor'">
      <xsl:variable name="major" select="substring-before($version,'.')" />
      <property name="productVersionMajor">
        <int><xsl:value-of select="$major"/></int>
      </property>
    </xsl:when>
    <xsl:when test="@name='productVersionMinor'">
      <xsl:variable name="minor"
        select="substring-before(substring-after($version,'.'),'.')" />
      <property name="productVersionMinor">
        <int><xsl:value-of select="$minor"/></int>
      </property>
    </xsl:when>
    <xsl:when test="@name='productVersionRevision'">
      <xsl:variable name="revision"
        select="substring-before(substring-after(substring-after($version,'.'),'.'),'.')" />
      <property name="productVersionRevision">
        <int><xsl:value-of select="$revision"/></int>
      </property>
    </xsl:when>
    <xsl:when test="@name='productVersionSubRevision'">
      <xsl:variable name="subrevision"
        select="substring-after(substring-after(substring-after($version,'.'),'.'),'.')" />
      <property name="productVersionSubRevision">
        <int><xsl:value-of select="$subrevision"/></int>
      </property>
    </xsl:when>
    <xsl:otherwise>
      <xsl:copy>
        <xsl:copy-of select="@*"/>
        <xsl:apply-templates/>
      </xsl:copy>
    </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <!-- ELEMENT restorationObjects -->

  <xsl:template match="restorationObjects">
    <xsl:text disable-output-escaping="yes"><![CDATA[<restorationObjects]]></xsl:text>
    <xsl:for-each select="@*">
      <xsl:text disable-output-escaping="yes"><![CDATA[ ]]></xsl:text>
      <xsl:value-of select="name()"/>
      <xsl:text disable-output-escaping="yes"><![CDATA[="]]></xsl:text><xsl:value-of select="."/><xsl:text disable-output-escaping="yes"><![CDATA["]]></xsl:text>
    </xsl:for-each>
    <xsl:choose>
    <xsl:when test=".!=''">
      <xsl:text disable-output-escaping="yes"><![CDATA[>]]></xsl:text>
      <xsl:apply-templates/>
      <xsl:text disable-output-escaping="yes"><![CDATA[</restorationObjects>]]></xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text disable-output-escaping="yes"><![CDATA[/>]]></xsl:text>
    </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <!-- ELEMENT string -->

  <xsl:template match="string">
    <string><xsl:text disable-output-escaping="yes"><![CDATA[<]]></xsl:text>![CDATA[<xsl:value-of select="." disable-output-escaping="yes"/>]]<xsl:text disable-output-escaping="yes"><![CDATA[>]]></xsl:text></string>
  </xsl:template>


  <!-- ELEMENT versionID -->

  <xsl:template match="versionID">
    <xsl:text disable-output-escaping="yes"><![CDATA[<versionID]]></xsl:text>
    <xsl:for-each select="@*">
      <xsl:text disable-output-escaping="yes"><![CDATA[ ]]></xsl:text>
      <xsl:value-of select="name()"/>
      <xsl:text disable-output-escaping="yes"><![CDATA[="]]></xsl:text><xsl:value-of select="."/><xsl:text disable-output-escaping="yes"><![CDATA["]]></xsl:text>
    </xsl:for-each>
    <xsl:choose>
    <xsl:when test=".!=''">
      <xsl:text disable-output-escaping="yes"><![CDATA[>]]></xsl:text>
      <xsl:apply-templates/>
      <xsl:text disable-output-escaping="yes"><![CDATA[</versionID>]]></xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text disable-output-escaping="yes"><![CDATA[/>]]></xsl:text>
    </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <!-- otherwise -->

  <xsl:template match="*">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>
