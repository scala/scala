<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">

  <xsl:template match="a">
    <a><xsl:attribute name="href">
    <xsl:value-of select="@href"/>
  </xsl:attribute><xsl:value-of select="."/></a></xsl:template>
  
  <xsl:template match="em"><em><xsl:value-of select="."/></em></xsl:template>
  
  <xsl:template match="p"><p><xsl:apply-templates/></p></xsl:template>
  
  <xsl:template match="ul"><ul><xsl:apply-templates/></ul></xsl:template>
  
  <xsl:template match="li"><li><xsl:apply-templates/></li></xsl:template>

</xsl:stylesheet>