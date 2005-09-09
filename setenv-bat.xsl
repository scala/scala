<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">
  <xsl:output method="text" indent="no" />

  <xsl:template match="/">
@echo off
    <xsl:apply-templates />
  </xsl:template>

  <xsl:template match="property">
set nsc_<xsl:value-of select="substring-after(@name,'nsc.')" />=<xsl:value-of select="@value"/>
  </xsl:template>

</xsl:stylesheet>
