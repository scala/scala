<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">
  <xsl:output method="text" encoding="iso-8859-1" indent="no" />

  <!-- ##################### Match Rules ####################### -->

  <xsl:template match="/"><xsl:apply-templates /></xsl:template>


  <!-- ELEMENT source -->

  <xsl:template match="source">
<xsl:apply-templates />
\newpage
  </xsl:template>


  <!-- ELEMENT header -->

  <xsl:template match="header">
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Author: <xsl:value-of select="author" />
%% Keywords: <xsl:value-of select="keywords" />
  </xsl:template>


  <!-- ELEMENT content -->

  <xsl:template match="content">
<xsl:apply-templates />
  </xsl:template>


  <!-- ELEMENT csharp, dotnet, scala -->

  <xsl:template match="csharp">\CSharp</xsl:template>
  <xsl:template match="dotnet">\DotNet</xsl:template>
  <xsl:template match="scala">\Scala</xsl:template>


  <!-- ELEMENT title, h, h3 -->

  <xsl:template match="title">
    <xsl:variable name="_subtitle"><xsl:apply-templates /></xsl:variable>
    <xsl:variable name="subtitle"><xsl:value-of select="substring-after($_subtitle, ': ')" /></xsl:variable>
\section{<xsl:value-of select="$subtitle" />}
  </xsl:template>
  <xsl:template match="h">\subsection*{<xsl:apply-templates />}</xsl:template>
  <xsl:template match="h3">\subsubsection*{<xsl:apply-templates />}</xsl:template>

  <!-- ELEMENT a, code, em, i, p, tt -->

  <xsl:template match="a">
    <xsl:choose>
      <xsl:when test="starts-with(@href, 'http://')">
\href{<xsl:value-of select="@href" />}{<xsl:apply-templates />}
      </xsl:when>
<!--
      <xsl:when test="starts-with(@href, 'intro/')">
\hypertarget{<xsl:value-of select="translate(substring-before(@href, '.html'), '/', ':')" />}{<xsl:apply-templates />}
      </xsl:when>
      <xsl:when test="not(@name)">
<xsl:apply-templates /> [\nolinkurl{<xsl:value-of select="@href" />}]
      </xsl:when>
-->
      <xsl:otherwise>
        <xsl:apply-templates />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="code">\texttt{<xsl:apply-templates />}</xsl:template>
  <xsl:template match="em">\emph{<xsl:apply-templates />}</xsl:template>
  <xsl:template match="i">\emph{<xsl:apply-templates />}</xsl:template>
  <xsl:template match="p">
\noindent <xsl:apply-templates />
  </xsl:template>
  <xsl:template match="tt">\texttt{<xsl:apply-templates />}</xsl:template>
  

  <!-- ELEMENT img -->

  <xsl:template match="img">
    <xsl:variable name="_src">
      <xsl:value-of select="substring-before(substring-after(@src, '/'), '.')" />
    </xsl:variable>
\begin{figure}[ht] <!-- h:here, t:top, b:bottom, p:separate page -->
\begin{center}
    <xsl:choose>
      <xsl:when test="contains($_src,'classhierarchy')">
\scalebox{0.75}[0.75]{\includegraphics{<xsl:value-of select="$_src" />}}
      </xsl:when>
      <xsl:otherwise>
\includegraphics{<xsl:value-of select="$_src" />}
      </xsl:otherwise>
    </xsl:choose>
\end{center}
\end{figure}
  </xsl:template>


  <!-- ELEMENT ul, li -->

  <xsl:template match="ul">\begin{list}{-}{\setlength{\topsep}{0.0em}\setlength{\itemsep}{0.0em}}
    <xsl:apply-templates />
\end{list}</xsl:template>
  <xsl:template match="li">\item<xsl:apply-templates /></xsl:template>


  <!-- ELEMENT pre -->

  <xsl:template match="pre">
\begin{small}
\begin{alltt}<xsl:apply-templates />\end{alltt}
\end{small}
  </xsl:template>


  <!-- ELEMENT src (Scala source code) -->

  <xsl:template match="src">
    <xsl:if test="not(@action) or @action!='hide'">
\begin{small}
\begin{alltt}<xsl:apply-templates />\end{alltt}
\end{small}
    </xsl:if>
  </xsl:template>


  <!-- ELEMENT key (Scala keyword) -->

  <xsl:template match="key">\textbf{<xsl:apply-templates />}</xsl:template>


  <!-- ELEMENT chr (character literal) -->

  <xsl:template match="chr"><xsl:apply-templates /></xsl:template>


  <!-- ELEMENT str (string literal) -->

  <xsl:template match="str"><xsl:apply-templates /></xsl:template>


  <!-- ELEMENT cmt (comment) -->

  <xsl:template match="cmt"><xsl:apply-templates /></xsl:template>


  <!-- ELEMENT div, hidden -->

  <xsl:template match="div" />
  <xsl:template match="hidden"></xsl:template>


  <!-- otherwise -->

  <xsl:template match="*">
     <xsl:copy>
       <xsl:copy-of select="@*"/>
       <xsl:apply-templates/>       
     </xsl:copy>
  </xsl:template>


</xsl:stylesheet>
