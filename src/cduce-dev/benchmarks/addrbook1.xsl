<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:template match="/">
    <doc>
      <xsl:apply-templates/>
    </doc>
  </xsl:template>

  <xsl:template match="person">
  </xsl:template>


  <xsl:template match="person[tel]">
    <entry>
      <name>
        <xsl:value-of select="name"/>
      </name>
      <tel>
        <xsl:value-of select="tel"/>
      </tel>
    </entry>
  </xsl:template>

</xsl:stylesheet>
