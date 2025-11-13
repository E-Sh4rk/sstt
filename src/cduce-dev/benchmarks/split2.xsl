<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template match="person">
  <xsl:variable name="gender">
    <xsl:choose>
      <xsl:when test="@gender = 'M'">man</xsl:when>
      <xsl:otherwise>women</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:element name="{$gender}">
    <xsl:attribute name="name">
      <xsl:value-of select="name/text()"/>
    </xsl:attribute>
    <sons>
      <xsl:apply-templates select="children/person[@gender='M']"/>
    </sons>
    <daughters>
      <xsl:apply-templates select="children/person[@gender='F']"/>
    </daughters>
  </xsl:element>
</xsl:template>

</xsl:stylesheet>
