<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template match="person[@gender = 'M']">
  <man name="{name/text()}">
    <sons>
      <xsl:apply-templates select="children/person[@gender='M']"/>
    </sons>
    <daughters>
      <xsl:apply-templates select="children/person[@gender='F']"/>
    </daughters>
  </man>
</xsl:template>

<xsl:template match="person[@gender = 'F']">
  <woman name="{name/text()}">
    <sons>
      <xsl:apply-templates select="children/person[@gender='M']"/>
    </sons>
    <daughters>
      <xsl:apply-templates select="children/person[@gender='F']"/>
    </daughters>
  </woman>
</xsl:template>

</xsl:stylesheet>
