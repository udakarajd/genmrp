<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">

<h2> MRP Solution</h2>

<table border ="1">
<tr>
<td>Parts</td>
<td>Time Period</td>
<xsl:for-each select ="MRP/days/day">
	<td><xsl:value-of select="."/></td>
</xsl:for-each>
</tr>
<xsl:for-each select ="MRP/parts/part">
	<tr bgcolor="#EAEAFF">
		<th rowspan="2">
			<xsl:value-of select ="name"/> 
			<div>
			<font size ="1"> On hand = <xsl:value-of select ="onhand"/> </font>
			</div>
		</th>
		<td>Gross Requirement</td>
		<xsl:for-each select="orders/day">
			<td>
				<xsl:value-of select="."></xsl:value-of>
			</td>	
		</xsl:for-each>
		
	</tr>
	<tr>
		<td>
		  Planned order release
		</td>
		<xsl:for-each select="por/day">
			<td>
				<b><font color="#660066"><xsl:value-of select="."></xsl:value-of></font></b>
			</td>	
		</xsl:for-each>
	
	</tr>
</xsl:for-each>
</table>


</xsl:template>
</xsl:stylesheet>
