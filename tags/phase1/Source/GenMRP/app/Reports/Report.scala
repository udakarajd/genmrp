package Reports

import mrp.Soln
import models._
import java.io._
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.StringReader
import javax.xml.parsers._
import javax.xml.transform.Source
import javax.xml.transform.Transformer
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamSource
import org.w3c.dom.Document
import org.xml.sax.InputSource;
import play.Application
import play.api.Play.current

import java.io._
import com.lowagie.text.DocumentException
import org.xhtmlrenderer.pdf.ITextRenderer


case class Report(var body:String)
object Report {
  
  def reportSoln(soln:Soln):Report ={
     var rpt = Report("")
     var startday = soln.orderlist.minBy(_.days)
     if(soln.portlist.length>0){
       startday = soln.portlist.minBy(_.days)  
      }
      
      var endday = soln.orderlist.maxBy(_.days)
       rpt.body +="<MRP>"
       rpt.body +="<days>"  
       for(i<-startday.days until endday.days+1){
        rpt.body +="<day>"+i+"</day>"
       }  
       rpt.body +="</days>"  
       rpt.body +="<parts>"  
      
      soln.partsarray.foreach( part => {
          
          var tempsol: Soln = soln.copy()
           if(sol_of_part(part.partid,tempsol)){
                 rpt.body += reportSolnPart(part,tempsol, startday.days , endday.days)      
           }
           
                
         }
      )
       rpt.body +="</parts>"
       rpt.body +="</MRP>"
       rpt.body = transformReport(rpt.body)
    rpt
  }
  
  private def reportSolnPart(part:Part,soln:Soln, startdays:Int , enddays:Int):String ={
    var rpt ="<part>"
      
      rpt +="<name>"+part.partname+"</name>"
      rpt +="<onhand>"+part.onhand+"</onhand>"
      
      rpt +="<orders>"
      for(i <- startdays until enddays+1){
         rpt += "<day count=\""+i+"\">"
         if(soln.orderlist.exists(e => e.days == i)){
           var sum = 0
           //soln.orderlist.find(e => e.days == i).foreach(ord=> sum += ord.quantity)
           soln.orderlist.filter(e => e.days == i).foreach(ord=> sum += ord.quantity)
           rpt += ""+sum //soln.orderlist.find(e => e.days == i).head.quantity
           
         }
         
        rpt += "</day>"
        
      }
      rpt +="</orders>"
      rpt +="<por>"  
        for(i <- startdays until enddays+1){
         rpt += "<day count=\""+i+"\">"
         if(soln.portlist.exists(e => e.days == i)){
           var sum = 0
           //soln.portlist.find(e => e.days == i).foreach(ord=> sum += ord.quantity)
           soln.portlist.filter(e => e.days == i).foreach(ord=> sum += ord.quantity)
           rpt += ""+ sum //soln.portlist.find(e => e.days == i).head.quantity
         }
         
        rpt += "</day>"
        
      }
      
        
      rpt +="</por>"
    rpt +="</part>"
    
   
    rpt
  }
  
   private def sol_of_part(part_id : String , soln: Soln): Boolean = {
             
    soln.orderlist = soln.orderlist.filterNot(e => !e.partid.equals(part_id))
    if(!(soln.orderlist.length > 0) ){
       return false
    }
    soln.portlist = soln.portlist.filterNot(e => !e.partid.equals(part_id))
    
    true
  }
   

  
   private def transformReport(xmldata : String):String ={
     var reporthtm=""
    
     try{
		  
		  var factory = DocumentBuilderFactory.newInstance();
	      var builder = factory.newDocumentBuilder();
	      var bos= new  ByteArrayOutputStream();
	      var tFactory = TransformerFactory.newInstance();
	      
	      var is = new InputSource( new StringReader( xmldata ) );
	      
	      var doc = builder.parse( is );
       
	      
	      var layoutpath = current.path.getAbsolutePath.toString()+"/public/stylesheets/mrprpt.xsl"
	      
	      var xslfile = new File(layoutpath);
	      if(xslfile.exists()){
	      
		  var xslsource = new StreamSource(xslfile);		  
		  var transformer = tFactory.newTransformer(xslsource);
		  transformer.transform(  new DOMSource(doc), new javax.xml.transform.stream.StreamResult(bos));
		  reporthtm = bos.toString();
	      }else{
	        reporthtm = "Layout file not found" 
	      }
       
     }   
     reporthtm
   
   }


   def saveReportHtml(report:Report ,outputFile:String) ={
     var out_file = new FileOutputStream(outputFile)
     var out_stream = new PrintStream(out_file)
     out_stream.print("<html><body>")
     out_stream.print(report.body)
     out_stream.print("</body></html>")
     out_stream.close()
   }
   
   def saveReportpdf(report:Report , outputFile:String)={
     var buf = new StringBuffer()
     buf.append(report.body)
     var builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
     var doc = builder.parse(new StringBufferInputStream(buf.toString()));
     var renderer = new ITextRenderer();   
     renderer.setDocument(doc, null);
     var os = new FileOutputStream(outputFile);
     renderer.layout();
     renderer.createPDF(os);
        
     os.close();
     
   }
}