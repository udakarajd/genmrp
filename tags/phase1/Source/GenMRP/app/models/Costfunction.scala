package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._

case class Costfunction(compid : String, partid : String , funcid : String,functxt:String)

object Costfunction{
  
  def all(compid : String, partid : String): List[Costfunction]= {
		 DB.withConnection(implicit c => 
		   SQL("select * from costfunction where compid={compid} and partid={partid}").on(
		       'compid -> compid,
		       'partid -> partid
		   ).as(costfunction *)
		 )
  }
  def all_company_parts(compid : String): List[Costfunction] = {
    DB.withConnection(implicit c => 
		   SQL("select * from costfunction where compid={compid}").on(
		       'compid -> compid
		   ).as(costfunction *)
		 )     
  }
  
  def deletefucn(costfunc : Costfunction) ={
    DB.withConnection(implicit c =>
      SQL("delete from costfunction where compid={compid} and partid={partid} and funcid={funcid}").on(
          'compid -> costfunc.compid,
          'partid -> costfunc.partid,
          'funcid -> costfunc.funcid
      ).executeUpdate()      
    )
  }
  def addfunc(costfunc : Costfunction)={
    DB.withConnection(implicit c =>
      SQL("insert into costfunction (compid,partid,funcid,functxt) values ({compid},{partid},{funcid},{functxt})").on(
          'compid -> costfunc.compid,
          'partid -> costfunc.partid,
          'funcid -> costfunc.funcid,
          'functxt -> costfunc.functxt
      ).executeUpdate()      
    )
  }
  def getcostfunc(compid : String,partid: String, funcid: String):Costfunction ={
   var cfs = DB.withConnection(implicit c=>
    	SQL("select * from costfunction where compid={compid} and partid={partid} and funcid={funcid}").on(
		       'compid -> compid,
		       'partid -> partid,
		       'funcid -> funcid
		   ).as(costfunction *)
    )
    cfs.head
    
  }
  
  def update(costfunc: Costfunction)={
    DB.withConnection(implicit c =>
    	SQL("update costfunction set functxt ={functxt} where compid={compid} and partid={partid} and funcid={funcid}").on(
    		   'functxt -> costfunc.functxt,
    		   'compid -> costfunc.compid,
		       'partid -> costfunc.partid,
		       'funcid -> costfunc.funcid
    	).executeUpdate()
    )
    
  }
  
  
  val costfunction ={
    get[String]("compid")~
    get[String]("partid")~
    get[String]("funcid")~
    get[String]("functxt") map {
      case compid~partid~funcid~functxt => Costfunction(compid,partid,funcid,functxt)
    }
  }
}