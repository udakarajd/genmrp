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
  
  
  val costfunction ={
    get[String]("compid")~
    get[String]("partid")~
    get[String]("funcid")~
    get[String]("functxt") map {
      case compid~partid~funcid~functxt => Costfunction(compid,partid,funcid,functxt)
    }
  }
}