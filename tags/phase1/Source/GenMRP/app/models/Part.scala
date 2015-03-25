package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._



case class Part (compid:String,partid:String,partname:String,leadtime:Int,onhand:Int)

object Part{
  
  def all() : List[Part]= DB.withConnection{implicit c => SQL("select * from part").as(part *)}
  
  def allCompanyParts(compid:String) : List[Part] = DB.withConnection{implicit c => SQL("select * from part where compid = {compid}").on('compid ->compid).as(part *)}
  
  def getpart(compid:String, partid:String):Part = {
    var parts = DB.withConnection{implicit c => SQL("select * from part where compid = {compid} and partid={partid}").on('compid -> compid, 'partid -> partid).as(part *)}
    parts.head
  }
    
  def add(part:Part)= {
    
  DB.withConnection { implicit c =>
    SQL("insert into part(compid,partid,partname,leadtime,onhand) values({compid},{partid},{partname},{leadtime},{onhand})").on(
      'compid->part.compid,'partid->part.partid,'partname->part.partname,'leadtime->part.leadtime,'onhand->part.onhand
    ).executeUpdate()
   }    
  }
  
  
  def remove(part: Part)={
    DB.withConnection{implicit c =>
    SQL("delete from part where compid = {compid} and partid = {partid} ").on(
        'compid->part.compid , 'partid ->part.partid
        ).executeUpdate()
    }
      
  }
  def update(part: Part)={
    DB.withConnection{implicit c =>
      SQL("update part set partname={partname},leadtime={leadtime} , onhand={onhand} where compid={compid} and partid={partid}").on(
          'partname -> part.partname,
          'leadtime -> part.leadtime,
          'onhand -> part.onhand,
          'compid -> part.compid,
          'partid -> part.partid
          ).executeUpdate()
      
    }
    
  }
  
  
  val part = {
	  get[String]("compid")~
	  get[String]("partid") ~ 
	  get[String]("partname")~
	  get[Int]("leadtime")~
	  get[Int]("onhand") map {
	    case compid~partid~partname~leadtime~onhand => Part(compid,partid,partname,leadtime,onhand)
	  }
  }
  
  	
}