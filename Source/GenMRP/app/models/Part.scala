package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._



case class Part (compid:String,partid:String,partname:String,leadtime:Int)

object Part{
  
  def all() : List[Part]= DB.withConnection{implicit c => SQL("select * from part").as(part *)}
  
  def add(part:Part){
    
  DB.withConnection { implicit c =>
    SQL("insert into part(compid,partid,partname,leadtime) values({compid},{partid},{partname},{leadtime})").on(
      'compid->part.compid,'partid->part.partid,'partname->part.partname,'leadtime->part.leadtime
    ).executeUpdate()
   }
    
  }
  
  
  def remove(part: Part){
    DB.withConnection{implicit c =>
    SQL("delete from part where compid = {compid} and partid = {partid} ").on(
        'compid->part.compid , 'partid ->part.partid
        ).executeUpdate()
    }
  }
  
  
   val part = {
  get[String]("compid")~
  get[String]("partid") ~ 
  get[String]("partname")~
  get[Int]("leadtime") map {
    case compid~partid~partname~leadtime => Part(compid,partid,partname,leadtime)
  }
  }
  
  	
}