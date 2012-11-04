package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._



case class Part (compid:String,partid:String,partname:String,leadtime:Int,onhand:Int)

object Part{
  
  def all() : List[Part]= DB.withConnection{implicit c => SQL("select * from part").as(part *)}
  
  def allCompanyParts(compid:String) : List[Part] = DB.withConnection{implicit c => SQL("select * from part where compid = {compid}").on('compid ->compid).as(part *)}
    
    
  def add(part:Part)= {
    
  DB.withConnection { implicit c =>
    SQL("insert into part(compid,partid,partname,leadtime,onhand) values({compid},{partid},{partname},{leadtime},{onhand})").on(
      'compid->part.compid,'partid->part.partid,'partname->part.partname,'leadtime->part.leadtime,'onhand->part.onhand
    ).executeUpdate()
   }    
  }
  
  
  def remove(part: Part){
    DB.withConnection{implicit c =>
    SQL("delete from part where compid = {compid} and partid = {partid} ").on(
        'compid->part.compid , 'partid ->part.partid
        ).executeUpdate()
    }
    
   
   //--------------------------find part from a List by partid
  // def get_part_by_id(partid:String ,parts:List[Part]): Part ={
   // } 
  
   
    
    
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