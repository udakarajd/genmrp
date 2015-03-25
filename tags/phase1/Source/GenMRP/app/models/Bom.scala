package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
case class Bom(compid: String, partid:String , childpartid:String, quantity:Int)

object Bom{
  
  def getChildren( companyid:String ,partid:String):List[Bom] = DB.withConnection(implicit c =>
       SQL("select * from bom where compid={companyid} and partid = {partid}").on(
         'companyid -> companyid,
         'partid -> partid
       ).as(bom *)
       )
       
  def getCompanyBom(companyid:String):List[Bom] = DB.withConnection(implicit c =>
  	SQL("select * from bom where compid ={companyid}").on('companyid -> companyid).as(bom *)
  )
  
  def add(bom : Bom) = {
    DB.withConnection(implicit c =>
      SQL("insert into bom(compid,partid,childpartid,quantity) values({compid},{partid},{childpartid},{quantity})").on(
       'compid -> bom.compid,
       'partid -> bom.partid,
       'childpartid -> bom.childpartid,
       'quantity -> bom.quantity
      ).executeUpdate()
    )
  }
  def delete(bom : Bom) = {
    DB.withConnection(implicit c =>
    SQL("delete from bom where compid = {compid} and partid = {partid} and childpartid = {childpartid}").on(
      'compid -> bom.compid,
      'partid -> bom.partid,
      'childpartid -> bom.childpartid).executeUpdate())
      
    
  }
  
  val bom ={
    get[String]("compid")~
    get[String]("partid")~
    get[String]("childpartid")~
    get[Int]("quantity") map {
      case compid~partid~childpartid~quantity => Bom(compid,partid,childpartid,quantity)
    }
  }
  
}