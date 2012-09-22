package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._

case class Order(compid:String,partid:String,orderid:Int ,scheduleid:Int,days:Int,quantity:Int)

object Order{
  
  def company_order_snapshot(compid: String): List[Order] = {
    DB.withConnection(implicit c =>
    SQL("select * from ordersnapshot where compid ={compid}").on(
        'compid -> compid).as(order *)
    )
  }
  
  def put_order(order:Order) = {
    DB.withConnection(implicit c=>
      SQL("insert into ordersnapshot(compid,partid,orderid,scheduleid,days,quantity) values({compid},{partid},{orderid},{scheduleid},{days},{quantity})").on(
    		 'compid -> order.compid,
    		 'partid -> order.partid,
    		 'orderid -> order.orderid,
    		 'scheduleid -> order.scheduleid,
    		 'days -> order.days,
    		 'quantity -> order.quantity
      ).executeUpdate())
  }
  
  def delete_order(order : Order) ={
    DB.withConnection(implicit c =>
    SQL("delete from ordersnapshot where orderid = {orderid} or scheduleid = {scheduleid}").on(
        'orderid -> order.orderid,
        'scheduleid -> order.scheduleid
        ).executeUpdate()
    )
  }
  
  
  val order = {
    get[String]("compid")~
    get[String]("partid")~
    get[Int]("orderid")~
    get[Int]("scheduleid")~
    get[Int]("days")~
    get[Int]("quantity") map {
      case compid~partid~orderid~scheduleid~days~quantity => Order(compid,partid,orderid,scheduleid,days,quantity)
    }
  }
}