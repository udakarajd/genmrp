package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
//import util.control.Breaks._
import scala.collection.mutable.Stack

import mrp.Soln
import gen.Solpool
import Reports.Report

case class Order(compid:String,partid:String,orderid:Int ,scheduleid:Int,days:Int, quantity:Int)


object Order{
  
  def company_order_snapshot(compid: String): List[Order] = {
    DB.withConnection(implicit c =>
    SQL("select * from orders where compid ={compid}").on(
        'compid -> compid).as(order *)
    )
  }
  
  
  def updateorder(order:Order) ={
    DB.withConnection(implicit c =>
    	SQL("update ordersnapshot set days={days} , quantity={quantity} where " +
    			"compid={compid} and partid={partid} and orderid={orderid} and scheduleid={scheduleid} ").on(
    		'days -> order.days,	
    	    'quantity -> order.quantity,
    	    'compid -> order.compid,
    	    'partid -> order.partid,
    	    'orderid -> order.orderid,
    	    'scheduleid -> order.scheduleid
    	).executeUpdate()
    	
    )
  }
  def getOrder(compid:String,orderid:Int):Order ={
    var orders = DB.withConnection(implicit c => SQL("select * from ordersnapshot where compid={compid} and orderid={orderid}").on('compid -> compid,'orderid -> orderid).as(order *))
    orders.head
  
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
  
  def next_order_id : Int ={
    
    val maxorderid = DB.withConnection(implicit c=> 
     	SQL("select * from ordersnapshot where orderid in(select max(orderid) from ordersnapshot)").as(order *)  
     )
    if(maxorderid.length>0){ 
    	maxorderid.head.orderid + 10 //maxorderid.first.orderid + 10
    } else{
    	10 //start order id
    }
       
    
  }
  
  def next_schdeuld_id(orderid:Int):Int={
    
    val maxscheduleid = DB.withConnection(implicit c=>
      SQL("select * from ordersnapshot where orderid ={orderid} and scheduleid in (select max(scheduleid) from ordersnapshot)").on(
          'orderid -> orderid
          ).as(order *)        
    )
    if(maxscheduleid.length>0){
      maxscheduleid.head.scheduleid + 10 //maxscheduleid.first.scheduleid + 10
    } else{
      10 //Start Schedule id
    } 
    
  }
  
 //---------------------------functions for order generation.------------------
  def company_orders(compid : String): List[Order] ={
    DB.withConnection(implicit c =>
    SQL("select * from orders where compid ={compid} and scheduleid=10").on(
        'compid -> compid).as(order *)
    )    
  }
  
  //generate plan order release. 
  def gen_por(compid:String):String = {
    
    var  orderinfo =""
    var ordersuccess = true;
    var  company_order_stack = new Stack[Order]
    var  partslist = Part.allCompanyParts(compid).toArray
  
    var soln = Soln(partslist.clone(),Bom.getCompanyBom(compid),company_orders(compid),Nil,0.0)
    
    setPrimSoln(soln)
    porlist =Nil
    neworderlist = Nil    
       
    ordersuccess = Soln.gen_por(soln) //generate Purchase Order Release
    soln.partsarray = partslist // Again set original parts information 
    
    porlist = soln.portlist 
    neworderlist = soln.orderlist
    mainsoln = soln.copy()
    
    println()
    if(ordersuccess){
      orderinfo =" All orders can be successfully Released"
        
	    if (Soln.validate_sol(soln)){
	      orderinfo += "- Validate Success <br/>"
	      var r = Report.reportSoln(soln)
	      orderinfo = r.body
	    
	     
	     
	    }else{
	      orderinfo += "- Validate failed "
	    }    
	    // end validate  solution test 
        
        
    }else{
      orderinfo = "Order failed"
    }
    
    
    
    
    orderinfo    
  }

 
 def mapordersortedparts(orderlist:List[Order],prtlst:List[Part]):List[Order]={
    
    var maporderlist = orderlist.groupBy(_.partid)
    
    var sortedorderlist:List[Order] =  List()
    sortedorderlist=Nil
    maporderlist.foreach( m => {
     if(m._2.length>1){
       sortedorderlist ++ m._2.sortWith(_.days >_.days) 
       
     }else{
       sortedorderlist ++ m._2
       println(m._2)
     }
     println(sortedorderlist.toString())  
      
    })
    
    
    
    return sortedorderlist
  }

  
  def getPorList : List[Order] ={
    if (porlist != Nil){
      porlist
    }else{
      Nil
    }     
  }
  def getNewOrderList : List[Order] ={
    if (neworderlist != Nil){
      neworderlist
    }else{
      Nil
    }     
  }
  def getMainSol : Soln ={
    mainsoln
  }
 
  def getPrimSoln : Soln ={
    primsoln
  }
  def setPrimSoln(soln:Soln) ={
    primsoln = soln.copy()  //Only orders in the db
  }
  
  
  var porlist: List[Order]= _
  var neworderlist : List[Order] = _
  var mainsoln :Soln =_ 
  var primsoln :Soln =_
  
  
  
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