package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
//import util.control.Breaks._
import scala.collection.mutable.Stack

case class Order(compid:String,partid:String,orderid:Int ,scheduleid:Int,days:Int,quantity:Int)

object Order{
  
  def company_order_snapshot(compid: String): List[Order] = {
    DB.withConnection(implicit c =>
    SQL("select * from orders where compid ={compid}").on(
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
  
  def next_order_id : Int ={
    
    val maxorderid = DB.withConnection(implicit c=> 
     	SQL("select * from ordersnapshot where orderid in(select max(orderid) from ordersnapshot)").as(order *)  
     )
    if(maxorderid.length>0){ 
    	maxorderid.first.orderid+10
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
      maxscheduleid.first.scheduleid+10
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
    val  company_order_list = company_orders(compid)
    var  orderinfo =""
    var ordersuccess = true;
    
    porlist =Nil
    neworderlist = Nil
    
    bomlist = Nil
    var partslist = Part.allCompanyParts(compid).toArray
    bomlist =  Bom.getCompanyBom(compid)
    
    
   
    company_order_list.reverse
    company_order_list.foreach(comporder =>{
            println(comporder.partid +"pushed")  //remove
    		company_order_stack.push(comporder)
    	}
    )
    
    while(!company_order_stack.isEmpty && ordersuccess){
      var temp_stack_array = company_order_stack.toList
      company_order_stack.clear();
      //temp_stack_array=temp_stack_array.sortWith(_.days < _.days)
     /* var temp_part_orders:List[Order] =Nil
      for(p <- partslist){
        temp_part_orders = for(ord <- temp_stack_array if p.partid == ord.partid) yield   ord
        temp_part_orders = temp_part_orders.sortWith(_.days < _.days)
        println(temp_part_orders.toString())
        
        for(i <- temp_part_orders){
	        
	        company_order_stack.push(i)
        }
       
      }
      */
      temp_stack_array= temp_stack_array.sortWith(sortorders)
     // temp_stack_array = mapordersortedparts(temp_stack_array,partslist.toList)
      
      //println( temp_stack_array)
      for(i <- temp_stack_array){
	        
	        company_order_stack.push(i)
        }
      
     
      
      
      var temorder = company_order_stack.pop()
      println("Process order"+temorder.partid + " "+partslist.length)
      ordersuccess = cal_por_per_order(temorder,partslist)
      
    }
    
       
    
    if(ordersuccess){
      orderinfo =" All orders can be successfully Released"
    }else{
      orderinfo = "Order failed"
    }
    orderinfo    
  }
  
  def sortorders(order1:Order , order2:Order) = (
    if(order1.partid.equals(order2.partid)){
       order1.days> order2.days
    }else{
       order1.days< order2.days
      
    }
 )
 
 def mapordersortedparts(orderlist:List[Order],prtlst:List[Part]):List[Order]={
    
    var maporderlist = orderlist.groupBy(_.partid)
    
    var sortedorderlist:List[Order] =  List()
    sortedorderlist=Nil
    //println(maporderlist.toString())
    maporderlist.foreach( m => {
     if(m._2.length>1){
       sortedorderlist ++ m._2.sortWith(_.days >_.days) 
       //println(m._2)
     }else{
       sortedorderlist ++ m._2
       println(m._2)
     }
     println(sortedorderlist.toString())  
      
    })
    
    
    
    return sortedorderlist
  }

   
  // calculate por per order
  def cal_por_per_order(comporder:Order,partslist :Array[Part]):Boolean ={
    	
    		var tempartarray= partslist.find( e=> e.partid== comporder.partid)
    		var ordersuccess = false
    		if(!tempartarray.isEmpty){
	    		var tempart = tempartarray.head
	    		  
	    		var newordertime = comporder.days - tempart.leadtime
	    		var newquantity = comporder.quantity -tempart.onhand
	    		
	    		//chang onhand values
	    		if(newquantity>0){
	    		  for(i <- 0 to  partslist.length-1){
	    		    if(partslist(i).partid == tempart.partid){	    		      
	    		      partslist(i) = Part (tempart.compid,tempart.partid,tempart.partname,tempart.leadtime,0)
	    		      println("Onand set to 0 for "+tempart.partid)
	    		    }    		    
	    		  } 	    		  
	    		}else {   			
	    			for(i <- 0 to  partslist.length-1){
		    		    if(partslist(i).partid == tempart.partid){
		    		      		      partslist(i) = Part (tempart.compid,tempart.partid,tempart.partname,tempart.leadtime,(newquantity * -1))
		    		      		      println(tempart.partid+"Onand if <0 set  "+(newquantity * -1))
		    		    }
	    			}
	    			ordersuccess = true;
	    			return ordersuccess
	    		}
	    		if(newordertime <0){
	    		  false
	    		}else if(newordertime>=0 && newquantity>0){ // order release should be put
	    		  ordersuccess = true
	    		  porlist ::=  comporder.copy( days = newordertime,quantity =newquantity)
	    		  
	    		  var dependant_parts = bomlist.filter( b => b.partid==comporder.partid)
	    		  var scheduleidtemp = comporder.scheduleid
	    		  if(dependant_parts.isEmpty){return true}
	    		  dependant_parts.foreach( dpbom => { 
	    			     //var next_order_part =partslist.find(e => e.partid == dpbom.childpartid).get
	    			     scheduleidtemp +=10
	    			     var new_part_order = comporder.copy(partid = dpbom.childpartid ,
						    			         		 scheduleid = scheduleidtemp ,
						    			         		 days = newordertime,
						    			         		 quantity =(newquantity * dpbom.quantity) )
						 
						 neworderlist ::= new_part_order   			         		 
						 
						 company_order_stack.push(new_part_order)		 
						 
	    			
	    		  
	    		  
	    		  }
	    		  
	    		  )
	    		  
	    		  ordersuccess = true;
	    		}
    		}else{
    		  println("Not found in Parts List " +comporder.partid)
    		  ordersuccess = false;
    		}
    	
	    	ordersuccess
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
  
  var porlist: List[Order]=Nil
  var neworderlist : List[Order] =Nil
  
  
  
  var bomlist:List[Bom] =Nil
  
   val  company_order_stack = new Stack[Order]
  
  
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