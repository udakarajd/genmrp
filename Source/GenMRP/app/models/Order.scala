package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
//import util.control.Breaks._
import scala.collection.mutable.Stack

import mrp.Soln
import gen.Solpool

case class Order(compid:String,partid:String,orderid:Int ,scheduleid:Int,days:Int, quantity:Int)


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
    
    porlist =Nil
    neworderlist = Nil    
       
    ordersuccess = Soln.gen_por(soln) //generate Purchase Order Release
    soln.partsarray = partslist // Again set original parts information 
    
    porlist = soln.portlist 
    neworderlist = soln.orderlist
    
    if(ordersuccess){
      orderinfo =" All orders can be successfully Released"
        
	    //validate solution test        
	    if (Soln.validate_sol(soln)){
	      orderinfo += "- Validate Success "
	      println("Valid Solution -")
	      soln.portlist.foreach(order => print(order.quantity+" "))
	      println() 
	      println("Random solution")
	      //var randsol =  Soln.generate_random_sol(soln)
	      //var randsol = Solpool.generate_poolof_random_sol(10,soln)
	      var randsol = Solpool.generate_poolof_valid_random_sol(30,soln)
	      println("Rand Solution length" + randsol.length)
	      var funclist = Costfunction.all_company_parts(compid)
	      randsol.foreach(solx => {
	      		solx.portlist.foreach(orderc =>// print(orderc.partid+"-"+orderc.days+"-"+ orderc.quantity + " "))
	      		Solpool.calc_fitness_value_per_sol(solx,funclist)
	      		//	print("---"+solx.fitness)
	      		//println()
	      		)
	      	}
	      )
	     randsol = randsol.sortWith( _.fitness < _.fitness) 
	     randsol.foreach(solx => {
	       solx.portlist.foreach(orderc => print(orderc.partid+"-"+orderc.days+"-"+ orderc.quantity + " "))
	       print("---"+solx.fitness)
	       println()
	     })
	     println("-------- Now we create crossover pool---------")
	     var solpool = Solpool(randsol)
	     //Solpool.gen_crossover_pop(soln,solpool,funclist)
	     for (i <-0 until 5)
	      Solpool.iteration(soln,solpool,funclist)
	    
	     
	     println("---------Now Filter solutions ----")
	      solpool.sols.foreach(solx => {
	       solx.portlist.foreach(orderc => print(orderc.partid+"-"+orderc.days+"-"+ orderc.quantity + " "))
	       print("---"+solx.fitness)
	       println()
	     })
	     
	     
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
  
  var porlist: List[Order]= _
  var neworderlist : List[Order] = _
  
  
  
  
  
  
  
  
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