package mrp



import models._
import scala.collection.mutable.Stack
import javax.script.ScriptEngine
import javax.script.ScriptEngineManager
import javax.script.ScriptException
import scala.util.Random
import scala.math._
import org.joda.time.Days

import gen.Solpool

case class Soln(var partsarray : Array[Part],var bomlist : List[Bom], var orderlist: List[Order],var portlist : List[Order],var fitness:Double)
//case class Solpool( var sols : List[Soln])

object Soln{  
  
  
  
  def gen_por(soln : Soln):Boolean ={
     var  company_order_stack = new Stack[Order]
     var ordersuccess = true
     soln.orderlist.foreach(comporder =>{
    		company_order_stack.push(comporder)
    	}
     )
     
     while(!company_order_stack.isEmpty && ordersuccess){
	     //sort stack 
	     var temp_stack_array = company_order_stack.toList
	     company_order_stack.clear();      
	     temp_stack_array= temp_stack_array.sortWith(sortorders)
	    // temp_stack_array = sortordersmanual(temp_stack_array)
	     for(i <- temp_stack_array){	        
		        company_order_stack.push(i)
	     }
	     var temorder = company_order_stack.pop()     
         ordersuccess = cal_por_per_order(temorder,company_order_stack,soln)    
     }
     
     ordersuccess
  }
     
  private def sortorders(order1:Order , order2:Order):Boolean = {
   if(order1.partid.equals(order2.partid)){
       order1.days >= order2.days
    }else{
       order1.days < order2.days
   //   false
    }
  }
  
  //this does not work well
  private def sortordersmanual(orderlist:List[Order]):List[Order]={
      var sortOrders : List[Order]= Nil
      var temporderlist = orderlist
      println("Start lenght of Order list"+ orderlist.length)
      while(temporderlist.length>0){
    	  	 var minDaysOrder = temporderlist.minBy(Order => Order.days)
    	  	 println("Part "+ minDaysOrder.partid +" lenght "+temporderlist.length)
    	  	 var minOrderList:List[Order] = temporderlist.filter(or => or.partid == minDaysOrder.partid) //remove(or => or.partid == minDaysOrder.partid)
    	  	 temporderlist = temporderlist.filterNot(or => or.partid == minDaysOrder.partid)
    	  	 println("minOrderList "+minOrderList.length)
    	  	 minOrderList = minOrderList.sortWith(_.days < _.days)
    	  	 minOrderList.foreach{
    	  	   o =>  sortOrders ::= o.copy()
    	  	 }    	  	
    	  	 
    	  	 println(temporderlist.length)
    	  	 println("Sort order list len"+sortOrders.length)
      }
     temporderlist = sortOrders
     sortOrders
  }
    
  def cal_por_per_order(comporder:Order,company_order_stack : Stack[Order],soln : Soln):Boolean ={
    	//var partsgroup = orderlist.groupBy(order => order.partid)
    		
    		var tempartarray = soln.partsarray.find(e=> e.partid == comporder.partid)
    		var ordersuccess = false
    		if(!tempartarray.isEmpty){
	    		var tempart = tempartarray.head
	    		  
	    		var newordertime = comporder.days - tempart.leadtime
	    		var newquantity = comporder.quantity -tempart.onhand
	    		
	    		//chang onhand values
	    		if(newquantity>0){	    		    
	    		     soln.partsarray(soln.partsarray.indexWhere(e => e.partid == tempart.partid)) = Part (tempart.compid,tempart.partid,tempart.partname,tempart.leadtime,0)
	    		}else {	    			
	    		    soln.partsarray(soln.partsarray.indexWhere(e => e.partid == tempart.partid))=  Part (tempart.compid,tempart.partid,tempart.partname,tempart.leadtime,(newquantity * -1))
	    			return true
	    		}
	    		
	    		if(newordertime <0){
	    		  return false
	    		}else if(newordertime >= 0 && newquantity > 0){ // order release should be put
	    		  ordersuccess = true
	    		  soln.portlist ::=  comporder.copy( days = newordertime,quantity = newquantity)
	    		   
	    		  var dependant_parts = soln.bomlist.filter( b => b.partid == comporder.partid)
	    		  var scheduleidtemp = comporder.scheduleid
	    		  if(dependant_parts.isEmpty){return true}
	    		  dependant_parts.foreach( dpbom => { 
	    			     scheduleidtemp += 10
	    			     var new_part_order = comporder.copy(partid = dpbom.childpartid ,
						    			         		 scheduleid = scheduleidtemp ,
						    			         		 days = newordertime,
						    			         		 quantity =(newquantity * dpbom.quantity) )
						 
						 
						 soln.orderlist ::= new_part_order
						 
						 company_order_stack.push(new_part_order)		 
				    		  
	    		  })
	    		  
	    		  ordersuccess = true;
	    		}else{
	    		  return false
	    		}
    		}else{
    		  println("Not found in Parts List " +comporder.partid)
    		  ordersuccess = false;
    		}
    	
	    	ordersuccess
  }
  
  
  
  //prime solution is the first solution from the database. 
  def validate_sol_with_bom(soln : Soln,primeSoln:Soln):Boolean ={
    var  company_order_stack = new Stack[Order]
     var ordersuccess = true
     var temPrimSoln = primeSoln.copy()
     temPrimSoln.orderlist.foreach(comporder =>{
    		company_order_stack.push(comporder)
    	}
     )
    //println("Stack created")
     
     while(!company_order_stack.isEmpty && ordersuccess){
	     //sort stack 
	     var temp_stack_array = company_order_stack.toList
	     company_order_stack.clear();      
	     temp_stack_array= temp_stack_array.sortWith(sortorders)
	     for(i <- temp_stack_array){	        
		        company_order_stack.push(i)
	     }
	     var temorder = company_order_stack.pop()
	     //println(" validate order "+ temorder.toString())
         ordersuccess = validate_sol_with_bom_per_order(temorder,company_order_stack,primeSoln,soln)    
     }
     
     temPrimSoln.portlist = Nil
     temPrimSoln.orderlist = primeSoln.orderlist
     ordersuccess
    
       
  }
  private def validate_sol_with_bom_per_order(comporder:Order,company_order_stack : Stack[Order],soln : Soln, genSoln:Soln):Boolean ={
	  	   var tempartarray = soln.partsarray.find(e=> e.partid == comporder.partid)
    		var ordersuccess = false
    		if(!tempartarray.isEmpty){
	    		var tempart = tempartarray.head
	    		  
	    		var newordertime = comporder.days - tempart.leadtime
	    		var newquantity = comporder.quantity -tempart.onhand
	    		
	    		//chang onhand values
	    		if(newquantity>0){	    		    
	    		     soln.partsarray(soln.partsarray.indexWhere(e => e.partid == tempart.partid)) = Part (tempart.compid,tempart.partid,tempart.partname,tempart.leadtime,0)
	    		}else {	    			
	    		    soln.partsarray(soln.partsarray.indexWhere(e => e.partid == tempart.partid))=  Part (tempart.compid,tempart.partid,tempart.partname,tempart.leadtime,(newquantity * -1))
	    			return true
	    		}
	    		
	    		if(newordertime <0){
	    		  return false
	    		}else if(newordertime >= 0 && newquantity > 0){ // order release should be put
	    		  ordersuccess = true
	    		  
	    		  var neworder:Order = comporder.copy( days = newordertime,quantity = newquantity)
	    		 
	    		  var genorderlist = genSoln.portlist.find(order => (order.days == neworder.days && order.orderid == neworder.orderid && order.partid.equals(neworder.partid)  && order.scheduleid == neworder.scheduleid) )
	    		  if(genorderlist.isEmpty){
	    		   // println("new order"+neworder.toString())
	    		  	//println("Genetic order"+ genorder.toString())
	    		    return true
	    		  }
	    		  var genorder = genorderlist.head 
	    		  
	    		  if(genorder.quantity >= neworder.quantity){
	    		   
	    		    soln.portlist ::= genorder.copy()
	    		  }else{
	    		     //test
	    		  	  Solpool.print_sol_list_of_orders(soln)
	    		  	  println("expected order "+ neworder.toString())
	    		  	  println("gen order " + genorder.toString())
	    		  	
	    		  //end test
	    		    return false
	    		  }
	    		  //soln.portlist ::=  comporder.copy( days = newordertime,quantity = newquantity)
	    		   
	    		  var dependant_parts = soln.bomlist.filter( b => b.partid == comporder.partid)
	    		  var scheduleidtemp = comporder.scheduleid
	    		  if(dependant_parts.isEmpty){
	    		    return true
	    		   }
	    		  dependant_parts.foreach( dpbom => { 
	    			     scheduleidtemp += 10
	    			     var new_part_order = genorder.copy(partid = dpbom.childpartid ,
						    			         		 scheduleid = scheduleidtemp ,
						    			         		 days = newordertime,
						    			         		 quantity =(newquantity * dpbom.quantity) )
						 
						 
						 soln.orderlist ::= new_part_order
						 
						 company_order_stack.push(new_part_order)		 
				    		  
	    		  })
	    		  
	    		  ordersuccess = true;
	    		}else{
	    		  return false
	    		}
    		}else{
    		  println("Not found in Parts List " +comporder.partid)
    		  ordersuccess = false;
    		}
    	
	    	ordersuccess
    
  }
  
  
  
  
  def validate_sol(soln : Soln):Boolean = { 
   
     soln.partsarray.foreach( part => {
              
      // var tempsol: Soln = soln.copy()
       var tempsol: Soln =  Soln(soln.partsarray,soln.bomlist, soln.orderlist,soln.portlist,0.0)
       
       if(sol_of_part(part.partid,tempsol)){
    	      	   
    	   if (!validate_sol_of_part(part.onhand,tempsol,part.leadtime))//validate solution of a part
    		   return false
       }
       
     }) //end of foreach
      
    true
  }
  
  private def validate_sol_of_part(onhand: Int ,soln: Soln,leadtime :Int):Boolean = {
    var sum_order_quantity = 0
    var sum_por_quantity = onhand
    var startday = (soln.orderlist.minBy(_.days).days - leadtime)
           if(soln.portlist.length>0)
        	  startday = min(soln.portlist.minBy(_.days).days,startday)
    	   //var startday = min(soln.portlist.minBy(_.days).days,(soln.orderlist.minBy(_.days).days - leadtime))
    	  
    	   var endday = soln.orderlist.maxBy(_.days).days
    	   
    	   for (i <- startday until endday){
    	      // print(" "+i)
    	     if(i + leadtime <= endday)
    	     soln.orderlist.filter(_.days == (i+leadtime)).map{ e =>
    	       sum_order_quantity += e.quantity
    	     //  print("-O"+sum_order_quantity)
    	       }
    	      
    	     soln.portlist.filter(_.days == i).map{e => 
    	       sum_por_quantity += e.quantity
    	      //  print("-P"+sum_por_quantity)
    	       }
    	     
    	     if (sum_order_quantity > sum_por_quantity)
    	      	return false
    	   }
    //println("Start date- "+ startday.days+" End day- "+endday.days)
    //println("Sum order quantity "+ sum_order_quantity +" Sum por quantity "+ sum_por_quantity )
    true
  }
 
  /* create a sub solution of a part and return false if the part is not  in the solution */
  private def sol_of_part(part_id : String , soln: Soln): Boolean = {
             
    soln.orderlist = soln.orderlist.filterNot(e => !e.partid.equals(part_id))
    if(!(soln.orderlist.length > 0) ){
       return false
    }
    soln.portlist = soln.portlist.filterNot(e => !e.partid.equals(part_id))
    
    true
  }
  
  
  
/*  private def functioneval(fn:String,Q:Int): Double ={
    
   val manager = new ScriptEngineManager()
   var engin = manager.getEngineByName("js")
   var output = 0.0
   engin.put("Q",Q)
   
   
   try{
      output = engin.eval(fn).toString().toDouble
       
   }catch {
     case ex: ScriptException =>  print("The fn eval didn't work ,script exception")
     case e:Exception => {print("Some exceptino why fn eval")}
   }
   
   output
  }
  
  
  def generate_random_sol(soln : Soln ):Soln = {
    //create new solution
   
    var randquantity = new Random()
    var orderlist: List[Order] = Nil
    
    
    soln.portlist.foreach( order => orderlist ::= Order(order.compid,order.partid,order.orderid,order.scheduleid,order.days, randquantity.nextInt(200)))
    //var l = soln.copy(portlist = tempsol.portlist )
    
   //    tempsol.portlist.foreach(order => print(order.quantity+" "))
   //    println()
    var tempsol =  soln.copy(portlist = orderlist)
    tempsol
   
  }
  
  def generate_poolof_random_sol(poolsize:Int,soln:Soln):List[Soln] = {
    
	//var solpool:List[Soln]  = Nil
	
	var solpooltem  = Solpool(List(generate_random_sol(soln),generate_random_sol(soln)))
	
	
	
	//var solarray = new Array[Soln](poolsize)
	for( i <- 0 until poolsize-2){
	 
	  var tempsol = generate_random_sol(soln)
	  println()	  
	  solpooltem.sols  ::= 	 Soln(tempsol.partsarray,tempsol.bomlist, tempsol.orderlist,tempsol.portlist,0.0)
	  
	}
	
	 
	println("Length of solution pool "+ solpooltem.sols.length)
	
	//solpooltem.sols.foreach(s => {s.portlist.foreach(o => print(o.quantity+" "))
	// 						println()})
	
	
	for( i <- 0 until poolsize){
	  //solarray(i).portlist.foreach(or =>print(or.quantity+" ") )
	  var tempsol = solarray(i)
	  tempsol.portlist.foreach(or => print(or.quantity+" "))
	  println()
	}
	
	
	while(!solpool.isEmpty){	  
	  
	  var temp_sol = solpool.pop()
	  
	  temp_sol.portlist.foreach( order => print(order.quantity+" "))
	  
	  println()
	  
	}
	  
    return solpooltem.sols
  }
  

  def generate_poolof_valid_random_sol(poolsize:Int, soln:Soln): List[Soln] = {
    var solpooltem :Solpool = Solpool(Nil)
	
	
	
	//var solarray = new Array[Soln](poolsize)
    var solcount = 0
    solpooltem.sols ::= soln
	while( solcount< poolsize-1){
	  
	  var tempsol = generate_random_sol(soln)
	  if(validate_sol(tempsol)){
	    solpooltem.sols  ::= Soln(tempsol.partsarray,tempsol.bomlist, tempsol.orderlist,tempsol.portlist,0.0)
	    solcount += 1
	  }	  
	  
	  
	}
	
    solpooltem.sols
  }
  
  def calc_fitness_value_per_sol(soln:Soln , cfunclist : List[Costfunction]) = {
    
     var fitnessval = 0.0
     
     soln.portlist.foreach(order => {
       cfunclist.find( cf => cf.partid == order.partid).map{m =>
         var fit1 = functioneval(m.functxt,order.quantity)
         fitnessval +=fit1
         //println("fn="+m.functxt+" Quantity "+order.quantity+"="+ fit1)
       }//.getOrElse()
      
     })
     
     fitnessval = 1/fitnessval
     soln.fitness = fitnessval
     //fitnessval
  }*/
    
    
  
}