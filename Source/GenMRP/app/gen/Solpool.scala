package gen

import javax.script.ScriptEngine
import javax.script.ScriptEngineManager
import javax.script.ScriptException
import scala.util.Random

import mrp.Soln
import models._


case class Solpool( var sols : List[Soln])

object Solpool {

  private def functioneval(fn:String,Q:Int): Double ={
    
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
    var tempsol =  soln.copy(portlist = orderlist.reverse)
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
	
	
	/*for( i <- 0 until poolsize){
	  //solarray(i).portlist.foreach(or =>print(or.quantity+" ") )
	  var tempsol = solarray(i)
	  tempsol.portlist.foreach(or => print(or.quantity+" "))
	  println()
	}*/
	
	
	/*while(!solpool.isEmpty){	  
	  
	  var temp_sol = solpool.pop()
	  
	  temp_sol.portlist.foreach( order => print(order.quantity+" "))
	  
	  println()
	  
	}*/
	  
    return solpooltem.sols
  }
  

  def generate_poolof_valid_random_sol(poolsize:Int, soln:Soln): List[Soln] = {
    var solpooltem :Solpool = Solpool(Nil)
	
	
	
	//var solarray = new Array[Soln](poolsize)
    var solcount = 0
    solpooltem.sols ::= soln.copy()
	while( solcount< poolsize-1){
	  
	  var tempsol = generate_random_sol(soln)
	  if(Soln.validate_sol(tempsol)){
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
  }
  
  def iteration(soln:Soln,solpool : Solpool, cfunclist : List[Costfunction]) ={
    // Generate Crossover population
    var solpoolsize = solpool.sols.size
    gen_crossover_pop(soln,solpool,cfunclist)
    
    // Select best fit solution
    get_best_fit_solutions(solpoolsize,solpool)
    // Replace the original solution.     
    
  }
  
  
  def get_best_fit_solutions(solpoolsize:Int,solpool: Solpool) ={
     solpool.sols = solpool.sols.sortWith(_.fitness > _.fitness )
     solpool.sols = solpool.sols.take(solpoolsize)  
    
  }
  
  def gen_crossover_pop(soln:Soln ,solpool: Solpool,cfunclist : List[Costfunction]) ={
     var soln1 : Soln  = Soln(soln.partsarray,Nil,Nil,Nil,0.0)
     var soln2 : Soln  = Soln(soln.partsarray,Nil,Nil,Nil,0.0)
     var newcrsols : List[Soln] = Nil    
    solpool.sols.foreach(solx => {
      if(soln1.portlist == Nil){
         //soln1 = soln.copy()
         soln1 = Soln(solx.partsarray,solx.bomlist, solx.orderlist,solx.portlist,solx.fitness)
      }else if(soln1.portlist != Nil && soln2.portlist == Nil){
        //soln2 = soln.copy()
         soln2 = Soln(solx.partsarray,solx.bomlist, solx.orderlist,solx.portlist,solx.fitness)
      }
        
      if(soln1.portlist != Nil && soln2.portlist != Nil){
        //cross two solutions
        random_cross_solns(soln1,soln2,cfunclist)
       /* println("Crossover solutions")
        print_sol_list_of_orders(soln1)
        print_sol_list_of_orders(soln2)*/
     
        
        //add soln1 and sol2 to the Crossover population
        //newcrsols ::= soln1.copy()
        if(Soln.validate_sol(soln1))
        newcrsols ::= Soln(soln1.partsarray,soln1.bomlist, soln1.orderlist,soln1.portlist,soln1.fitness)
        //newcrsols ::= soln2.copy()
        if(Soln.validate_sol(soln2))
        newcrsols ::= Soln(soln2.partsarray,soln2.bomlist, soln2.orderlist,soln2.portlist,soln2.fitness)
        soln1.portlist = Nil
        soln2.portlist = Nil
        
        //null sol1 and sol2 for next round
      }      
    })
    
    solpool.sols = solpool.sols ::: newcrsols 
  }
  
   private def random_cross_solns(soln1:Soln,soln2:Soln,cfunclist : List[Costfunction]) = {
     var random_pos = new Random()
     
      
     var ran_pos  = random_pos.nextInt(soln1.portlist.length-1)+1 //+1 is to eliminate  being randomly generated
     
     /*println("original Soultions")
     print_sol_list_of_orders(soln1)
     print_sol_list_of_orders(soln2)
     println("Cross over position "+ ran_pos)*/
    
      var tempsoln1 = soln1.portlist.splitAt(ran_pos)._1 ::: soln2.portlist.splitAt(ran_pos)._2
      var tempsoln2 = soln2.portlist.splitAt(ran_pos)._1 ::: soln1.portlist.splitAt(ran_pos)._2
      
      soln1.portlist = tempsoln1
      calc_fitness_value_per_sol(soln1,cfunclist)
      soln2.portlist = tempsoln2
      calc_fitness_value_per_sol(soln2,cfunclist)
     
     /*println("Crossover solutions")
     print_sol_list_of_orders(soln1)
     print_sol_list_of_orders(soln2)*/
     
   }
  
   //just for debugging purposes.
   def print_sol_list_of_orders(soln: Soln) ={
     soln.portlist.foreach(orderc => print(orderc.partid+"-"+orderc.days+"-"+ orderc.quantity + " "))
	       print("---"+soln.fitness)
	       println()
   }
  
  
  
  
}