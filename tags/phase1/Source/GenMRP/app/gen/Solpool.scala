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
  
  
  def generate_random_sol(soln : Soln ,genconfig:Genconfig):Soln = {
    //create new solution
   
    var randquantity = new Random()
    var orderlist: List[Order] = Nil
    
    
    soln.portlist.foreach( order => orderlist ::= Order(order.compid,order.partid,order.orderid,order.scheduleid,order.days, randquantity.nextInt(genconfig.maxorder)))
    
    var tempsol =  soln.copy(portlist = orderlist.reverse)
    tempsol
   
  }
  
  def generate_poolof_random_sol(poolsize:Int,soln:Soln ,genconfig:Genconfig):List[Soln] = {
    
	println("Generate pool of random solutions")
	var solpooltem  = Solpool(List(generate_random_sol(soln,genconfig),generate_random_sol(soln,genconfig)))
	
	for( i <- 0 until poolsize-2){
	 
	  var tempsol = generate_random_sol(soln,genconfig)
	  println()	  
	  solpooltem.sols  ::= 	 Soln(tempsol.partsarray,tempsol.bomlist, tempsol.orderlist,tempsol.portlist,0.0)
	  
	}
	
	 
	println("Length of solution pool "+ solpooltem.sols.length)
	return solpooltem.sols
  }
  

  def generate_poolof_valid_random_sol( soln:Soln,primsol:Soln, genconfig :Genconfig): List[Soln] = {
    var solpooltem :Solpool = Solpool(Nil)
	var solcount = 0
    var temprimsol = primsol.copy()
    solpooltem.sols ::= soln.copy()
    
	while( solcount< genconfig.poolsize-1){
	  
	  var tempsol = generate_random_sol(soln,genconfig)
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
         
       }
     })
     
     fitnessval = 1/fitnessval
     soln.fitness = fitnessval
     
  }
  
  def iteration(soln:Soln,solpool : Solpool, cfunclist : List[Costfunction],genconfig:Genconfig) ={
    // Generate Crossover population
    var solpoolsize = solpool.sols.size
    //gen_crossover_pop(soln,solpool,cfunclist)
    gen_crossover_pop_stedy_state(soln,solpool,cfunclist,genconfig)
    // Select best fit solution
    get_best_fit_solutions(solpoolsize,solpool)
    // Replace the original solution.     
    
  }
  
  
  def get_best_fit_solutions(solpoolsize:Int,solpool: Solpool) ={
     solpool.sols = solpool.sols.sortWith(_.fitness > _.fitness )
     solpool.sols = solpool.sols.take(solpoolsize)  
    
  }
  
  
  
  def gen_crossover_pop(soln:Soln ,solpool: Solpool,cfunclist : List[Costfunction],genconfig:Genconfig) ={
     var soln1 : Soln  = Soln(soln.partsarray,Nil,Nil,Nil,0.0)
     var soln2 : Soln  = Soln(soln.partsarray,Nil,Nil,Nil,0.0)
     var newcrsols : List[Soln] = Nil    
    solpool.sols.foreach(solx => {
      if(soln1.portlist == Nil){
         soln1 = Soln(solx.partsarray,solx.bomlist, solx.orderlist,solx.portlist,solx.fitness)
      }else if(soln1.portlist != Nil && soln2.portlist == Nil){
         soln2 = Soln(solx.partsarray,solx.bomlist, solx.orderlist,solx.portlist,solx.fitness)
      }
        
      if(soln1.portlist != Nil && soln2.portlist != Nil){
        random_cross_solns(soln1,soln2,cfunclist,genconfig)
       if(Soln.validate_sol(soln1)){
          calc_fitness_value_per_sol(soln1,cfunclist)
          newcrsols ::= Soln(soln1.partsarray,soln1.bomlist, soln1.orderlist,soln1.portlist,soln1.fitness)
        }
        if(Soln.validate_sol(soln2)){
         calc_fitness_value_per_sol(soln2,cfunclist) 
         newcrsols ::= Soln(soln2.partsarray,soln2.bomlist, soln2.orderlist,soln2.portlist,soln2.fitness)
        }
        soln1.portlist = Nil
        soln2.portlist = Nil
        
        }      
    })
    
    solpool.sols = solpool.sols ::: newcrsols 
  }
  
  def gen_crossover_pop_stedy_state(soln:Soln ,solpool: Solpool,cfunclist : List[Costfunction],genconfig:Genconfig) ={
     var sol_array = solpool.sols.toArray
     var sol_size = sol_array.length
     var soln1 : Soln  = Soln(soln.partsarray,Nil,Nil,Nil,0.0)
     var soln2 : Soln  = Soln(soln.partsarray,Nil,Nil,Nil,0.0)
     var random_pos = new Random()
     var pos = 0 
     
     for(i <- 0 until sol_size){
       if(soln1.portlist == Nil){
        pos= random_pos.nextInt(sol_size)
        soln1 = sol_array(pos).copy()
        }else if(soln1.portlist != Nil && soln2.portlist == Nil){
         pos= random_pos.nextInt(sol_size)
         soln2 =  sol_array(pos).copy()
        }
       if(soln1.portlist != Nil && soln2.portlist != Nil){
          random_cross_solns(soln1,soln2,cfunclist,genconfig)
       
          if(Soln.validate_sol(soln1)){
            calc_fitness_value_per_sol(soln1,cfunclist)
            solpool.sols ::= soln1.copy()
          }
            
          if(Soln.validate_sol(soln2)){
            calc_fitness_value_per_sol(soln2,cfunclist)
            solpool.sols ::= soln2.copy()
          }
          
         
         
          soln1.portlist = Nil
          soln2.portlist = Nil
       }
       
     }
  }
  
  def mutate(soln : Soln,genconfig:Genconfig)={
    var random_pos = new Random()
    var sol_length = soln.portlist.length
    var order_list : List [Order] = Nil
    
    soln.portlist.map( order => {
      if(random_pos.nextInt(sol_length) < 1){
        order_list ::= order.copy(quantity = random_pos.nextInt(genconfig.maxorder))
      }else{
        order_list ::= order.copy()
      }
       
      })
     soln.portlist = order_list.reverse
    
  }
    
  
   private def random_cross_solns(soln1:Soln,soln2:Soln,cfunclist : List[Costfunction],genconfig:Genconfig) = {
     var random_pos = new Random()
     
      
     var ran_pos  = random_pos.nextInt(soln1.portlist.length-1)+1 //+1 is to eliminate  being randomly generated
     
      var tempsoln1 = soln1.portlist.splitAt(ran_pos)._1 ::: soln2.portlist.splitAt(ran_pos)._2
      var tempsoln2 = soln2.portlist.splitAt(ran_pos)._1 ::: soln1.portlist.splitAt(ran_pos)._2
      
      soln1.portlist = tempsoln1
      mutate(soln1,genconfig)
      soln1.fitness = 0.0
      soln2.portlist = tempsoln2
      mutate(soln2,genconfig)
      soln2.fitness = 0.0
      
   }
  
   //just for debugging purposes.
   def print_sol_list_of_orders(soln: Soln) ={
     soln.portlist.foreach(orderc => print(orderc.partid+"-"+orderc.days+"-"+ orderc.quantity + " "))
	       print("---"+soln.fitness)
	       println()
   }
  
  
  
  
}