package gen

import akka.actor._
import akka.routing.RoundRobinRouter
import akka.util.Duration
import akka.util.duration._

import javax.script.ScriptEngine
import javax.script.ScriptEngineManager
import javax.script.ScriptException

import mrp.Soln
import models.Order
import models.Part
import models.Costfunction
import models.Genconfig
import scala.util.Random

import Reports.Report
import play.api.Play.current

object ParaGen {

  //calculate(nrOfWorkers = 10, nrOfElements = 10000, nrOfMessages = 10000)
  sealed trait GenMessage
  case object Generate extends GenMessage
  case class Work(soln:Soln ,solpool: Solpool,cfunclist : List[Costfunction],genconfig:Genconfig,poolsize:Int) extends GenMessage
  case class Result( solpool: Solpool ) extends GenMessage
  case class FullResult(solpool:Solpool,starttime:Long ) extends GenMessage
  case class NextSol( solpool: Solpool ) extends GenMessage
  
  class Worker extends Actor {
    def gen_crossover_pop_stedy_state(soln:Soln ,solpool: Solpool,cfunclist : List[Costfunction],genconfig:Genconfig,sol_size:Int):Solpool ={
     var sol_array = solpool.sols.toArray
     
     var soln1 : Soln  = Soln(soln.partsarray,Nil,Nil,Nil,0.0)
     var soln2 : Soln  = Soln(soln.partsarray,Nil,Nil,Nil,0.0)
     var tempool : Solpool = solpool.copy()
     tempool.sols = Nil
     var random_pos = new Random()
     var pos = 0 
     var solcount = 0;
     while(solcount < sol_size){
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
            tempool.sols ::= soln1.copy()
            solcount +=1
          }
            
          if(Soln.validate_sol(soln2)){
            calc_fitness_value_per_sol(soln2,cfunclist)
            tempool.sols ::= soln2.copy()
            solcount += 1
          }
          soln1.portlist = Nil
          soln2.portlist = Nil
       }
       
     }
    tempool
    
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
   
	   def receive = {
	       case Work(soln,solpool,cfunclist,genconfig,poolsize) => {
	       sender ! Result(gen_crossover_pop_stedy_state(soln,solpool,cfunclist ,genconfig, poolsize)) // perform the work
	       }
	   }
   
  }//end of workder actor
  
  class Master(
    soln:Soln ,solpool: Solpool,cfunclist : List[Costfunction],genconfig:Genconfig,  
    listener: ActorRef) extends Actor {

    
    var nrOfResults: Int = _
    var start: Long = System.currentTimeMillis
    var newsolpool = solpool.copy()
    val workerRouter = context.actorOf(
      Props[Worker].withRouter(RoundRobinRouter(genconfig.workers)), name = "workerRouter")

    def receive = {
      case Generate =>{
        var poolsize = 2;
        start = System.currentTimeMillis
        if(solpool.sols.length > genconfig.workers)
    	  poolsize = scala.math.round(solpool.sols.length/genconfig.workers).toInt
    	  
        for (i ← 0 until genconfig.workers) {
          workerRouter ! Work(soln,solpool,cfunclist,genconfig,poolsize)
          
        }
      }
      case Result(value) =>{
        value.sols.foreach(s => newsolpool.sols ::= s.copy() )
        
        nrOfResults += 1
        if (nrOfResults == genconfig.workers) {
          nrOfResults = 0;
          listener ! FullResult(newsolpool,start)
          }
      }
      case NextSol(value) =>{
        
        var poolsize = 2
        newsolpool = value.copy()
        if(value.sols.length > genconfig.workers)
    	  poolsize = scala.math.round(value.sols.length/genconfig.workers).toInt
    	  
        for (i ← 0 until genconfig.workers) {
          workerRouter ! Work(soln,value,cfunclist,genconfig,poolsize)
          
        }
        
      }
    }
    

  }//endo fo master actor
  
  class Listener(genconfig:Genconfig,solpoolsize:Int) extends Actor {
    private var finalsolpool : Solpool = _
    private var curiter = 0
    
    def savereportpdf(soln:Soln,filepath:String)={
      var report:Report = Report.reportSoln(soln)
      var folderpath = filepath
      var filename ="GenMRP_"+ soln.partsarray.head.compid+"_"+ System.currentTimeMillis() + ".pdf"
      if(filepath.equals(".")){
        folderpath = current.path.getAbsolutePath.toString()+"/public/"
      }
      Report.saveReportpdf(report,folderpath+filename)
    }
    
    
    def savereporthtml(soln:Soln,filepath:String)={
      var report:Report = Report.reportSoln(soln)
      var folderpath = filepath
      var filename = "GenMRP_"+ soln.partsarray.head.compid+"_"+ System.currentTimeMillis() +".htm"
       if(filepath.equals(".")){
        folderpath = current.path.getAbsolutePath.toString()+"/public/"
      }
      Report.saveReportHtml(report,folderpath+filename)
      
    }
    def TimeDiff(miliseconds : Long):String ={
      var diff= miliseconds
      var timestring = ""
      var milisecs =  diff % 1000
      diff = (diff - (diff % 1000))/1000
      var dseconds = diff % 60
      diff = (diff - (diff % 60))/60
      var dminutes = diff % 60
      diff = (diff - (diff % 60))/60
      var dhours =  diff % 60
      timestring =  dhours+"h: "+dminutes+"m: "+dseconds+"s: "+milisecs+"ms:"
     
      timestring
  }
    
    def receive = {
     
      case FullResult(value,starttime)=>{}
        if(curiter < genconfig.iterations){
          curiter +=1
          value.sols = value.sols.sortWith(_.fitness > _.fitness )
          value.sols = value.sols.take(solpoolsize)  
          sender ! NextSol(value)
        }else{
          curiter = 0
          value.sols = value.sols.sortWith(_.fitness > _.fitness )
          value.sols = value.sols.take(solpoolsize) 
          finalsolpool = value.copy()
          savereporthtml(finalsolpool.sols.head,genconfig.reports)
          savereportpdf(finalsolpool.sols.head,genconfig.reports)
	      println(TimeDiff(System.currentTimeMillis - starttime))
          
	       /* finalsolpool.sols.foreach(s => {
	          s.portlist.foreach(o =>  print(o.partid+"->"+o.quantity+" "))
	          println()
	        })*/
	      
        }
      
	      
	      
     }   //context.system.shutdown()
    }
  
  
  def calculate(soln:Soln ,solpool: Solpool,cfunclist : List[Costfunction],genconfig:Genconfig) {
    val system = ActorSystem("GenSystem")
    		
    // create the result listener, which will print the result and 
    // shutdown the system
    
    val listener = system.actorOf(Props( new Listener(genconfig,solpool.sols.length)), name = "listener")

    // create the master
    val master = system.actorOf(Props(new Master(
      soln,solpool,cfunclist,genconfig,listener)),
      name = "master")

    // start the calculation
    master ! Generate

  }
  
  
  
}