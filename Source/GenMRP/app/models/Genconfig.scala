package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._

import gen.Solpool
import mrp.Soln


case class Genconfig(poolsize:Int,iterations:Int,maxorder:Int)

object Genconfig {
  
  def getConfig(compid: String):Genconfig = {
      var conflist = DB.withConnection{implicit c => SQL("select * from genengine where compid = {compid}").on('compid ->compid).as(genconfig *)}
      if(conflist.length>0){
       conflist.head 
      }else{
       Genconfig(0,0,0)
      }
       
  }
  
  def setConfig(compid:String,genconfig: Genconfig) = {
    DB.withConnection { implicit c =>
    SQL("insert into genengine(compid,poolsize,iterations,maxorder) values({compid},{poolsize},{iterations},{maxorder})").on(
      'compid->compid,'poolsize -> genconfig.poolsize , 'iterations -> genconfig.iterations,'maxorder ->genconfig.maxorder
    ).executeUpdate()
   }  
  }
  
  def updateConfig(compid:String , genconfig: Genconfig)= {
    DB.withConnection{implicit c=> 
      SQL("update genengine set poolsize ={poolsize},iterations = {iterations},maxorder = {maxorder} where compid = {compid}").on(
          'poolsize -> genconfig.poolsize, 'iterations -> genconfig.iterations ,'maxorder -> genconfig.maxorder,'compid -> compid
          ).executeUpdate()
    }
    
    
  }
  
  
  def executeGen(soln:Soln,primsol:Soln,compid:String):Solpool ={
    var genconfig = getConfig(compid)
    var solpool:Solpool = Solpool(Nil)
    var copyprimsol = primsol.copy()
    if (Soln.validate_sol(soln) && genconfig.poolsize>0 && genconfig.iterations>0){
      println("Random solution")
      var randsol = Solpool.generate_poolof_valid_random_sol(soln,copyprimsol,genconfig)
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
	  solpool = Solpool(randsol)
	  for (i <-0 until genconfig.iterations	)
		  Solpool.iteration(soln,solpool,funclist,genconfig)
	  
      
    }
    //test validate bom
    var  partslist = Part.allCompanyParts(compid).toArray
    var primesoln = Soln(partslist.clone(),Bom.getCompanyBom(compid),Order.company_orders(compid),Nil,0.0)
 
      Solpool.print_sol_list_of_orders(solpool.sols.head)
      var valid = Soln.validate_sol_with_bom(soln,primesoln)
      if(valid)
      {
        print("Valid")
      }else{
        print("Invalid")
      } 
        
   
      
      
     
    
    //end test validate bom
    solpool
  }
  
  def TimeDiff(miliseconds : Double):String ={
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
  
  
  val genconfig = {
	  get[Int]("poolsize")~
	  get[Int]("iterations")~
	  get[Int]("maxorder") map {
	    case poolsize ~ iterations ~ maxorder => Genconfig(poolsize,iterations,maxorder)
	  }
  }

}