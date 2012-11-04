package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._

import gen.Solpool
import mrp.Soln


case class Genconfig(poolsize:Int,iterations:Int)

object Genconfig {
  
  def getConfig(compid: String):Genconfig = {
      var conflist = DB.withConnection{implicit c => SQL("select * from genengine where compid = {compid}").on('compid ->compid).as(genconfig *)}
      if(conflist.length>0){
       conflist.head 
      }else{
       Genconfig(0,0)
      }
       
  }
  
  def setConfig(compid:String,genconfig: Genconfig) = {
    DB.withConnection { implicit c =>
    SQL("insert into genengine(compid,poolsize,iterations) values({compid},{poolsize},{iterations})").on(
      'compid->compid,'poolsize -> genconfig.poolsize , 'iterations -> genconfig.iterations
    ).executeUpdate()
   }  
  }
  
  def updateConfig(compid:String , genconfig: Genconfig)= {
    DB.withConnection{implicit c=> 
      SQL("update genengine set poolsize ={poolsize},iterations = {iterations} where compid = {compid}").on(
          'poolsize -> genconfig.poolsize, 'iterations -> genconfig.iterations ,'compid -> compid
          ).executeUpdate()
    }
    
    
  }
  
  
  def executeGen(soln:Soln,compid:String):Solpool ={
    var genconfig = getConfig(compid)
    var solpool:Solpool = Solpool(Nil)
    if (Soln.validate_sol(soln) && genconfig.poolsize>0 && genconfig.iterations>0){
      println("Random solution")
      var randsol = Solpool.generate_poolof_valid_random_sol(genconfig.poolsize,soln)
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
		  Solpool.iteration(soln,solpool,funclist)
	  
      
    }
    solpool
  }
  
  
  val genconfig = {
	  get[Int]("poolsize")~
	  get[Int]("iterations") map {
	    case poolsize ~ iterations => Genconfig(poolsize,iterations)
	  }
  }

}