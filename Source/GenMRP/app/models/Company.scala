package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current

case class Company(compid:String,name:String,password:String,description:String)

object Company{
	def find(comp:Company): List[Company]= {
	  DB.withConnection(implicit c =>
	    SQL("select * from company where compid ={compid} and password={password}").on(
	        'compid -> comp.compid,
	        'password -> comp.password
	        ).as(company *)
	  )
	  
	}
	def saveCompany(company: Company) ={ DB.withConnection { implicit c =>
    SQL("insert into company (compid,name,password,description) values ({compid},{name},{password},{description})").on(
      'compid -> company.compid,
      'name -> company.name,
      'password -> company.password,
      'description -> company.description
    ).executeUpdate()
   } }
    
	
    val company = {
	  get[String]("compid") ~ 
	  get[String]("name") ~
	  get[String]("password") ~
	  get[String]("description") map {
	    case compid~name~password~description => Company(compid, name,password,description)
	  }
	 }
	
}

