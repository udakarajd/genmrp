package controllers

import play.api._
import play.api.mvc._

import models.Part
import models.Company
import models.LoginInfo
import models.Bom
import models.Costfunction
import models.Order

import play.api.data._
import play.api.data.Forms._
import play.api.libs.openid.Errors
import views.html.defaultpages.unauthorized

import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Promise
import play.api.Play.current



object Application extends Controller {
  
  //------------ 1.Actions--------------------
  //-------------1.1 Index actions-----------------
  
  def index = Action {implicit request =>
    request.session.get("companyid").map{ compid => 
      Ok(views.html.index(compid))
      }.getOrElse{
       //Redirect(views.html.index)
       //Ok(views.html.index("No Session"))
        Redirect(routes.Application.login)
      }    
  }
  
  def login = Action {
           Ok(views.html.login(loginForm))
  }
  def logout =Action{implicit request => 
    Ok(views.html.index("Invalid Company")).withSession(session -"companyid")
  }
  
  def loginvalidate = Action{ implicit request =>
    loginForm.bindFromRequest.fold(
        errors => BadRequest(views.html.login(errors)),
        logininfo =>{
          if(LoginInfo.validate(logininfo)){
           Ok(views.html.index("Valid Company")).withSession("companyid" ->logininfo.userid)
          }else
           Ok(views.html.login(loginForm))
        }
        )    
  }
  //------------------1.2 Company Actions -------------------------------
  def signin = Action{
	  Ok(views.html.signin(saveCompany))
  }
  
  def savecompany = Action{ implicit request =>
   saveCompany.bindFromRequest().fold(
    		errors => BadRequest(views.html.signin(errors)),
    		company =>{
    		  Company.saveCompany(company)
    		  Redirect(routes.Application.index)
    		}
    		  
    )
  }
  
  //------------------1.3 Part actions---------------------------
  
  
  
  
  def part = Action{implicit request =>
    request.session.get("companyid").map{ compid =>
      Ok(views.html.part(Part.allCompanyParts(compid),partForm.fill(Part(compid,"","",0,0))))
    }.getOrElse{
      Redirect(routes.Application.login)
    }
  
  }
   
  
  def partadd = Action{ implicit request => 
    partForm.bindFromRequest.fold(
        Errors => BadRequest(views.html.part(Part.all(),Errors)),
        part =>{ 
          Part.add(part)
         Redirect(routes.Application.part)
          }
        
    )  
  }
  
  def partdelete(partid:String) = Action{ implicit request =>
    request.session.get("companyid").map{companyid => 
      Part.remove(Part(companyid,partid,"",0,0))
      Ok(views.html.part(Part.allCompanyParts(companyid),partForm.fill(Part(companyid,"","",0,0))))
      }.getOrElse{
         Redirect(routes.Application.login)
      }
         
  }
   //------------------1.4 Bom actions---------------------------
 
  def bom (partid : String)= Action{ implicit request => 
    request.session.get("companyid").map{companyid =>
    	Ok(views.html.bom(partid,Bom.getChildren(companyid,partid),bomForm.fill(Bom(companyid,partid,"",0))))
    }.getOrElse{
       Redirect(routes.Application.login)
    }
    
  }
  def bomadd = Action{implicit request =>
    bomForm.bindFromRequest.fold(
        Errors=> BadRequest(views.html.bom("",Bom.getChildren("",""),Errors)),
        bom =>{
          Bom.add(bom)
           Redirect(routes.Application.bom(bom.partid))
        }
    		
    )
  }
  
 
  
  def bomdelete(partid : String,childpartid :String) =Action{ implicit request => 
    request.session.get("companyid").map{companyid =>
    	Bom.delete(Bom(companyid,partid,childpartid,0))
    	Redirect(routes.Application.bom(partid))
    }.getOrElse{
       Redirect(routes.Application.login)
    }
  }
  
  //-----------------1.5 Cost Functoin Actions --------------------
  
  def costfunc(partid : String) =Action{ implicit request => 
    request.session.get("companyid").map{companyid =>
    	Ok(views.html.costfunc(Costfunction.all(companyid,partid),costForm.fill(Costfunction(companyid,partid,"","f(x)"))))
    }.getOrElse{
       Redirect(routes.Application.login)
    }
  } 
  def costfuncadd() = Action{implicit request =>
    costForm.bindFromRequest.fold(
        Errors=> BadRequest(views.html.costfunc(Costfunction.all("",""),Errors)),
        costfunc =>{
          Costfunction.addfunc(costfunc)
           Redirect(routes.Application.costfunc(costfunc.partid))
        }
    		
    )
  }
  
  def costfuncdelete(partid : String , funcid : String) = Action{ implicit request => 
    request.session.get("companyid").map{companyid =>
      	Costfunction.deletefucn(Costfunction(companyid,partid,funcid,""))
    	Redirect(routes.Application.costfunc(partid))
    }.getOrElse{
       Redirect(routes.Application.login)
    }
  }
  
  //------------------1.6 Order Actions----------------------
  
  
  def order = Action{ implicit request =>
    request.session.get("companyid").map{compid =>
    	Ok(views.html.order(Order.company_order_snapshot(compid), orderForm.fill(Order(compid,"",Order.next_order_id,10,0,0))))	
    }.getOrElse{
       Redirect(routes.Application.login)
    }
  } 
  
  def orderpart(partid: String) = Action{implicit request =>
    	request.session.get("companyid").map{compid =>
    	Ok(views.html.order(Order.company_order_snapshot(compid), orderForm.fill(Order(compid,partid,Order.next_order_id,10,0,0))))	
    	}.getOrElse{
    		Redirect(routes.Application.login)
    	}
  }
  def ordernew = Action{implicit request =>
  	orderForm.bindFromRequest.fold(
  	  error => BadRequest(views.html.order(Order.company_order_snapshot(""),error)),
  	  order => {
  	    Order.put_order(order)
  	    Redirect(routes.Application.order)  	    
  	  }
  	)
  }
  
  def orderremove(orderid:Int) = Action {implicit request => 
     request.session.get("companyid").map{compid =>       
        Order.delete_order(Order("","",orderid,0,0,0))
  	    Redirect(routes.Application.order)  
     }.getOrElse{
       Redirect(routes.Application.login)
    }
    	
    
    
  }
  
 def genpor = Action{implicit request =>
    request.session.get("companyid").map{compid =>      
     var messagepromis: Promise[String] = Akka.future{  Order.gen_por(compid) }
     Async{
       messagepromis.map(message =>           
    		   Ok(views.html.por(message,(Order.getNewOrderList).sortWith(_.days > _.days),(Order.getPorList).sortWith(_.days > _.days)))
       ) 
       
     } 
     
    }.getOrElse{
       Redirect(routes.Application.login)
    }
    
  }

/*
  def genpor = Action{implicit request =>
    request.session.get("companyid").map{compid =>
      
     var message=  Order.gen_por(compid)
     
     Ok(views.html.por(message,(Order.company_order_snapshot(compid) ++ Order.getNewOrderList).sortWith(_.days > _.days),(Order.getPorList).sortWith(_.days > _.days)))
     
    }.getOrElse{
       Redirect(routes.Application.login)
    }
    
  }
 */
  //------------------ Form mappings -----------------------
  val loginForm = Form(
      mapping (
        "Company Id" -> text,
        "Password" -> text        
      )(LoginInfo.apply)(LoginInfo.unapply)
  )
  
  
  val saveCompany = Form(
		  mapping(
				  "Company Id" -> nonEmptyText,
				  "Name" -> text,
				  "Password" -> nonEmptyText,
				  "Description" -> text
		  )(Company.apply)(Company.unapply)
  )
  
  val partForm = Form(
      mapping(
      "CompId" -> nonEmptyText,
      "PartId" -> nonEmptyText,
      "Part name" -> nonEmptyText,
      "Lead time" -> number,
      "On Hand"-> number
      )(Part.apply)(Part.unapply)
    )
    
  val bomForm = Form(
	  mapping(
		 "Company ID" -> nonEmptyText,
	     "Part ID" -> nonEmptyText,
	     "Child Part" -> nonEmptyText,
	     "Quantity"-> number
	  )(Bom.apply)(Bom.unapply)
	    
  )
  
    
  val costForm = Form(
		  mapping(
				  "Company ID" -> nonEmptyText,
				  "Part ID"		-> nonEmptyText,
				  "Function ID" -> nonEmptyText,
				  "Function" 	-> nonEmptyText
		  )(Costfunction.apply)(Costfunction.unapply)
  )
  val orderForm= Form(
		    mapping(
				  "Company ID"	-> nonEmptyText,
				  "Part ID"		-> nonEmptyText,
				  "Order Id"	-> number,
				  "Schedule ID"	-> number,
				  "Days" 		-> number,
				  "Quantity" 	-> number		      
		      )(models.Order.apply)(models.Order.unapply)
  )  
    
    
}