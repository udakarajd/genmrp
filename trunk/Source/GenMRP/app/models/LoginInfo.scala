package models

case class LoginInfo (userid : String , password : String)

object LoginInfo{
  def validate(logininfo:LoginInfo):Boolean= {
    Company.find(Company(logininfo.userid,"",logininfo.password,"")).length>0}
}