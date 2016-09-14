package io.rout.example.benchmark

import com.twitter.finagle.{Filter, Service, ServiceException}
import com.twitter.finagle.http.{Request, Response}
import com.twitter.util.Future
import io.rout.ReqExt

import scala.collection.mutable
import scala.util.Random

case class Passport(name: String,age: Int,password: String)

case class AuthResult(resultCode: AuthResultCode.Value,passport: Option[Passport])

case class RequestUnauthenticated(authResultCode: AuthResultCode.Value) extends ServiceException

object AuthResultCode extends Enumeration {
  val OK,Fail = Value
}

case class AuthedReq(request: Request,passport: Passport)

object AuthFilter {
  val auth = Filter.mk[Request,Response,AuthedReq,Response]{(request,service) =>
    AuthService.authService(request).flatMap {
      case AuthResult(AuthResultCode.OK, Some(passport)) => service(AuthedReq(request,passport))
      case authResult: AuthResult =>
        Future.exception(new RequestUnauthenticated(authResult.resultCode))
      }
    }
}

object AuthService {
  val authService = Service.mk[Request,AuthResult] { req =>
    req.cookies.get("password").flatMap(pass => PassportDatabase.get(pass.value)) match {
      case Some(passport) => Future(AuthResult(AuthResultCode.OK,Some(passport)))
      case None => Future(AuthResult(AuthResultCode.Fail,None))
    }
  }
}

object PassportDatabase {
  private[this] val db: mutable.Map[String, Passport] = mutable.Map.empty[String, Passport]

  def get(password: String): Option[Passport] = synchronized { db.get(password) }
  def list(): List[Passport] = synchronized { db.values.toList }
  def save(t: Passport): Unit = synchronized { db += (t.password -> t) }
  def delete(password: String): Unit = synchronized { db -= password }

  save(Passport(Random.nextString(5),Random.nextInt(30),"1d2a409b"))
}