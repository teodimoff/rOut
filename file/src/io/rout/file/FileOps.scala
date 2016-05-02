package io.rout.file

import java.io.File

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import com.twitter.app.GlobalFlag
import com.twitter.cache.guava.GuavaCache
import com.twitter.finagle.http.Response
import com.twitter.io.{Buf, Files}
import com.twitter.util.{Future, StorageUnit}

import scala.collection.convert.wrapAll._

case class FileOpsResponse(file: Buf, contentType: String)
case class FileOpsRequest(path: String,local: String)

case class FileOps(cacheSize: StorageUnit ,localRoot: String){
  private [this] val cache: LoadingCache[FileOpsRequest, Future[Option[FileOpsResponse]]] = CacheBuilder
    .newBuilder()
    .maximumSize(cacheSize.inBytes)
    .build[FileOpsRequest,Future[Option[FileOpsResponse]]](
    new CacheLoader[FileOpsRequest,Future[Option[FileOpsResponse]]] {
    def load(req: FileOpsRequest): Future[Option[FileOpsResponse]] =  Future(read(req.path,req.local))
  })

  private [this] val fileOpsCache = GuavaCache.fromLoadingCache(cache)

  def read(str: String) =
    fileOpsCache(FileOpsRequest(str,localRoot)) map {
      case None => cache.invalidate(FileOpsRequest(str,localRoot)) ; None
      case Some(resp) => Some(resp)
  }

  private val typemap = Map(
    "css" -> "text/css",
    "js" -> "application/javascript",
    "png" -> "image/png",
    "html" -> "text/html",
    "jpg" -> "image/jpg",
    "jpeg" -> "image/jpg",
    "ico" -> "image/x-icon",
    "gif" -> "image/gif",
    "none" -> "text/plain")

  private def input(str: String) =
    if (str.endsWith("/")) None else {
      // println(str)
      Option(getClass.getResourceAsStream(str)) flatMap (i =>
        extension(str).flatMap(ct => typemap.get(ct).map(ctt =>
          FileOpsResponse(Buf.ByteArray.Owned(com.twitter.io.StreamIO.buffer(i).toByteArray), ctt))))
    }

  private def read(str: String,localRoot: String = "") = {
    // println(localRoot + str)
    if(localRoot == "") input(str)
    else {
      val file = new File(localRoot + str)
      (file.isFile, !file.isDirectory, file.canRead, file.exists()) match {
        case (true, true, true, true) => extension(str).flatMap(ct =>
          typemap.get(ct).map(ctt => FileOpsResponse(Buf.ByteArray.Owned(Files.readBytes(file)), ctt)))
        case _ => None
      }
    }
  }

  private def extension(str: String) = """(?<=\.)\w{1,5}$""".r findFirstIn str

  def static(str: String,notFound:  Future[Response]): Future[Response] =
    fileOpsCache(FileOpsRequest(str,FileOps())) flatMap  {
      case None =>
        cache.invalidate(FileOpsRequest(str,FileOps()))
        notFound
      case Some(fileResponse) =>
        val response = Response()
        response.setStatusCode(200)
        response.contentType = fileResponse.contentType
        response.content = fileResponse.file
        Future(response)
  }


  def seeAll() = {
    val c = cache.asMap()
    val response = Response()
    response.write(c.mkString("\n"))
    Future(response)
  }
}

object FileOps extends GlobalFlag[String]("","Use local file system for fileService (empty = jar) eg. " +
  "-io.rout.file.FileOps=/home/xx/xxx/"){

  def apply(size: StorageUnit): FileOps = FileOps(size,FileOps())
  //move strucutres in case calss FileOps
}

/*
object FileOps extends GlobalFlag[String]("","Use local file system for fileService (empty = jar) eg. " +
  "-io.rout.file.FileOps=/home/xx/xxx/"){

  private [this] val cache = CacheBuilder
  .newBuilder()
  .maximumSize(40.megabytes.inBytes)
  .build[(String,String),Future[Option[(Array[Byte],String)]]](new CacheLoader[(String,String),Future[Option[(Array[Byte],String)]]] {
  def load(path: (String,String)): Future[Option[(Array[Byte],String)]] =  Future(read(path._1,path._2))
  })

  private [this] val arrayCache = GuavaCache.fromLoadingCache(cache)

  def resource(str: String,localRoot: String = "") = arrayCache(str -> localRoot) map {
    case None => cache.invalidate(str -> localRoot) ; None
    case x => x
  }

  def static(str: String,notFound:  Future[Response]): Future[Response] = arrayCache(str -> FileOps()) flatMap  {
    case None => cache.invalidate(str -> FileOps()) ; notFound
    case Some(file) =>
      val response = Response()
      response.setStatusCode(200)
      response.contentType = file._2
      response.write(file._1)
      Future(response)
  }


  def seeAll() = {
    val c = cache.asMap()
    val response = Response()
    response.write(c.mkString("\n"))
    Future(response)
  }



  private val typemap = Map(
    "css" -> "text/css",
    "js" -> "application/javascript",
    "png" -> "image/png",
    "html" -> "text/html",
    "jpg" -> "image/jpg",
    "jpeg" -> "image/jpg",
    "ico" -> "image/x-icon",
    "gif" -> "image/gif",
    "none" -> "text/plain")

  private def input(str: String) =
    if (str.endsWith("/")) None else {
     // println(str)
      Option(getClass.getResourceAsStream(str)) flatMap (i =>
        extension(str).flatMap(ct => typemap.get(ct).map(ctt => com.twitter.io.StreamIO.buffer(i).toByteArray -> ctt)))
    }

  private def read(str: String,localRoot: String = "") = {
   // println(localRoot + str)
    if(localRoot == "") input(str)
    else {
    val file = new File(localRoot + str)
    (file.isFile, !file.isDirectory, file.canRead, file.exists()) match {
      case (true, true, true, true) => extension(str).flatMap(ct => typemap.get(ct).map(ctt => Files.readBytes(file) -> ctt))
      case _ => None
     }
    }
  }

  private def extension(str: String) = """(?<=\.)\w{1,5}$""".r findFirstIn str

  def apply(path: String,
            localRoot: String = FileOps()) = {
    //println(path)
    resource(path,localRoot)
  }
  //move strucutres in case calss FileOps

}
 */