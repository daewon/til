package io.daewon.async.memcached

import io.daewon.async.Future

object Keys {
  val RequestKey = 0x80
  val ResponseKey = 0x81

  val Get = 0x00
  val Set = 0x01
  val Delete = 0x04
}

trait Client {
  def set(key: String, value: Array[Byte], flags: Int = 0, expiration: Int = 0): Future[StatusResponse]

  def get(key: String): Future[GetResponse]

  def delete(key: String): Future[StatusResponse]

  def connect(): Future[Client]

  def close(): Future[Client]
}

sealed abstract class ClientRequest(val code: Int)

case class SetRequest(key: String,
                      value: Array[Byte],
                      flags: Int = 0,
                      expiration: Int = 0) extends ClientRequest(Keys.Set)

case class GetRequest(key: String) extends ClientRequest(Keys.Get)

case class DeleteRequest(key: String) extends ClientRequest(Keys.Delete)

object ServerResponse {
  val Ok = 0x0000
  val NotFound = 0x0001
  val Exists = 0x0002
  val ItemNotStored = 0x0005

  val ValueTooLarge = 0x0003
  val InvalidArguments = 0x0004
  val IncrementDecrementNonNumeric = 0x0006
  val Unknown = 0x0081
  val OutOfMemory = 0x0082
}

sealed abstract class ServerResponse(val command: Int,
                                     val status: Int,
                                     val opaque: Int,
                                     val cas: Long) {

  import ServerResponse._

  def isError: Boolean = status match {
    case Ok | NotFound | Exists | ItemNotStored => false
    case _ => true
  }
}

class StatusResponse(command: Int, status: Int, opaque: Int, cas: Long, val body: Option[String] = None)
  extends ServerResponse(command, status, opaque, cas)

class GetResponse(val value: Option[Array[Byte]], status: Int, val flags: Int, opaque: Int, cas: Long)
  extends ServerResponse(Keys.Get, status, opaque, cas)