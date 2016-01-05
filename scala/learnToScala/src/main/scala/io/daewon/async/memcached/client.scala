package io.daewon.async.memcached

import io.daewon.async.Future

trait Client {
  def set(key: String, value: Array[Byte], flags: Int = 0, expiration: Int = 0): Future[StatusResponse]

  def get(key: String): Future[GetResponse]

  def delete(key: String): Future[StatusResponse]

  def connect(): Future[Client]

  def close(): Future[Client]

}

