package memcached

import io.netty.channel.{ChannelFuture, ChannelFutureListener}

import scala.concurrent.{Future, Promise}

object ChannelFutureTransformer {

  implicit def toFuture(channelFuture: ChannelFuture): Future[ChannelFuture] = {
    val promise = Promise[ChannelFuture]

    channelFuture.addListener(new ChannelFutureListener {
      override def operationComplete(future: ChannelFuture) {
        if (future.isSuccess) {
          promise.success(future)
        } else {
          if (future.cause() != null) {
            promise.failure(future.cause())
          } else {
            val exception = new FailedFutureException(channelFuture)
            exception.fillInStackTrace()
            promise.failure(exception)
          }
        }
      }
    })

    promise.future
  }

}