package io.daewon.timeclient

import java.util
import java.util.Date

import io.netty.bootstrap.Bootstrap
import io.netty.buffer.ByteBuf
import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.ByteToMessageDecoder

// http://ikpil.com/1338
case class UnixTime(value: Long = System.currentTimeMillis()) {
  val date = (value - 2208988800L) * 1000L

  override def toString() = new Date(date).toString
}

class TimeDecoder extends ByteToMessageDecoder {
  override def decode(ctx: ChannelHandlerContext, in: ByteBuf, out: util.List[AnyRef]): Unit = {
    println("1" * 70)
    if (in.readableBytes() >= 4) out.add(UnixTime(in.readUnsignedInt()))
  }
}

class TimeClientHandler extends ChannelInboundHandlerAdapter {
  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = {
    val ut = msg.asInstanceOf[UnixTime]
    println("2" * 70)
    println(ut.toString())
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
    cause.printStackTrace()
    ctx.close()
  }
}

object TimeClient extends App {
  val workerGroup: EventLoopGroup = new NioEventLoopGroup()

  try {
    val bootstrap = new Bootstrap()
    bootstrap.group(workerGroup)
      .channel(classOf[NioSocketChannel])
      .option(ChannelOption.SO_KEEPALIVE, Boolean.box(true))
      .handler(new ChannelInitializer[SocketChannel] {
        override def initChannel(ch: SocketChannel): Unit = {
          val pipe = ch.pipeline()
          pipe.addLast(new TimeDecoder(), new TimeClientHandler)
        }
      })

    println("start!")
    val bindFuture = bootstrap.connect("localhost", 9000)
    bindFuture.channel().closeFuture().sync()
  } finally {
    println("finish")
    workerGroup.shutdownGracefully()
  }
}
