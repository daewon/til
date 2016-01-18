package io.daewon.timeserver

import io.daewon.timeclient.UnixTime
import io.netty.bootstrap.ServerBootstrap
import io.netty.buffer.ByteBuf
import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel

class TimeEncoder extends ChannelOutboundHandlerAdapter {
  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise): Unit = {
    val m: UnixTime = msg.asInstanceOf[UnixTime]
    val encoded: ByteBuf = ctx.alloc().buffer(4)
    encoded.writeInt(m.date.asInstanceOf[Int])
    println("2" * 70)
    ctx.write(encoded, promise)
  }
}

class TimeServerHandler() extends ChannelInboundHandlerAdapter {
  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    println("1" * 70)
    val future: ChannelFuture = ctx.writeAndFlush(UnixTime())
    future.addListener(ChannelFutureListener.CLOSE)
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
    cause.printStackTrace()
    ctx.close()
  }
}

object TimeServer extends App {
  val bossGroup: EventLoopGroup = new NioEventLoopGroup()
  val workerGroup: EventLoopGroup = new NioEventLoopGroup()

  try {
    val bootstrap = new ServerBootstrap()
    bootstrap.group(bossGroup, workerGroup)
      .channel(classOf[NioServerSocketChannel])
      .option(ChannelOption.SO_BACKLOG, Int.box(2048))
      .childOption(ChannelOption.SO_KEEPALIVE, Boolean.box(true))
      .childHandler(new ChannelInitializer[SocketChannel]() {
        override def initChannel(ch: SocketChannel): Unit = {
          val pipe = ch.pipeline()
          pipe.addLast(new TimeEncoder, new TimeServerHandler())
        }
      })

    println("start!")
    val bindFuture = bootstrap.bind(9000).sync()
    bindFuture.channel().closeFuture().sync()
  } finally {
    println("finish")
    workerGroup.shutdownGracefully()
    bossGroup.shutdownGracefully()
  }
}
