import io.netty.bootstrap.Bootstrap
import io.netty.buffer.ByteBuf
import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel

class TimeClientHandler extends ChannelInboundHandlerAdapter {
  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = {
    val buf: ByteBuf = msg.asInstanceOf[ByteBuf]

    try {
      val ts = (buf.readUnsignedInt() - 2208988800L)
      println(ts * 1000)
      ctx.close()
    } finally {
      buf.release()
    }
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
          pipe.addLast(new TimeClientHandler())
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
