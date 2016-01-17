package memcached

import io.netty.channel.ChannelFuture

class FailedFutureException(val channelFuture: ChannelFuture) extends IllegalStateException
