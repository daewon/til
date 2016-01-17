package memcached

class BusyClientException
  extends IllegalStateException("This client already has pending requests, you have to wait until they are finished")
