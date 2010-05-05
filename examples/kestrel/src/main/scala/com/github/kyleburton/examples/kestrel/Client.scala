// vim: set ts=2 sw=2 et:
package com.github.kyleburton.examples.kestrel

import net.lag.smile.MemcacheClient

object Main {
  def main ( args : Array[String] ) {
    val host = "localhost"
    val queueName = "items-queue"
    val hashingMethod = "crc32-itu"
    println("Connect to the client...at %s".format(host))
    val distribution = "default"

    val client = MemcacheClient.create(Array(host), distribution, hashingMethod)

    0 until 100 foreach { (idx) =>
      val item = "%d:%s".format(idx, new java.util.Date())
      val result = client.set(queueName, item)
      println("added item to queue[%s]: %s res=>'%s'".format(queueName, item, result))
    }

    var count = 1
    var item : Option[String] = client.get(queueName)
    while ( None != item ) {
      println("%s[%d]: '%s'".format(queueName, count, item.get))
      item = client.get(queueName)
      count += 1
    }

    println("exhausted the queue, got %d items".format(count))
    client.shutdown()
  }
}

