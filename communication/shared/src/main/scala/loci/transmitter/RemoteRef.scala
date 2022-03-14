package loci
package transmitter

import communicator.ProtocolCommon

trait RemoteRef extends Equals {
  def identity: String
  def protocol: ProtocolCommon
  def connected: Boolean
  def disconnect(): Unit
  val disconnected: Notice.Steady[Unit]
}
