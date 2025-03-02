package loci
package communicator
package webrtc

import org.scalajs.dom

protected[webrtc] trait WebRTCUpdate {
  sealed abstract class Update
  sealed abstract class IncrementalUpdate extends Update
  sealed abstract class CompleteUpdate extends Update

  sealed case class InitialSession(
    descType: String, sdp: String)
      extends IncrementalUpdate {
    def sessionDescription = new dom.RTCSessionDescription(
      new dom.RTCSessionDescriptionInit {
        `type` = InitialSession.this.descType.asInstanceOf[dom.RTCSdpType]
        sdp = InitialSession.this.sdp
      })
  }

  object InitialSession {
    def apply(value: dom.RTCSessionDescription): InitialSession =
      InitialSession(value.`type`.asInstanceOf[String], value.sdp)
  }

  sealed case class SessionUpdate(
    candidate: String, sdpMid: String, sdpMLineIndex: Double)
      extends IncrementalUpdate {
    def iceCandidate = new dom.RTCIceCandidate(
      new dom.RTCIceCandidateInit {
        candidate = SessionUpdate.this.candidate
        sdpMid = SessionUpdate.this.sdpMid
        sdpMLineIndex = SessionUpdate.this.sdpMLineIndex
      })
  }

  object SessionUpdate {
    def apply(value: dom.RTCIceCandidate): SessionUpdate =
      SessionUpdate(value.candidate, value.sdpMid, value.sdpMLineIndex)
  }

  sealed case class CompleteSession(
    descType: String, sdp: String)
      extends CompleteUpdate {
    def sessionDescription = new dom.RTCSessionDescription(
      new dom.RTCSessionDescriptionInit {
        `type` = CompleteSession.this.descType.asInstanceOf[dom.RTCSdpType]
        sdp = CompleteSession.this.sdp
      })
  }

  object CompleteSession {
    def apply(value: dom.RTCSessionDescription): CompleteSession =
      CompleteSession(value.`type`.asInstanceOf[String], value.sdp)
  }
}
