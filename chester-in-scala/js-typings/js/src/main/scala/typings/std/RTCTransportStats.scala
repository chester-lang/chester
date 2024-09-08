package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait RTCTransportStats
  extends StObject
     with RTCStats {
  
  /* standard dom */
  var bytesReceived: js.UndefOr[Double] = js.undefined
  
  /* standard dom */
  var bytesSent: js.UndefOr[Double] = js.undefined
  
  /* standard dom */
  var dtlsCipher: js.UndefOr[java.lang.String] = js.undefined
  
  /* standard dom */
  var dtlsState: js.UndefOr[RTCDtlsTransportState] = js.undefined
  
  /* standard dom */
  var iceRole: js.UndefOr[RTCIceRole] = js.undefined
  
  /* standard dom */
  var localCertificateId: js.UndefOr[java.lang.String] = js.undefined
  
  /* standard dom */
  var packetsReceived: js.UndefOr[Double] = js.undefined
  
  /* standard dom */
  var packetsSent: js.UndefOr[Double] = js.undefined
  
  /* standard dom */
  var remoteCertificateId: js.UndefOr[java.lang.String] = js.undefined
  
  /* standard dom */
  var rtcpTransportStatsId: js.UndefOr[java.lang.String] = js.undefined
  
  /* standard dom */
  var selectedCandidatePairChanges: js.UndefOr[Double] = js.undefined
  
  /* standard dom */
  var selectedCandidatePairId: js.UndefOr[java.lang.String] = js.undefined
  
  /* standard dom */
  var srtpCipher: js.UndefOr[java.lang.String] = js.undefined
  
  /* standard dom */
  var tlsGroup: js.UndefOr[java.lang.String] = js.undefined
  
  /* standard dom */
  var tlsVersion: js.UndefOr[java.lang.String] = js.undefined
}
object RTCTransportStats {
  
  inline def apply(): RTCTransportStats = {
    val __obj = js.Dynamic.literal()
    __obj.asInstanceOf[RTCTransportStats]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: RTCTransportStats] (val x: Self) extends AnyVal {
    
    inline def setBytesReceived(value: Double): Self = StObject.set(x, "bytesReceived", value.asInstanceOf[js.Any])
    
    inline def setBytesReceivedUndefined: Self = StObject.set(x, "bytesReceived", js.undefined)
    
    inline def setBytesSent(value: Double): Self = StObject.set(x, "bytesSent", value.asInstanceOf[js.Any])
    
    inline def setBytesSentUndefined: Self = StObject.set(x, "bytesSent", js.undefined)
    
    inline def setDtlsCipher(value: java.lang.String): Self = StObject.set(x, "dtlsCipher", value.asInstanceOf[js.Any])
    
    inline def setDtlsCipherUndefined: Self = StObject.set(x, "dtlsCipher", js.undefined)
    
    inline def setDtlsState(value: RTCDtlsTransportState): Self = StObject.set(x, "dtlsState", value.asInstanceOf[js.Any])
    
    inline def setDtlsStateUndefined: Self = StObject.set(x, "dtlsState", js.undefined)
    
    inline def setIceRole(value: RTCIceRole): Self = StObject.set(x, "iceRole", value.asInstanceOf[js.Any])
    
    inline def setIceRoleUndefined: Self = StObject.set(x, "iceRole", js.undefined)
    
    inline def setLocalCertificateId(value: java.lang.String): Self = StObject.set(x, "localCertificateId", value.asInstanceOf[js.Any])
    
    inline def setLocalCertificateIdUndefined: Self = StObject.set(x, "localCertificateId", js.undefined)
    
    inline def setPacketsReceived(value: Double): Self = StObject.set(x, "packetsReceived", value.asInstanceOf[js.Any])
    
    inline def setPacketsReceivedUndefined: Self = StObject.set(x, "packetsReceived", js.undefined)
    
    inline def setPacketsSent(value: Double): Self = StObject.set(x, "packetsSent", value.asInstanceOf[js.Any])
    
    inline def setPacketsSentUndefined: Self = StObject.set(x, "packetsSent", js.undefined)
    
    inline def setRemoteCertificateId(value: java.lang.String): Self = StObject.set(x, "remoteCertificateId", value.asInstanceOf[js.Any])
    
    inline def setRemoteCertificateIdUndefined: Self = StObject.set(x, "remoteCertificateId", js.undefined)
    
    inline def setRtcpTransportStatsId(value: java.lang.String): Self = StObject.set(x, "rtcpTransportStatsId", value.asInstanceOf[js.Any])
    
    inline def setRtcpTransportStatsIdUndefined: Self = StObject.set(x, "rtcpTransportStatsId", js.undefined)
    
    inline def setSelectedCandidatePairChanges(value: Double): Self = StObject.set(x, "selectedCandidatePairChanges", value.asInstanceOf[js.Any])
    
    inline def setSelectedCandidatePairChangesUndefined: Self = StObject.set(x, "selectedCandidatePairChanges", js.undefined)
    
    inline def setSelectedCandidatePairId(value: java.lang.String): Self = StObject.set(x, "selectedCandidatePairId", value.asInstanceOf[js.Any])
    
    inline def setSelectedCandidatePairIdUndefined: Self = StObject.set(x, "selectedCandidatePairId", js.undefined)
    
    inline def setSrtpCipher(value: java.lang.String): Self = StObject.set(x, "srtpCipher", value.asInstanceOf[js.Any])
    
    inline def setSrtpCipherUndefined: Self = StObject.set(x, "srtpCipher", js.undefined)
    
    inline def setTlsGroup(value: java.lang.String): Self = StObject.set(x, "tlsGroup", value.asInstanceOf[js.Any])
    
    inline def setTlsGroupUndefined: Self = StObject.set(x, "tlsGroup", js.undefined)
    
    inline def setTlsVersion(value: java.lang.String): Self = StObject.set(x, "tlsVersion", value.asInstanceOf[js.Any])
    
    inline def setTlsVersionUndefined: Self = StObject.set(x, "tlsVersion", js.undefined)
  }
}
