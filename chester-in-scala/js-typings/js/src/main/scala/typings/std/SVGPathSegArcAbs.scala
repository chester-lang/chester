package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait SVGPathSegArcAbs
  extends StObject
     with SVGPathSeg {
  
  /* standard dom */
  var angle: Double
  
  /* standard dom */
  var largeArcFlag: scala.Boolean
  
  /* standard dom */
  var r1: Double
  
  /* standard dom */
  var r2: Double
  
  /* standard dom */
  var sweepFlag: scala.Boolean
  
  /* standard dom */
  var x: Double
  
  /* standard dom */
  var y: Double
}
object SVGPathSegArcAbs {
  
  inline def apply(
    PATHSEG_ARC_ABS: Double,
    PATHSEG_ARC_REL: Double,
    PATHSEG_CLOSEPATH: Double,
    PATHSEG_CURVETO_CUBIC_ABS: Double,
    PATHSEG_CURVETO_CUBIC_REL: Double,
    PATHSEG_CURVETO_CUBIC_SMOOTH_ABS: Double,
    PATHSEG_CURVETO_CUBIC_SMOOTH_REL: Double,
    PATHSEG_CURVETO_QUADRATIC_ABS: Double,
    PATHSEG_CURVETO_QUADRATIC_REL: Double,
    PATHSEG_CURVETO_QUADRATIC_SMOOTH_ABS: Double,
    PATHSEG_CURVETO_QUADRATIC_SMOOTH_REL: Double,
    PATHSEG_LINETO_ABS: Double,
    PATHSEG_LINETO_HORIZONTAL_ABS: Double,
    PATHSEG_LINETO_HORIZONTAL_REL: Double,
    PATHSEG_LINETO_REL: Double,
    PATHSEG_LINETO_VERTICAL_ABS: Double,
    PATHSEG_LINETO_VERTICAL_REL: Double,
    PATHSEG_MOVETO_ABS: Double,
    PATHSEG_MOVETO_REL: Double,
    PATHSEG_UNKNOWN: Double,
    angle: Double,
    largeArcFlag: scala.Boolean,
    pathSegType: Double,
    pathSegTypeAsLetter: java.lang.String,
    r1: Double,
    r2: Double,
    sweepFlag: scala.Boolean,
    x: Double,
    y: Double
  ): SVGPathSegArcAbs = {
    val __obj = js.Dynamic.literal(PATHSEG_ARC_ABS = PATHSEG_ARC_ABS.asInstanceOf[js.Any], PATHSEG_ARC_REL = PATHSEG_ARC_REL.asInstanceOf[js.Any], PATHSEG_CLOSEPATH = PATHSEG_CLOSEPATH.asInstanceOf[js.Any], PATHSEG_CURVETO_CUBIC_ABS = PATHSEG_CURVETO_CUBIC_ABS.asInstanceOf[js.Any], PATHSEG_CURVETO_CUBIC_REL = PATHSEG_CURVETO_CUBIC_REL.asInstanceOf[js.Any], PATHSEG_CURVETO_CUBIC_SMOOTH_ABS = PATHSEG_CURVETO_CUBIC_SMOOTH_ABS.asInstanceOf[js.Any], PATHSEG_CURVETO_CUBIC_SMOOTH_REL = PATHSEG_CURVETO_CUBIC_SMOOTH_REL.asInstanceOf[js.Any], PATHSEG_CURVETO_QUADRATIC_ABS = PATHSEG_CURVETO_QUADRATIC_ABS.asInstanceOf[js.Any], PATHSEG_CURVETO_QUADRATIC_REL = PATHSEG_CURVETO_QUADRATIC_REL.asInstanceOf[js.Any], PATHSEG_CURVETO_QUADRATIC_SMOOTH_ABS = PATHSEG_CURVETO_QUADRATIC_SMOOTH_ABS.asInstanceOf[js.Any], PATHSEG_CURVETO_QUADRATIC_SMOOTH_REL = PATHSEG_CURVETO_QUADRATIC_SMOOTH_REL.asInstanceOf[js.Any], PATHSEG_LINETO_ABS = PATHSEG_LINETO_ABS.asInstanceOf[js.Any], PATHSEG_LINETO_HORIZONTAL_ABS = PATHSEG_LINETO_HORIZONTAL_ABS.asInstanceOf[js.Any], PATHSEG_LINETO_HORIZONTAL_REL = PATHSEG_LINETO_HORIZONTAL_REL.asInstanceOf[js.Any], PATHSEG_LINETO_REL = PATHSEG_LINETO_REL.asInstanceOf[js.Any], PATHSEG_LINETO_VERTICAL_ABS = PATHSEG_LINETO_VERTICAL_ABS.asInstanceOf[js.Any], PATHSEG_LINETO_VERTICAL_REL = PATHSEG_LINETO_VERTICAL_REL.asInstanceOf[js.Any], PATHSEG_MOVETO_ABS = PATHSEG_MOVETO_ABS.asInstanceOf[js.Any], PATHSEG_MOVETO_REL = PATHSEG_MOVETO_REL.asInstanceOf[js.Any], PATHSEG_UNKNOWN = PATHSEG_UNKNOWN.asInstanceOf[js.Any], angle = angle.asInstanceOf[js.Any], largeArcFlag = largeArcFlag.asInstanceOf[js.Any], pathSegType = pathSegType.asInstanceOf[js.Any], pathSegTypeAsLetter = pathSegTypeAsLetter.asInstanceOf[js.Any], r1 = r1.asInstanceOf[js.Any], r2 = r2.asInstanceOf[js.Any], sweepFlag = sweepFlag.asInstanceOf[js.Any], x = x.asInstanceOf[js.Any], y = y.asInstanceOf[js.Any])
    __obj.asInstanceOf[SVGPathSegArcAbs]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: SVGPathSegArcAbs] (val x: Self) extends AnyVal {
    
    inline def setAngle(value: Double): Self = StObject.set(x, "angle", value.asInstanceOf[js.Any])
    
    inline def setLargeArcFlag(value: scala.Boolean): Self = StObject.set(x, "largeArcFlag", value.asInstanceOf[js.Any])
    
    inline def setR1(value: Double): Self = StObject.set(x, "r1", value.asInstanceOf[js.Any])
    
    inline def setR2(value: Double): Self = StObject.set(x, "r2", value.asInstanceOf[js.Any])
    
    inline def setSweepFlag(value: scala.Boolean): Self = StObject.set(x, "sweepFlag", value.asInstanceOf[js.Any])
    
    inline def setX(value: Double): Self = StObject.set(x, "x", value.asInstanceOf[js.Any])
    
    inline def setY(value: Double): Self = StObject.set(x, "y", value.asInstanceOf[js.Any])
  }
}