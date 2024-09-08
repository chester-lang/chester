package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

/** SVGTransform is the interface for one of the component transformations within an SVGTransformList; thus, an SVGTransform object corresponds to a single component (e.g., scale(…) or matrix(…)) within a transform attribute. */
trait SVGTransform extends StObject {
  
  /* standard dom */
  val SVG_TRANSFORM_MATRIX: Double
  
  /* standard dom */
  val SVG_TRANSFORM_ROTATE: Double
  
  /* standard dom */
  val SVG_TRANSFORM_SCALE: Double
  
  /* standard dom */
  val SVG_TRANSFORM_SKEWX: Double
  
  /* standard dom */
  val SVG_TRANSFORM_SKEWY: Double
  
  /* standard dom */
  val SVG_TRANSFORM_TRANSLATE: Double
  
  /* standard dom */
  val SVG_TRANSFORM_UNKNOWN: Double
  
  /* standard dom */
  val angle: Double
  
  /* standard dom */
  val matrix: org.scalajs.dom.SVGMatrix
  
  /* standard dom */
  def setMatrix(matrix: org.scalajs.dom.SVGMatrix): Unit
  
  /* standard dom */
  def setRotate(angle: Double, cx: Double, cy: Double): Unit
  
  /* standard dom */
  def setScale(sx: Double, sy: Double): Unit
  
  /* standard dom */
  def setSkewX(angle: Double): Unit
  
  /* standard dom */
  def setSkewY(angle: Double): Unit
  
  /* standard dom */
  def setTranslate(tx: Double, ty: Double): Unit
  
  /* standard dom */
  val `type`: Double
}
object SVGTransform {
  
  inline def apply(
    SVG_TRANSFORM_MATRIX: Double,
    SVG_TRANSFORM_ROTATE: Double,
    SVG_TRANSFORM_SCALE: Double,
    SVG_TRANSFORM_SKEWX: Double,
    SVG_TRANSFORM_SKEWY: Double,
    SVG_TRANSFORM_TRANSLATE: Double,
    SVG_TRANSFORM_UNKNOWN: Double,
    angle: Double,
    matrix: org.scalajs.dom.SVGMatrix,
    setMatrix: org.scalajs.dom.SVGMatrix => Unit,
    setRotate: (Double, Double, Double) => Unit,
    setScale: (Double, Double) => Unit,
    setSkewX: Double => Unit,
    setSkewY: Double => Unit,
    setTranslate: (Double, Double) => Unit,
    `type`: Double
  ): SVGTransform = {
    val __obj = js.Dynamic.literal(SVG_TRANSFORM_MATRIX = SVG_TRANSFORM_MATRIX.asInstanceOf[js.Any], SVG_TRANSFORM_ROTATE = SVG_TRANSFORM_ROTATE.asInstanceOf[js.Any], SVG_TRANSFORM_SCALE = SVG_TRANSFORM_SCALE.asInstanceOf[js.Any], SVG_TRANSFORM_SKEWX = SVG_TRANSFORM_SKEWX.asInstanceOf[js.Any], SVG_TRANSFORM_SKEWY = SVG_TRANSFORM_SKEWY.asInstanceOf[js.Any], SVG_TRANSFORM_TRANSLATE = SVG_TRANSFORM_TRANSLATE.asInstanceOf[js.Any], SVG_TRANSFORM_UNKNOWN = SVG_TRANSFORM_UNKNOWN.asInstanceOf[js.Any], angle = angle.asInstanceOf[js.Any], matrix = matrix.asInstanceOf[js.Any], setMatrix = js.Any.fromFunction1(setMatrix), setRotate = js.Any.fromFunction3(setRotate), setScale = js.Any.fromFunction2(setScale), setSkewX = js.Any.fromFunction1(setSkewX), setSkewY = js.Any.fromFunction1(setSkewY), setTranslate = js.Any.fromFunction2(setTranslate))
    __obj.updateDynamic("type")(`type`.asInstanceOf[js.Any])
    __obj.asInstanceOf[SVGTransform]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: SVGTransform] (val x: Self) extends AnyVal {
    
    inline def setAngle(value: Double): Self = StObject.set(x, "angle", value.asInstanceOf[js.Any])
    
    inline def setMatrix(value: org.scalajs.dom.SVGMatrix): Self = StObject.set(x, "matrix", value.asInstanceOf[js.Any])
    
    inline def setSVG_TRANSFORM_MATRIX(value: Double): Self = StObject.set(x, "SVG_TRANSFORM_MATRIX", value.asInstanceOf[js.Any])
    
    inline def setSVG_TRANSFORM_ROTATE(value: Double): Self = StObject.set(x, "SVG_TRANSFORM_ROTATE", value.asInstanceOf[js.Any])
    
    inline def setSVG_TRANSFORM_SCALE(value: Double): Self = StObject.set(x, "SVG_TRANSFORM_SCALE", value.asInstanceOf[js.Any])
    
    inline def setSVG_TRANSFORM_SKEWX(value: Double): Self = StObject.set(x, "SVG_TRANSFORM_SKEWX", value.asInstanceOf[js.Any])
    
    inline def setSVG_TRANSFORM_SKEWY(value: Double): Self = StObject.set(x, "SVG_TRANSFORM_SKEWY", value.asInstanceOf[js.Any])
    
    inline def setSVG_TRANSFORM_TRANSLATE(value: Double): Self = StObject.set(x, "SVG_TRANSFORM_TRANSLATE", value.asInstanceOf[js.Any])
    
    inline def setSVG_TRANSFORM_UNKNOWN(value: Double): Self = StObject.set(x, "SVG_TRANSFORM_UNKNOWN", value.asInstanceOf[js.Any])
    
    inline def setSetMatrix(value: org.scalajs.dom.SVGMatrix => Unit): Self = StObject.set(x, "setMatrix", js.Any.fromFunction1(value))
    
    inline def setSetRotate(value: (Double, Double, Double) => Unit): Self = StObject.set(x, "setRotate", js.Any.fromFunction3(value))
    
    inline def setSetScale(value: (Double, Double) => Unit): Self = StObject.set(x, "setScale", js.Any.fromFunction2(value))
    
    inline def setSetSkewX(value: Double => Unit): Self = StObject.set(x, "setSkewX", js.Any.fromFunction1(value))
    
    inline def setSetSkewY(value: Double => Unit): Self = StObject.set(x, "setSkewY", js.Any.fromFunction1(value))
    
    inline def setSetTranslate(value: (Double, Double) => Unit): Self = StObject.set(x, "setTranslate", js.Any.fromFunction2(value))
    
    inline def setType(value: Double): Self = StObject.set(x, "type", value.asInstanceOf[js.Any])
  }
}