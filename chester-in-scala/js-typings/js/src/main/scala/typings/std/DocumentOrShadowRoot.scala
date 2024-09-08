package typings.std

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

trait DocumentOrShadowRoot extends StObject {
  
  /* standard dom */
  val activeElement: org.scalajs.dom.Element | Null
  
  /* standard dom */
  def caretPositionFromPoint(x: Double, y: Double): CaretPosition | Null
  
  /** @deprecated */
  /* standard dom */
  def caretRangeFromPoint(x: Double, y: Double): org.scalajs.dom.Range
  
  /* standard dom */
  def elementFromPoint(x: Double, y: Double): org.scalajs.dom.Element | Null
  
  /* standard dom */
  def elementsFromPoint(x: Double, y: Double): js.Array[org.scalajs.dom.Element]
  
  /**
    * Returns document's fullscreen element.
    */
  /* standard dom */
  val fullscreenElement: org.scalajs.dom.Element | Null
  
  /* standard dom */
  def getSelection(): org.scalajs.dom.Selection | Null
  
  /* standard dom */
  val pointerLockElement: org.scalajs.dom.Element | Null
  
  /**
    * Retrieves a collection of styleSheet objects representing the style sheets that correspond to each instance of a link or style object in the document.
    */
  /* standard dom */
  val styleSheets: org.scalajs.dom.StyleSheetList
}
object DocumentOrShadowRoot {
  
  inline def apply(
    caretPositionFromPoint: (Double, Double) => CaretPosition | Null,
    caretRangeFromPoint: (Double, Double) => org.scalajs.dom.Range,
    elementFromPoint: (Double, Double) => org.scalajs.dom.Element | Null,
    elementsFromPoint: (Double, Double) => js.Array[org.scalajs.dom.Element],
    getSelection: () => org.scalajs.dom.Selection | Null,
    styleSheets: org.scalajs.dom.StyleSheetList
  ): DocumentOrShadowRoot = {
    val __obj = js.Dynamic.literal(caretPositionFromPoint = js.Any.fromFunction2(caretPositionFromPoint), caretRangeFromPoint = js.Any.fromFunction2(caretRangeFromPoint), elementFromPoint = js.Any.fromFunction2(elementFromPoint), elementsFromPoint = js.Any.fromFunction2(elementsFromPoint), getSelection = js.Any.fromFunction0(getSelection), styleSheets = styleSheets.asInstanceOf[js.Any], activeElement = null, fullscreenElement = null, pointerLockElement = null)
    __obj.asInstanceOf[DocumentOrShadowRoot]
  }
  
  @scala.inline
  implicit open class MutableBuilder[Self <: DocumentOrShadowRoot] (val x: Self) extends AnyVal {
    
    inline def setActiveElement(value: org.scalajs.dom.Element): Self = StObject.set(x, "activeElement", value.asInstanceOf[js.Any])
    
    inline def setActiveElementNull: Self = StObject.set(x, "activeElement", null)
    
    inline def setCaretPositionFromPoint(value: (Double, Double) => CaretPosition | Null): Self = StObject.set(x, "caretPositionFromPoint", js.Any.fromFunction2(value))
    
    inline def setCaretRangeFromPoint(value: (Double, Double) => org.scalajs.dom.Range): Self = StObject.set(x, "caretRangeFromPoint", js.Any.fromFunction2(value))
    
    inline def setElementFromPoint(value: (Double, Double) => org.scalajs.dom.Element | Null): Self = StObject.set(x, "elementFromPoint", js.Any.fromFunction2(value))
    
    inline def setElementsFromPoint(value: (Double, Double) => js.Array[org.scalajs.dom.Element]): Self = StObject.set(x, "elementsFromPoint", js.Any.fromFunction2(value))
    
    inline def setFullscreenElement(value: org.scalajs.dom.Element): Self = StObject.set(x, "fullscreenElement", value.asInstanceOf[js.Any])
    
    inline def setFullscreenElementNull: Self = StObject.set(x, "fullscreenElement", null)
    
    inline def setGetSelection(value: () => org.scalajs.dom.Selection | Null): Self = StObject.set(x, "getSelection", js.Any.fromFunction0(value))
    
    inline def setPointerLockElement(value: org.scalajs.dom.Element): Self = StObject.set(x, "pointerLockElement", value.asInstanceOf[js.Any])
    
    inline def setPointerLockElementNull: Self = StObject.set(x, "pointerLockElement", null)
    
    inline def setStyleSheets(value: org.scalajs.dom.StyleSheetList): Self = StObject.set(x, "styleSheets", value.asInstanceOf[js.Any])
  }
}
