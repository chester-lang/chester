package typings.std.global

import org.scalajs.dom.EventListenerOptions
import typings.std.AddEventListenerOptions
import typings.std.EventListenerOrEventListenerObject
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

/* This class was inferred from a value with a constructor. In rare cases (like HTMLElement in the DOM) it might not work as you expect. */
@JSGlobal("SVGSVGElement")
@js.native
/* standard dom */
open class SVGSVGElement ()
  extends StObject
     with typings.std.SVGSVGElement {
  
  /* standard dom */
  /* CompleteClass */
  override val SVG_ZOOMANDPAN_DISABLE: Double = js.native
  
  /* standard dom */
  /* CompleteClass */
  override val SVG_ZOOMANDPAN_MAGNIFY: Double = js.native
  
  /* standard dom */
  /* CompleteClass */
  override val SVG_ZOOMANDPAN_UNKNOWN: Double = js.native
  
  /* InferMemberOverrides */
  override def addEventListener(`type`: java.lang.String, listener: EventListenerOrEventListenerObject): Unit = js.native
  /* InferMemberOverrides */
  override def addEventListener(`type`: java.lang.String, listener: EventListenerOrEventListenerObject, options: scala.Boolean): Unit = js.native
  /* InferMemberOverrides */
  override def addEventListener(
    `type`: java.lang.String,
    listener: EventListenerOrEventListenerObject,
    options: AddEventListenerOptions
  ): Unit = js.native
  
  /* standard dom */
  /* CompleteClass */
  override val assignedSlot: typings.std.HTMLSlotElement | Null = js.native
  
  /* standard dom */
  /* CompleteClass */
  var innerHTML: java.lang.String = js.native
  
  /**
    * Returns the first following sibling that is an element, and null otherwise.
    */
  /* standard dom */
  /* CompleteClass */
  override val nextElementSibling: org.scalajs.dom.Element | Null = js.native
  
  /* standard dom */
  /* CompleteClass */
  override val preserveAspectRatio: org.scalajs.dom.SVGAnimatedPreserveAspectRatio = js.native
  
  /**
    * Returns the first preceding sibling that is an element, and null otherwise.
    */
  /* standard dom */
  /* CompleteClass */
  override val previousElementSibling: org.scalajs.dom.Element | Null = js.native
  
  /* InferMemberOverrides */
  override def removeEventListener(`type`: java.lang.String, callback: EventListenerOrEventListenerObject): Unit = js.native
  /* InferMemberOverrides */
  override def removeEventListener(
    `type`: java.lang.String,
    callback: EventListenerOrEventListenerObject,
    options: EventListenerOptions
  ): Unit = js.native
  /* InferMemberOverrides */
  override def removeEventListener(`type`: java.lang.String, callback: EventListenerOrEventListenerObject, options: scala.Boolean): Unit = js.native
  
  /* standard dom */
  /* CompleteClass */
  override val requiredExtensions: org.scalajs.dom.SVGStringList = js.native
  
  /* standard dom */
  /* CompleteClass */
  override val style: org.scalajs.dom.CSSStyleDeclaration = js.native
  
  /* standard dom */
  /* CompleteClass */
  override val systemLanguage: org.scalajs.dom.SVGStringList = js.native
  
  /* standard dom */
  /* CompleteClass */
  override val viewBox: org.scalajs.dom.SVGAnimatedRect = js.native
  
  /* standard dom */
  /* CompleteClass */
  var zoomAndPan: Double = js.native
}
object SVGSVGElement {
  
  /* standard dom */
  @JSGlobal("SVGSVGElement.SVG_ZOOMANDPAN_DISABLE")
  @js.native
  val SVG_ZOOMANDPAN_DISABLE: Double = js.native
  
  /* standard dom */
  @JSGlobal("SVGSVGElement.SVG_ZOOMANDPAN_MAGNIFY")
  @js.native
  val SVG_ZOOMANDPAN_MAGNIFY: Double = js.native
  
  /* standard dom */
  @JSGlobal("SVGSVGElement.SVG_ZOOMANDPAN_UNKNOWN")
  @js.native
  val SVG_ZOOMANDPAN_UNKNOWN: Double = js.native
}