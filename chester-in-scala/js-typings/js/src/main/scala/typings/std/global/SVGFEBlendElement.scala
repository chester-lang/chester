package typings.std.global

import org.scalajs.dom.EventListenerOptions
import typings.std.AddEventListenerOptions
import typings.std.EventListenerOrEventListenerObject
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

/* This class was inferred from a value with a constructor. In rare cases (like HTMLElement in the DOM) it might not work as you expect. */
@JSGlobal("SVGFEBlendElement")
@js.native
/* standard dom */
open class SVGFEBlendElement ()
  extends StObject
     with typings.std.SVGFEBlendElement {
  
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
  override val height: org.scalajs.dom.SVGAnimatedLength = js.native
  
  /* standard dom */
  /* CompleteClass */
  var innerHTML: java.lang.String = js.native
  
  /**
    * Returns the first following sibling that is an element, and null otherwise.
    */
  /* standard dom */
  /* CompleteClass */
  override val nextElementSibling: org.scalajs.dom.Element | Null = js.native
  
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
  override val result: org.scalajs.dom.SVGAnimatedString = js.native
  
  /* standard dom */
  /* CompleteClass */
  override val style: org.scalajs.dom.CSSStyleDeclaration = js.native
  
  /* standard dom */
  /* CompleteClass */
  override val width: org.scalajs.dom.SVGAnimatedLength = js.native
  
  /* standard dom */
  /* CompleteClass */
  override val x: org.scalajs.dom.SVGAnimatedLength = js.native
  
  /* standard dom */
  /* CompleteClass */
  override val y: org.scalajs.dom.SVGAnimatedLength = js.native
}
object SVGFEBlendElement {
  
  /* standard dom */
  @JSGlobal("SVGFEBlendElement.SVG_FEBLEND_MODE_COLOR")
  @js.native
  val SVG_FEBLEND_MODE_COLOR: Double = js.native
  
  /* standard dom */
  @JSGlobal("SVGFEBlendElement.SVG_FEBLEND_MODE_COLOR_BURN")
  @js.native
  val SVG_FEBLEND_MODE_COLOR_BURN: Double = js.native
  
  /* standard dom */
  @JSGlobal("SVGFEBlendElement.SVG_FEBLEND_MODE_COLOR_DODGE")
  @js.native
  val SVG_FEBLEND_MODE_COLOR_DODGE: Double = js.native
  
  /* standard dom */
  @JSGlobal("SVGFEBlendElement.SVG_FEBLEND_MODE_DARKEN")
  @js.native
  val SVG_FEBLEND_MODE_DARKEN: Double = js.native
  
  /* standard dom */
  @JSGlobal("SVGFEBlendElement.SVG_FEBLEND_MODE_DIFFERENCE")
  @js.native
  val SVG_FEBLEND_MODE_DIFFERENCE: Double = js.native
  
  /* standard dom */
  @JSGlobal("SVGFEBlendElement.SVG_FEBLEND_MODE_EXCLUSION")
  @js.native
  val SVG_FEBLEND_MODE_EXCLUSION: Double = js.native
  
  /* standard dom */
  @JSGlobal("SVGFEBlendElement.SVG_FEBLEND_MODE_HARD_LIGHT")
  @js.native
  val SVG_FEBLEND_MODE_HARD_LIGHT: Double = js.native
  
  /* standard dom */
  @JSGlobal("SVGFEBlendElement.SVG_FEBLEND_MODE_HUE")
  @js.native
  val SVG_FEBLEND_MODE_HUE: Double = js.native
  
  /* standard dom */
  @JSGlobal("SVGFEBlendElement.SVG_FEBLEND_MODE_LIGHTEN")
  @js.native
  val SVG_FEBLEND_MODE_LIGHTEN: Double = js.native
  
  /* standard dom */
  @JSGlobal("SVGFEBlendElement.SVG_FEBLEND_MODE_LUMINOSITY")
  @js.native
  val SVG_FEBLEND_MODE_LUMINOSITY: Double = js.native
  
  /* standard dom */
  @JSGlobal("SVGFEBlendElement.SVG_FEBLEND_MODE_MULTIPLY")
  @js.native
  val SVG_FEBLEND_MODE_MULTIPLY: Double = js.native
  
  /* standard dom */
  @JSGlobal("SVGFEBlendElement.SVG_FEBLEND_MODE_NORMAL")
  @js.native
  val SVG_FEBLEND_MODE_NORMAL: Double = js.native
  
  /* standard dom */
  @JSGlobal("SVGFEBlendElement.SVG_FEBLEND_MODE_OVERLAY")
  @js.native
  val SVG_FEBLEND_MODE_OVERLAY: Double = js.native
  
  /* standard dom */
  @JSGlobal("SVGFEBlendElement.SVG_FEBLEND_MODE_SATURATION")
  @js.native
  val SVG_FEBLEND_MODE_SATURATION: Double = js.native
  
  /* standard dom */
  @JSGlobal("SVGFEBlendElement.SVG_FEBLEND_MODE_SCREEN")
  @js.native
  val SVG_FEBLEND_MODE_SCREEN: Double = js.native
  
  /* standard dom */
  @JSGlobal("SVGFEBlendElement.SVG_FEBLEND_MODE_SOFT_LIGHT")
  @js.native
  val SVG_FEBLEND_MODE_SOFT_LIGHT: Double = js.native
  
  /* standard dom */
  @JSGlobal("SVGFEBlendElement.SVG_FEBLEND_MODE_UNKNOWN")
  @js.native
  val SVG_FEBLEND_MODE_UNKNOWN: Double = js.native
}