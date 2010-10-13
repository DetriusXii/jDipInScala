/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jdip.imageOutput

import jdip.rewritten.SymbolInjector

import dip.world.World
import dip.world.variant.VariantManager
import dip.world.variant.data.Variant
import dip.world.variant.data.MapGraphic
import dip.world.variant.data.SymbolPack

import jdip.rewritten.RenderCommandFactory.RenderCommand

import jdip.rewritten._

import java.net.URL
import java.io._

import org.w3c.dom.svg.SVGDocument
import org.w3c.dom.Document
import org.apache.batik.transcoder._
import org.apache.batik.transcoder.svg2svg.SVGTranscoder
import org.apache.batik.util.XMLResourceDescriptor
import org.apache.batik.dom.svg.SAXSVGDocumentFactory
import javax.xml.transform._


import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult

class ImageCreator(savePath : String,
                   variantFolder: String,
                   isValidating: Boolean) {

  def process(saveName : String, w : World) : Unit = {
      val vi : World.VariantInfo = w.getVariantInfo
      val search_path : Array[File] = new Array[File](1)
      search_path(0) = new File(variantFolder)

      VariantManager.init(search_path, isValidating);
      val variant : Variant =
        VariantManager.getVariant(vi.getVariantName, vi.getVariantVersion);
      val mg : MapGraphic =
        variant.getMapGrapic(vi.getMapName); /*the location*/

      val url : URL = VariantManager.getResource( variant, mg.getURI )

      /*finds the location of the Unit symbols and other symbols*/
      val symbolPack : SymbolPack =
        VariantManager.getSymbolPack(mg,
                                     vi.getSymbolPackName,
                                     vi.getSymbolPackVersion)
      
      val si : SymbolInjector = new SymbolInjector(isValidating,
                                               variant,
                                               mg,
                                               symbolPack)
      si.inject

      val text : String =
        VariantManager.getVariantPackageJarURL(variant).toString
      val transform_doc : Document
        = this.transform(si.getDocument,
                                 VariantManager
                                    .getVariantPackageJarURL(variant).toString)

      val mr : DefaultMapRenderer2 =
        new DefaultMapRenderer2(transform_doc.asInstanceOf[SVGDocument],
                                w,
                                symbolPack)
      
      val rcf : RenderCommandFactory = mr.getRenderCommandFactory
      val rc0 : RenderCommand =
        rcf.createRCSetLabel(mr, MapRenderer2.VALUE_LABELS_BRIEF)
      rc0.execute
      /*needed to evaluate the current turn state*/
      val rc1 : RenderCommand =
        rcf.createRCSetTurnstate(mr, w.getLastTurnState)
      rc1.execute
      val rc2 : RenderCommand = rcf.createRCRenderAll(mr); /*needed to render*/
      rc2.execute
      val rc3 : RenderCommand = rcf.createRCSetDisplayUnits(mr, true)
      rc3.execute



      val second_doc : Document = mr.get_document
      val outputImageSVG : File = new File(savePath + saveName + ".svg")
      outputImageSVG.delete/*the output image*/

      val tIn : TranscoderInput =
        new TranscoderInput(second_doc.cloneNode(true).asInstanceOf[Document]);
      val writer : Writer = new FileWriter(outputImageSVG)

      val tOut : TranscoderOutput= new TranscoderOutput(writer)


        /*having troubles with the SVGTranscoder, the JPEGTranscoder works fine
        */

      val transcoder : SVGTranscoder= new SVGTranscoder
      transcoder.transcode(tIn, tOut)

      writer.flush
      writer.close
  }

  def transform(inDoc : Document, uri : String) : Document = {
    val tFactory : TransformerFactory = TransformerFactory.newInstance
    val transformer : Transformer = tFactory.newTransformer

    val sw : StringWriter = new StringWriter
    val result : StreamResult = new StreamResult(sw)
    transformer.transform(new DOMSource(inDoc), result)
    sw.flush
    sw.close

    val parser : String = XMLResourceDescriptor.getXMLParserClassName
    val f : SAXSVGDocumentFactory = new SAXSVGDocumentFactory(parser)
    val outDoc : SVGDocument =
      f.createSVGDocument(uri, new StringReader(sw.toString))

    outDoc
  }
}
