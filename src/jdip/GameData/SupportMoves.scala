/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package GameData

import dip.world._

class SupportMoves (w: World, pName : String) {
  val supportMoves : List[(Location, PairList)] = supportMovesConstructor
  
  private type PairList = List[(Location, Location)]
  private def supportMovesConstructor : List[(Location, PairList)] = {
    val tState : TurnState = w.getLastTurnState
    val cPos : Position = tState.getPosition
    val f : Path.FleetFAPEvaluator = new Path.FleetFAPEvaluator(cPos)
    val m : Map = w.getMap
    val mwoc : MovesWithoutConvoy = new MovesWithoutConvoy(w, pName)
    val pMov : List[(Location, List[Location])] = mwoc.m
    val ps : List[Power] = m.getPowers.toList
    val mwocs : List[MovesWithoutConvoy] = ps map 
                                    (u => new MovesWithoutConvoy(w, u.getName))
    val sdPairs1 : List[PairList] = mwocs map
                                                    (u => u.generateSrcDstPairs)
    val mwcs : List[MovesByConvoy] = ps map
                                          (u => new MovesByConvoy(w, u.getName))
    val sdPairs2 : List[PairList] = mwcs map
                                                    (u => u.generateSrcDstPairs)
    val srcDstPairs : PairList =
                        (sdPairs1:::sdPairs2)
                        .foldLeft(Nil : PairList)((x,y) => x:::y)
    val sm : List[(Location, PairList)] = pMov map (u => (u._1,
                        srcDstPairs filter (v => !u._1.isProvinceEqual(v._1))
                        filter
                        (v => u._2 exists (q => q.isProvinceEqual(v._2)))))
    val supMoves : List[(Location, PairList)] = sm map (u => 
                (u._1, u._2 filter (v =>
                             !isNeededForConvoy(cPos, f, u._1, v._1, v._2))))
    supMoves
  }

  /* Checks if the support fleet unit is needed in all convoy paths from source
   * to destination.  If the source province doesn't contain an ARMY unit
   * or the supporting province isn't a naval unit, then the supporting
   * unit isn't needed for the convoy action */
  private def isNeededForConvoy(pos: Position, f : Path.FleetFAPEvaluator,
                                sprt : Location, src : Location,
                                dst : Location) : Boolean = {
    val sprtUnit : Unit = pos.getUnit(sprt.getProvince)
    val srcUnit : Unit = pos.getUnit(src.getProvince)
    if (sprtUnit.getType == Unit.Type.FLEET &&
                                            srcUnit.getType == Unit.Type.ARMY) {
      val allPaths : List[List[Province]] =
            Path.findAllSeaPaths(f, src.getProvince, dst.getProvince).toList map
                                                       (u => u.toList.init.tail)
      allPaths.foldLeft(true)((x : Boolean, y : List[Province])
                          => x && (y exists (u => sprt.isProvinceEqual(u))))
    } else {
      false
    }
    
  }

  def getSupportMoves(l : Location) : List[(Location, Location)] =
        (supportMoves find (u => u._1.equals(l))) match {
          case Some(x) => x._2
          case None => Nil
        }
  
}
