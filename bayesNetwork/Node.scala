package bayesNetwork

import java.util
import java.util._

/**
  * Created by Asia on 2017/7/26.
  */
class Node {
  var parentCount=0;
  var parents: ArrayList[Node] = null;
  var pTable:PTable=null;
  def addParent(parent:Node)={
    if (parents==null) parents=new util.ArrayList[Node]()
    else parents.add(parent)
    parentCount+=1;
  }//Of def
  def construct(): Unit ={
    pTable=new PTable(parentCount);
  }//Of construct
  def load(vals:String)= {
  pTable.add(vals);
  }//Of load
}//Of class Node
