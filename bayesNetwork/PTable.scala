package bayesNetwork

import java.util
import java.util._

import scala.collection.JavaConverters._

/**
  * Created by Asia on 2017/7/26.
  */
class PTable(paCount:Int){
  var count:Int=0;
  var parentCount=paCount;
  val tab=new Array[ArrayList[ArrayList[Double]]](paCount);
  val tabVal=new Array[ArrayList[String]](paCount);
//  for(i<-0 to paCount-1){
//    var frequent:ArrayList[ArrayList[Double]]=null;
//    var value:=null;
//    tab(i)=
//  }

  def add(vals:String)={
    var frequent:ArrayList[ArrayList[Double]]=tab(0);
    var value:ArrayList[String]=tabVal(0);
    var index=0;
    if(count==0){
      frequent=new ArrayList[ArrayList[Double]]();
      value=new ArrayList[String]();
//    count+=1;
      var data=new ArrayList[Double]();
      data.add(1d);
      frequent.add(data);
      value.add(vals);
    }//Of if
    else if((index=value.indexOf(vals))!= -1){
      var cuFrequent=0d;
      var ele:ArrayList[Double]=null;
      for(i<- 0 to frequent.size()-1 ) {
        ele=frequent.get(i);
        cuFrequent = ele.get(0);
        if (i==index){
          ele.set(0, (cuFrequent * count + 1) / (count + 1));
        }//Of if
        else{
          ele.set(0, (cuFrequent * count + 0) / (count + 1));
        }//Of else
        frequent.set(i, ele);
      }//Of for
    }//Of if
    else{
      var data=new ArrayList[Double]();
      data.add(1d/count+1);
      frequent.add(data);
      value.add(vals);
    }//Of else
    count+=1;
  }//Of add
}//Of class PTable
