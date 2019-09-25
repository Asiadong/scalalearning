package count

import java.io.PrintWriter
import scala.collection.mutable
import scala.io.Source

/**
  * Created by Asia on 2017/7/20.
  */
class Online extends Runnable{
  var r_path="";
  var w_path="";
  var UID="";
  var MID="";
  var CID="";
  var data:mutable.HashMap[String,Long]=null;
  def set(file_r_Path:String,file_w_path:String)={
    r_path=file_r_Path;
    w_path=file_w_path;
  }//Of set
  def count():mutable.Map[String,Long]={
    data=new mutable.HashMap[String,Long];
    var file=Source.fromFile(r_path).getLines();
    file.foreach({
      x=>{
        var tmp=iter(x);
        if(!data.contains(tmp)){
          data.put(tmp,1);
        }//of if
        else{
          data.update(tmp,data.get(tmp).get+1);
        }//Of else
      }//Of Online
    });
    data;
  }//Of count
  def iter(value: String): String ={
    val tmp=value.split(",");
    UID=tmp(0);
    MID=tmp(1);
    CID=tmp(2);
    UID;
  }//Of iter
  override def run(): Unit = {
    println(this.getClass.toString);
    var value=count();
    var writer=new PrintWriter(w_path);
    value.foreach(x=>writer.write(x._1+","+x._2+"\n"));
    writer.close();
  }//Of run
}
