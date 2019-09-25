package base

import java.io.{BufferedWriter, FileOutputStream, FileWriter, OutputStream}

import scala.collection.mutable

/**
  * Created by Asia on 2017/9/8.
  */
object LOG {
  val path="F:\\competition\\020初赛数据\\info\\"
  var writer:FileWriter=null
  var bfw:BufferedWriter=null
  val map=new mutable.HashMap[String,BufferedWriter]()
  def data(name:String,value:String,convert:Boolean=false): Unit = {
    writer = new FileWriter(path + name, true)
    if (!convert){
        writer.write("=======" + name + "========\n" + value + "\n")
      }//Of if
    else{
        writer.write(value)
    }//Of else
    writer.flush()
    writer.close()
  }//Of data
  def data(name:String,value:String): Unit ={
    if (!map.contains(name)){
      bfw=new BufferedWriter(new FileWriter(path+name,true),1024*1024*16)
      map.put(name,bfw)
    }//Of if
    else{
      bfw=map.get(name).get
    }//Of else
    bfw.write(value)
  }//Of data
  def err(name:String): Unit ={
  }//Of err
  def out={
    map.iterator.foreach(x=>x._2.close())
  }//Of out
}//Of LOG
