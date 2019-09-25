package base

import java.io._

/**
  * Created by Asia on 2017/8/29.
  */
class FileLoad(path:String) {
  var fileInReader:InputStreamReader=null;
  var bufreader:BufferedReader=null;
  fileLoad(path)
  def fileLoad(filePath:String)={
    fileInReader=new FileReader(filePath);
    bufreader=new BufferedReader(fileInReader);
  } //Of fileLoad
  def process(f:String=>Unit, describe:Boolean=true)={
    val bufr=new BufferedReader(fileInReader);
    var temp:String="";
    if(describe)bufr.readLine()
    temp=bufr.readLine()
    while( temp != null){
     /* print(temp)*/
  /*    if(temp!=null) {
       // print("NULL OCCERED")
        f(temp);
      }*/
      f(temp)
      temp=bufr.readLine()
    }//Of while
  }//Of process
  def fileToArray(length:Int):Array[Double]={
    val tmp=bufreader.readLine()
    var result=new Array[Double](length)
    /*println(tmp)*/
    if(tmp==null) result=null
    else{
      val tempData=tmp.split(",")
      for(i<-0 to length-1){
        result(i)=java.lang.Double.parseDouble(tempData(i))
      }//Of for
     /* result(5)=Integer.parseInt(tempData(5))*/
    }//Of else
    result
  }//Of if
}//Of class preProcess
