package base

import java.io.{BufferedReader, FileReader, FileWriter}
import java.text.{DateFormat, SimpleDateFormat}
import java.util

import wekaBased.PreProcess

import scala.collection.mutable

object Entrace {
   val basePath="F:\\competition\\020初赛数据\\020初赛数据\\";
  val basePathEnd="F:\\competition\\020初赛数据\\"
  val tranR=new FileReader(basePath+"ccf_offline_stage1_test_revised.csv")
  val fw=new FileWriter(basePath+"/processed/off.csv1");
  val ft=new FileWriter(basePath+"/processed/test.csv1");
  val tran=new FileReader(basePath+"ccf_offline_stage1_train.csv")
  val tranW=new FileWriter(basePath+"processed/tranTest.csv1")
  val map1=new util.ArrayList[String]()
  //map1.add("null")
  var count=0
    def spli(string: String)={
      val data=string.split(",")
      val uid=math.log(Integer.parseInt(data(0)))/math.log(100)
      val mid=math.log10(Integer.parseInt(data(1)))
      val cid=if(data(2).equals("null")) 0 else math.log10(Integer.parseInt(data(2)))
      if(!map1.contains(data(3))) {
        map1.add(data(3))
        println(data(3)+"added\n")
      };
      val disR=map1.indexOf(data(3))
      val dist=if(data(4).equals("null")) 21 else data(4)
      /*var re=0;
      if(data(5).equals("null")){
        re=0
      }//Of if
      else if(data(6).equals("null")){
        re=100;
      }//Of else if
      else{
        val df=new SimpleDateFormat("yyyyMMdd")
        val date1=df.parse(data(5))
        val date2=df.parse(data(6))
        re=((date2.getTime()-date1.getTime())/(1000*24*3600)).toInt
      }//Of else*/
     // if(count%100==0){
        tranW.write(uid+","+mid+","+cid+","+disR+","+dist+"\n")

      //}//Of if
      //else
      //fw.write(uid+","+mid+","+cid+","+disR+","+dist+","+re+"\n");
      //count+=1

    }//Of spli
    def combine()={
      var tempnew=new BufferedReader(new FileReader(basePath+"processed/tranTest.csv1"))
      val bufrr=new BufferedReader(tranR)
      val w=new FileWriter(basePath+"processed/put.csv")
      var s=bufrr.readLine()
      while(s!=null){
        val ss=bufrr.readLine().split(",")
        w.write(ss(0)+","+ss(1)+","+ss(5)+","+"s"+"\n")
      }//Of while
    }//Of combine
   def main(args: Array[String]): Unit = {
     /**
       * this is for preprocess the data
       */

    // val preProcess=new FileLoad(LOG.path+"OffSpit0.0.csv")
     //val bufferedReader =new BufferedReader(tran)
     /*
     var str=bufferedReader.readLine()
     while(str!=null){
       map1.add(str)
       str=bufferedReader.readLine()
     }//Of for
     preProcess.process(x=>spli(x))
*/
  /*   //println(map1.size())
     for(i<-0 to map1.size()-1){
       tranW.write(map1.get(i)+"\n")
     } //Of for
       fw.close()
       ft.close()*/
     //tranW.close()



/*val construction=Array(5,10,15,10,10,3);
nn.NN.build(construction)
var data=preProcess.fileToArray(6)
while(data!=null){
  nn.NN.train(data,10)
  data=preProcess.fileToArray(6)
}//Of if
     println("train end")
     preProcess.fileLoad(LOG.path+"OffSpit2.0.csv")
     data=preProcess.fileToArray(6)
     var total=0
     var right=0
     while(data!=null){
       total+=1
       if(nn.NN.test2(data))
         right+=1
        // tranW.write(nn.NN.test2(data)+"\n")
       //data=preProcess.fileToArray(6)

     }//Of if
     println("Total is:"+total)
     println("Accurate is"+right)
     println("Accuracy is:"+1.0*right/total)*/

    //tranW.close()


     new PreProcess().featureSelect
     //var a=new Array[Array[Double]](6)
    // Actor2.ss()
}//Of fun main
}//Of object entrace                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                =fd