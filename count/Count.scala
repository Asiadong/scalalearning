package count

import java.io.PrintWriter

import scala.collection.mutable
import bayesNetwork._
import scala.io.Source
import scala.reflect.io.File

/**
  * Created by Asia on 2017/7/20.
  */
object Count {
  val basepath="F:/competition/020初赛数据/020初赛数据/";
  val base_path="F:/PythonPro/new";
  val online_train_filePath=basepath+"ccf_online_stage1_train.csv";
  val offline_train_filePath=basepath+"ccf_offline_stage1_train.csv";
  val offline_test_filePath=basepath+"ccf_offline_stage1_test_revised.csv";
//
  class D(o:Long=0,f:Long=0,t:Long=0){
    var on=o
    var off=f
    var test=t
    def +(d: D): D ={
      on+=d.on;
      off+=d.off;
      test+=d.test;
      this;
  }//Of
  }//Of
  def main_s(args: Array[String]): Unit = {
    val put=new mutable.HashMap[String,D]();
    val off_count=basepath+"off_count";
    val on_coun=basepath+"on_count";
    val online = new Online();
    val offline = new Offline();
    val off_pre_line=new Online();
    online.set(online_train_filePath,on_coun);
    offline.set(offline_train_filePath,off_count);
    off_pre_line.set(offline_test_filePath,basepath+"test");
    var t3=new Thread(off_pre_line);
    var t1:Thread=new Thread(online);
    t1.start();
    var t2=new Thread(offline);
    t2.start();
    t3.start();
    while(t1.isAlive||t2.isAlive||t3.isAlive){
      Thread.sleep(200);
    }//Of while
    for( t <-online.data){
      val d=new D(t._2);
      put.put(t._1,d);
//      put.put(t._1,)
    }//Of for
    for( t <-offline.data){
      val d=new D(0,t._2);
      if(put.contains(t._1)){
        put.put(t._1,d+put.get(t._1).get);
      }
      else   put.put(t._1,d)
    }//Of for
    for( t <-off_pre_line.data){
      val d=new D(0,0,t._2);
      if(put.contains(t._1)){
        put.put(t._1,d+put.get(t._1).get);
      }
      else   put.put(t._1,d)
    }//Of for
    val w=new PrintWriter(basepath+"test_count");
    put.foreach(x=>{
      if(x._2.test!=0)
      w.write(x._1+";"+x._2.on+","+x._2.off+","+x._2.test+"\n");
    })
    w.close();
  }//Of main_
  def main(args: Array[String]): Unit = {
    var myBayes:bayesWork=new bayesWork(1);
    val node1=new Node;
    val node2=new Node;
    val node3=new Node;
    val node=new Node;
    node.addParent(node1);
    node.addParent(node2);
    node.addParent(node3);
    var data=Source.fromFile(base_path+"testData.txt","utf-8").getLines();
    for(line<-data){
      val extra=line.split(",");

    }//Of for
  }//Of function Main
}//Of obj Count
