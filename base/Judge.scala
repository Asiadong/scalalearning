package base

import java.io.{BufferedReader, FileReader}

import com.sun.deploy.util.SyncFileAccess.FileOutputStreamLock

/**
  * Created by Asia on 2017/8/29.
  */
class Judge {
  var right=0;
  var wrong=0;
  def getResult={
    println("The right predicted count is:"+right)
    println("The wrong predicted count is:"+wrong)
    println("accuracy is:"+right/(right+wrong))
  }//Of getResult
  def count(testFile:String,outFile:String,f:String=>String)={
    val fileTest=new FileReader(testFile)
    val fileOut=new FileReader(outFile)
    val bfrt=new BufferedReader(fileTest)
    val bfro=new BufferedReader(fileOut)
    var s="";
    while((s=bfro.readLine())!=null){
      if (f(s).equals(bfrt.readLine()))right+=1 else wrong+=1
    }//Of while
  }//Of count
}//Of class Judge
