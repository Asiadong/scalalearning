package wekaBased

import java.io.{BufferedReader, File, FileReader}

import org.jfree.data.io.CSV
import weka.core.{Attribute, Instance, Instances}
import weka.core.converters._

/**
  * Created by Asia on 2017/9/7.
  */
class FileChange {
  val basePath="F:\\competition\\020初赛数据\\"
  val off_train="ccf_offline_stage1_train.csv"
  val on_train="ccf_online_stage1_train.csv"
  val test="ccf_offline_stage1_test_revised.csv"
  def changeFile()={
    val csvF=new File(basePath+test)
    val arffSaver=new ArffSaver()
    val csv=new CSVLoader()
    csv.setSource(csvF)
    csv.setNominalAttributes("3,4,5,6")
    val instances1=csv.getDataSet()
    arffSaver.setInstances(instances1)
    arffSaver.setFile(new File(basePath+"end_test.arff"))
    arffSaver.writeBatch()

  }//Of changFile
}//Of FileChange
