package base

import java.text.SimpleDateFormat

/**
  * Created by Asia on 2017/9/22.
  */
class DateProcesser(strDate: String ) {
  val df=new SimpleDateFormat("yyyyMMdd")
  val D=df.parse(strDate)
  def month:Int={
    D.getMonth
  }//Of month
  def day:Int={
    D.getDay
  }//Of Date
}//Of DataProcesser
