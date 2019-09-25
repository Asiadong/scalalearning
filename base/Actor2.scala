package base
import scala.actors._, Actor._
/**
  * Created by Asia on 2017/9/22.
  */
object Actor2 {

  case class Speak(line : String);
  case class Gesture(bodyPart : String, action : String);
  case class NegotiateNewContract;
  case class ThatsAWrap;

  def ss() =
  {
    val badActor =
      actor
      {
        var done = false
        //while (! done)
        //{
          receive
          {
            case NegotiateNewContract =>
              System.out.println("I won't do it for less than $1 million!")
            case Speak(line) =>
              System.out.println(line)
            case Gesture(bodyPart, action) =>
              System.out.println("(" + action + "s " + bodyPart + ")")
            case ThatsAWrap =>
              System.out.println("Great cast party, everybody! See ya!")
              done = true
            case _ =>
              System.out.println("Huh? I'll be in my trailer.")
          }
        //}//
      }

    badActor ! NegotiateNewContract
    badActor ! Speak("Do ya feel lucky, punk?")
    badActor ! Gesture("face", "grimaces")
    badActor ! Speak("Well, do ya?")
    badActor ! ThatsAWrap
  }

}//Of Actor
