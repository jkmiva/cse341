// Name : Huang Jiaming
// NSID : jih211
// StuID: 11207964

package a4;

import java.util.Scanner;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.actor.Props;
import akka.actor.UntypedActor;
import akka.pattern.Patterns;
import akka.util.Timeout;
import scala.concurrent.Await;
import scala.concurrent.Future;
import scala.concurrent.duration.Duration;
import akka.actor.Inbox;

public class FiboSolution {
    
    private static Long table[] = new Long[10000];
    
    private static class myActor extends UntypedActor {
        
        @Override
        public void onReceive(Object arg0) throws Throwable {
            Long reply = new Long(1);
            if (arg0 instanceof Integer) {                
                Integer msg = (Integer) arg0;
                if (msg == 1 || msg == 2) {
                    reply = new Long(1);
                }
                else {
                    Timeout t = new Timeout(Duration.create(20, TimeUnit.SECONDS));
                    Long r1 = new Long(1);
                    Long r2 = new Long(1);
                    if (table[msg-1] == null) {
                        final ActorRef childActor1 = getContext().actorOf(Props.create(myActor.class));
                        Future<Object> future1 = Patterns.ask(childActor1, new Integer(msg-1), t);
                        try {
                            r1 = (Long) Await.result(future1, t.duration());
                            table[msg-1] = r1;
                        } catch (TimeoutException e) {
                            System.out.println("Got a timeout after waiting 20 seconds for the value from a child worker");
                            System.exit(1);
                        }
                    }
                    if (table[msg-2] == null) {
                        final ActorRef childActor2 = getContext().actorOf(Props.create(myActor.class));
                        Future<Object> future2 = Patterns.ask(childActor2, new Integer(msg-2), t);
                        try {                            
                            r2 = (Long) Await.result(future2, t.duration());
                            table[msg-2] = r2;
                        } catch (TimeoutException e) {
                            System.out.println("Got a timeout after waiting 20 seconds for the value from a child worker");
                            System.exit(1);
                        }
                    }                  
                    reply = table[msg-1] + table[msg-2];
                }
                getSender().tell(reply, getSelf());
            }
        }
    }
    
    public static void main(String[] args) {
        System.out.println("Input an Long n to calculte the nth fibonacci number: ");
        Scanner input = new Scanner(System.in);
        Integer aLong = new Integer(input.nextInt());
        input.close();
        
        final ActorSystem actorSystem = ActorSystem.create("actor-system");
        final ActorRef worker = actorSystem.actorOf(Props.create(myActor.class),"worker");
        final Inbox inbox = Inbox.create(actorSystem);
        inbox.send(worker, aLong);
        
        Long reply = null;
        try {
            reply = (Long) inbox.receive(Duration.create(20, TimeUnit.SECONDS));
        } catch (TimeoutException e) {
            System.out.println("Got a timeout after waiting 20 seconds !");
            System.exit(1);
        }
        
        System.out.println("result is " + reply + ".");
        actorSystem.terminate();
    }
}