// Name : Huang Jiaming
// NSID : jih211
// StuID: 11207964

package a4;

import java.util.ArrayList;
import java.util.Scanner;

import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.actor.Props;
import akka.actor.UntypedActor;

public class StableMarriage {
    public static int[] couples;
    public static ArrayList<ActorRef> manRef;
    public static ArrayList<ActorRef> womanRef;
    public static int[][] manBackupRanks;
    public static int[][] womanBackupRanks;
    
    public StableMarriage(int n) {
        couples = new int[n];
        manRef = new ArrayList<ActorRef>();
        womanRef = new ArrayList<ActorRef>();
        manBackupRanks = new int[n][n];
        womanBackupRanks = new int[n][n];
    }
    
    public static class manActor extends UntypedActor {

        private int id;
        private int num;
        private ArrayList<Integer> rank;
        
        public manActor(int a, int n) {
            this.id = a;
            this.num = n;
            createRank();
            propose();
        }
        
        private void createRank() {
            rank = new ArrayList<Integer>();
            for (int i = 0; i < num; i++) {
                rank.add(i);
            }
            java.util.Collections.shuffle(rank); 
            for (int i = 0; i < num; i++) {
                manBackupRanks[id][i] = rank.get(i);
            }
        }               
        
        public void propose() {
            if (!this.rank.isEmpty()) {
                ActorRef ref = womanRef.get(rank.remove(0));
                ref.tell(new Integer(this.id), getSelf());
            } else {
                System.err.println("Opps...");
            }
        }
        
        @Override
        public void onReceive(Object arg0) throws Throwable {
            if (arg0 instanceof Integer) {
                    int woman = (Integer)arg0;
                    couples[this.id] = woman;
            } else if (arg0 instanceof String) {
                propose();
            }
            
        }
        
    }
    
    public static class womanActor extends UntypedActor {
        private int id;
        private int curMan;
        private int num;
        private ArrayList<Integer> rank; // ascending order
        public boolean matched = false;
                
        public womanActor(int a, int n) {
            this.id = a;
            this.num = n;
            createRank();
        }

        private void createRank() {
            rank = new ArrayList<Integer>();
            for (int i = 0; i < num; i++) {
                rank.add(i);
            }
            java.util.Collections.shuffle(rank);
            for (int i = 0; i < num; i++) {
                womanBackupRanks[id][i] = rank.get(i);
            }
        }
        
        @Override
        public void onReceive(Object arg0) throws Throwable {
            if (arg0 instanceof Integer) {                
                int man = (Integer)arg0; 
                if (this.matched == false) {
                    this.matched = true; 
                    this.curMan = man;
                    couples[curMan] = this.id;
                    // reply integer id if agree
                    getSender().tell(new Integer(this.id), getSelf());
                } else if (prefer(man)) {
                    // break with curMan
                    ActorRef ref = manRef.get(curMan);
                    ref.tell("n", getSelf());
                    couples[curMan] = -1;
                    
                    this.curMan = man;
                    couples[curMan] = this.id;
                    getSender().tell(new Integer(this.id), getSelf());
                } else {    // reply "n" if disagree
                    getSender().tell("n", getSelf());
                }
            }            
        }
        
        private boolean prefer(int potentialMan) {
            int currentMan = 0;
            for (int i = 0; i < couples.length; i++) {
                if (couples[i] == this.id) {
                    currentMan = i;
                    break;
                }
            }
            int j1 = -1,j2 = -1;
            for (int i = 0; i < this.rank.size(); i++) {
                if (rank.get(i) == currentMan) {
                    j1 = i;
                }
                if (rank.get(i) == potentialMan) {
                    j2 = i;
                }
            }
            return j1 < j2;            
        }        
    }
    
    @SuppressWarnings("static-access")
    public static void main(String[] args) {
        System.out.println("Input an integer to specify the number of mam or woman:");
        Scanner in = new Scanner(System.in);
        int number = in.nextInt();
        in.close();
        StableMarriage sm = new StableMarriage(number);
        final ActorSystem actorSystem = ActorSystem.create("actor-system");
        for (int i = 0; i < number; i++) {
            sm.womanRef.add(actorSystem.actorOf(Props.create(StableMarriage.womanActor.class, i, number),"woman"+i));
        }
        for (int i = 0; i < number; i++) {
            sm.manRef.add(actorSystem.actorOf(Props.create(StableMarriage.manActor.class, i, number),"man"+i));
        }
        // wait 1 second for the result
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        System.out.println("rank list is created randomly in ascending order,"
                + " e.g.(Priority of rank[i] < Priority of rank[i+1] ");
        System.out.println("-- Men rank lists --");
        for (int i = 0; i < number; i++) {
            System.out.print("man_" + i + ": ");
            for (int j = 0; j < number; j++) {
                System.out.print(StableMarriage.manBackupRanks[i][j] + ", ");
            }
            System.out.println("");
        }
        System.out.println("-- Women rank lists --");
        for (int i = 0; i < number; i++) {
            System.out.print("woman_" + i + ": ");
            for (int j = 0; j < number; j++) {
                System.out.print(StableMarriage.womanBackupRanks[i][j] + ", ");
            }
            System.out.println("");
        }
        System.out.println("-- Stable match --");
        for (int i = 0; i < number; i++) {
            System.out.println("man_" + i + " : " + "woman_" + StableMarriage.couples[i]);
        }
        actorSystem.terminate();
    }
}
