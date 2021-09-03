/**
 *
 * java.util.Scanner inp = new java.util.Scanner(System.in);
 *
 *
 */
public class SwitchBasedEventLoop {
    public static java.util.Scanner inp;
    public enum Locations {
        FRONT_PORCH,
        ENTRYWAY,
        DEN,
        LIVING_ROOM,
    }

    public static String checkQuit(String action) {
        if (action.equals("quit") || action.equals("exit") || action.equals("q")) {
            System.out.println("ok, goodbye and thanks for playing!");
            System.exit(0);
        }

        return action;
    }

    public static String getPlayerInput(String prompt) {
        System.out.print(prompt);
        System.out.print("\n> ");
        return checkQuit(inp.nextLine());
    }


    public static void main (String [] args) {
        Locations playerLocation = Locations.FRONT_PORCH;
        inp = new java.util.Scanner(System.in);
        String action;

        while (true) {
            switch (playerLocation) {
                case FRONT_PORCH:
                    System.out.println("");
                    action = getPlayerInput("You find yoruself standing the front door to a house, you see a doorbell and a welcome mat.");
                    if (action.equals( "ring bell") || action.equals("ring doorbell") || action.equals("ring")) {
                        System.out.println("you ring the bell, the door mysteriously swings open and you step in side ...");
                        playerLocation = Locations.ENTRYWAY;
                        break;
                    }
                    else {
                        System.out.println("I'm not sure you can do that here ...");
                    }
                    break;
                case ENTRYWAY:
                    System.out.println("");
                    action = getPlayerInput("You're in the entryway of the house, you can see a door to your right that leads into the den, and the living room.  You can go into the den (den, door, right, north) or the living room (living room, forward, west).");
                    if (action.equals("den")) {
                        System.out.println("you step down a few stairs and into the den ...");
                        playerLocation = Locations.DEN;
                        break;
                    }
                    else {
                        System.out.println("I'm not sure you can do that here ...");
                    }
                    break;
                default:
                    System.out.println("oh no, I'm not sure where you are or how you got here ... your feel lightheaded and dizzy");
                    System.out.println(" ... you feel yourself falling as you slip unconcious ... ");
                    playerLocation = Locations.FRONT_PORCH;
            }
        }
    }
}
