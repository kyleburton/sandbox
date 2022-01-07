package algorithm;

public class OrderedNums {
    private int[] simpleLookingList;

    private int listLength;

    public OrderedNums(){

        listLength = 100;

        simpleLookingList = new int[listLength];

        for (int index = 0; index < listLength; index++){

            simpleLookingList[index] = index + 1;

        }//end for loop

    }//end constructor

    public int[] getList(){

        return simpleLookingList;

    }//end getter

    public void setList(int[] differentList){

        simpleLookingList = differentList;

    }//end setter

    public String toString(){

        String output = "";

        for (int nums : simpleLookingList){

            output += nums + " , ";

        }//end for loop

        return output;

    }//end toString

}//end class
