package advent_of_code.java;

public class LinkedListNode {

    private Object val;
    private LinkedListNode next;

    public LinkedListNode(Object val, LinkedListNode next) {
        this.val = val;
        this.next = next;
    }

    public Object getVal() {
        return this.val;   
    }

    public Object getNext() {
        return this.next;
    }

    public void setVal(Object val) {
        this.val = val;
    }

    public void setNext(LinkedListNode next) {
        this.next = next;
    } 

}
