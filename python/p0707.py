class MyLinkedList:
    '''
    Design your implementation of the linked list. You can choose to use
    the singly linked list or the doubly linked list. A node in a singly
    linked list should have two attributes: val and next. val is the
    value of the current node, and next is a pointer/reference to the
    next node. If you want to use the doubly linked list, you will need
    one more attribute prev to indicate the previous node in the linked
    list. Assume all nodes in the linked list are 0-indexed.

    Implement these functions in your linked list class:
    get(index) : Get the value of the index-th node in the linked list.
                 If the index is invalid, return -1.
    addAtHead(val) : Add a node of value val before the first element
                 of the linked list. After the insertion, the new node
                 will be the first node of the linked list.
    addAtTail(val) : Append a node of value val to the last element of
                 the linked list.
    addAtIndex(index, val) : Add a node of value val before the index-th
                node in the linked list. If index equals to the length
                of linked list, the node will be appended to the end of
                linked list. If index is greater than the length, the node
                will not be inserted.
    deleteAtIndex(index) : Delete the index-th node in the linked list,
                if the index is valid.

    Example:
    Input:
        ["MyLinkedList","addAtHead","addAtTail","addAtIndex","get","deleteAtIndex","get"]
        [[],[1],[3],[1,2],[1],[1],[1]]
    Output:
        [null,null,null,null,2,null,3]
    Explanation:
        MyLinkedList linkedList = new MyLinkedList(); // Initialize empty LinkedList
        linkedList.addAtHead(1);
        linkedList.addAtTail(3);
        linkedList.addAtIndex(1, 2);  // linked list becomes 1->2->3
        linkedList.get(1);            // returns 2
        linkedList.deleteAtIndex(1);  // now the linked list is 1->3
        linkedList.get(1);            // returns 3
    Constraints:
        0 <= index,val <= 1000
        Please do not use the built-in LinkedList library.
        At most 2000 calls will be made to get, addAtHead, addAtTail,  addAtIndex and deleteAtIndex.
    '''
    class Node(object):
        # no an elegant implementation: just functional. needs refactoring of the recurring traversals
        def __init__(self, data=None, prev=None, next=None):
            self.val = data
            self.prev = prev
            self.next = next
            return


    def __init__(self):
        """
        Initialize your data structure here.
        """
        self.root = None
        self.tail = self.root
        self.n = 0


    def print_list(self, what=""):
        return
        p = self.root
        i = 0
        print(what)
        while p:
            print (i, p.val)
            i = i+1
            p = p.next
        print('-'*32)
        return

    def get(self, index: int) -> int:
        """
        Get the value of the index-th node in the linked list. If the index is invalid, return -1.
        """

        if not (0 <= index < self.n):
            return -1

        ix, p = 0, self.root
        self.print_list("get")
        while p:
            if ix == index:
                return p.val
            ix += 1
            p = p.next
        return -1


    def addAtHead(self, val: int) -> None:
        """
        Add a node of value val before the first element of the linked list. After the insertion, the new node will be the first node of the linked list.
        """
        new_head = self.Node(val)
        if not self.root:
            self.root = new_head
            self.tail = new_head
            self.n += 1
            return

        new_head.next = self.root
        new_head.prev = None
        self.root.prev = new_head
        self.root = new_head
        self.n += 1
        self.print_list("addHead")
        return


    def addAtTail(self, val: int) -> None:
        """
        Append a node of value val to the last element of the linked list.
        """

        new_tail = self.Node(val)
        if not self.root:
            self.root = new_tail
            self.tail = new_tail
            self.n += 1
            return

        new_tail.prev = self.tail
        new_tail.next = None
        self.tail.next = new_tail
        self.tail = new_tail
        self.n += 1
        self.print_list("addTail")
        return


    def addAtIndex(self, index: int, val: int) -> None:
        """
        Add a node of value val before the index-th node in the linked list. If index equals to the length of linked list, the node will be appended to the end of linked list. If index is greater than the length, the node will not be inserted.
        """
        if not (0 <= index <= self.n): return
        ix = 0
        p = self.root
        if not self.root:
            self.addAtHead(val)
        if index == self.n:
            self.addAtTail(val)
            return

        self.print_list("add")
        while p:
            if ix == index:
                curr = p
                prev = p.prev
                new_node = self.Node(val)
                new_node.next = curr
                new_node.prev = prev
                curr.prev = new_node
                if not prev:
                    self.root = new_node
                else:
                    prev.next = new_node
                self.n += 1
                return
            ix += 1
            p = p.next
        return



    def deleteAtIndex(self, index: int) -> None:
        """
        Delete the index-th node in the linked list, if the index is valid.
        """
        if not (0 <= index < self.n): return
        ix = 0
        if not self.root:
            self.addAtHead(val)
            return

        p = self.root
        self.print_list('del')
        while p:
            if ix == index:
                self.n -= 1
                if p == self.root:
                    curr_node = p
                    next_node = curr_node.next
                    self.root = next_node
                    if next_node:
                        next_node.prev = None
                        return
                    self.tail = next_node
                    return
                if p == self.tail:
                    curr_node = p
                    prev_node = curr_node.prev
                    self.tail = prev_node
                    if prev_node:
                        prev_node.next = None
                        return
                    self.head = prev_node
                    return
                curr_node = p
                next_node = curr_node.next
                prev_node = curr_node.prev
                prev_node.next = next_node
                next_node.prev = prev_node
                return
            ix += 1
            p = p.next
        return



# Your MyLinkedList object will be instantiated and called as such:
# obj = MyLinkedList()
# param_1 = obj.get(index)
# obj.addAtHead(val)
# obj.addAtTail(val)
# obj.addAtIndex(index,val)
# obj.deleteAtIndex(index)
