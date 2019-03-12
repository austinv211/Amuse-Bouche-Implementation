
class List(object):
    def __init__(self, data=None, link=None):
        self.data = data
        self.link = link

    def getData(self):
        return self.data

    def getNext(self):
        return self.link

    def setNext(self, link):
        self.link = link

class LinkedList(object):
    def __init__(self, head=None):
        self.head = head
        self.size = 0

    def insert(self, data):
        new_link = List(data)
        new_link.setNext(self.head)
        self.head = new_link
        self.size += 1

    def dropOne(self):
        self.head = self.head.getNext() if self.size > 0 else None

    def justOne(self):
        return self.head.getData() if self.size > 0 else None

    def toString(self):
        temp = self.head
        while (temp != None):
            print(temp.getData())
            temp = temp.getNext()

    def clone(self):
        newList = LinkedList()
        temp = self.head
        dataList = []
        while (temp != None):
            dataList = [temp.getData()] + dataList
            temp = temp.getNext()
        for x in dataList:
            newList.insert(x)
        return newList

if __name__ == '__main__':
    oneWord = LinkedList()
    oneWord.insert("apple")
    # oneWord.toString()
    twoWords = LinkedList()
    twoWords.insert("cantaloupe")
    twoWords.insert("banana")
    # twoWords.toString()
    mystery1 = LinkedList()
    mystery1.insert("pear")
    # mystery2 = oneWord.insert("peach") -> this cannot be done

    mystery2 = oneWord.clone()
    mystery2.insert("peach")

    mystery2.toString()
    mystery2.dropOne()
    mystery2.toString()

