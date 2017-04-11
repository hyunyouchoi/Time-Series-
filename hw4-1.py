def average(string):
    string = string.lower() 
    list = string.split()
    string = string.replace(".","")
    string = string.replace(" ","")
    numofwords=len(list)
    lenofstring=len(string)
    return lenofstring/numofwords

print(average("This is the test data to be used for the fourth assignment."))
