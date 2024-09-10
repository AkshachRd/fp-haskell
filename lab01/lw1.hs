-- Get a char value from the tuple using fst and snd functions
task1 = do
    let tuple = ((1, 'a'), "abc")
    let charValue = snd (fst tuple)
    print charValue

-- Find 'b' element in the lists using head and tail functions
task2 = do
    let list1 = ['a', 'b', 'c']
    let elementB = head (tail list1)
    print elementB

    let list2 = [['a', 'b'], ['c', 'd']]
    let elementB = head (tail (head list2))
    print elementB

    let list3 = [['a', 'c', 'd'], ['a', 'b']]
    let elementB = head (tail (head (tail list3)))
    print elementB

    let list4 = [['a', 'd'], ['b', 'c']]
    let elementB = head (head (tail list4))
    print elementB

-- Create a list of odd numbers of length 20
task3 = do
    let oddNumbers1 = take 20 [1, 3..]
    print oddNumbers1

    let oddNumbers2 = take 20 [x | x <- [1..], odd x]
    print oddNumbers2

    let oddNumbers3 = take 20 (filter odd [1..])
    print oddNumbers3

-- Create a list of triangular numbers of length 50
task4 = do
    let triangularNumber n = n * (n + 1) `div` 2
    let triangularNumbers = [triangularNumber x | x <- [1..50]]
    print triangularNumbers

-- Create a list of pyramidal numbers of length 50
task5 = do
    let pyramidalNumber n = n * (n + 1) * (2 * n + 1) `div` 6
    let pyramidalNumbers = [pyramidalNumber x | x <- [1..50]]
    print pyramidalNumbers

main = do
    print "1) Get a char value from the tuple using fst and snd functions: "
    task1

    print "2) Find 'b' element in the lists using head and tail functions: "
    task2

    print "3) Create a list of odd numbers of length 20: "
    task3

    print "4) Create a list of triangular numbers of length 50: "
    task4

    print "5) Create a list of pyramidal numbers of length 50: "
    task5
