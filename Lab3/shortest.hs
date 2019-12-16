shortest :: [[a]] -> [a]

shortest [] = []
shortest [a] = a
shortest (list1 : list2 : lastList)
    | length list1 > length list2 = shortest (list2 : lastList)
    | length list2 > length list1 = shortest (list1 : lastList)