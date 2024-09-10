doubleMe x = x + x

doubleUs x y = doubleMe 2 + doubleMe 2

doubleSmallNumber x = if x > 100
    then x
    else x * 2