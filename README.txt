# rectangle-union

Author: Rainfield Mak

************************
Goal: Find the union of rectange using sweep line method

Description:
    Give a list of rectangle (rect-lst) in the format of (rect x0  x1 y0 y1) where x0 x1 and y0 y1 would be the lower left and upper right corner of rectangle. The function (total-rect-area rect-list) would return returns the total integer area covered by any (one or more) rectangles in the given lis. The function would run in O(n log n), where n is the number of rectangle inside the give list.

    The technique employed is using the sweep line method and storing the rect into a median interval tree to ensure the computation has a time complexity of O (n log n).





Run : racket test_union.rkt




Installation:
    To run the code, racket need to be install , the detail of how to install Racket is listed under "racket-install"
