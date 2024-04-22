:- module(init, [ init/3 ]).

/**
 * init(-RowsClues, -ColsClues, Grid).
 * Predicate specifying the initial grid, which will be shown at the beginning of the game,
 * including the rows and columns clues.
 */

 init(
    [[4], [6], [3,4], [2,5], [4,5], [10], [10], [8], [6], [4]],% RowsClues
    [[4], [6], [2,7], [10], [2,1,5], [10], [10], [8], [6], [4]], % ColsClues
    [[ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ], 
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ], % Grid
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ]]).