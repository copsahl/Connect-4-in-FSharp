open System

let mutable gameBoard = array2D[
    ['_';'_';'_';'_';'_';'_';'_'];
    ['_';'_';'_';'_';'_';'_';'_'];
    ['_';'_';'_';'_';'_';'_';'_'];
    ['_';'_';'_';'_';'_';'_';'_'];
    ['_';'_';'_';'_';'_';'_';'_'];
    ['_';'_';'_';'_';'_';'_';'_']
    ]

let mutable turn =  0
let mutable win = 0
let mutable pass = 0
let mutable piece = '0'
let mutable flag1 = 1
let mutable flag2 = 1
let mutable flag3 = 1
let mutable tie = 0
let mutable int_column = Unchecked.defaultof<int>
let mutable column = ""
let mutable not_char = 0

let print_gameBoard() =

    for y = 0 to 5 do
        for x = 0 to 6 do
            printf "%c|" gameBoard.[y,x]
        printf "\n"
    
    printfn "--------------"
    printfn "1 2 3 4 5 6 7 "

let play_game() =
    let mutable row = 5
    
    if turn.Equals(0) then
        piece <- 'O'
    elif turn.Equals(1) then
        piece <- 'X'
    

    printfn "Turn --> %c" piece
    printf "Enter the Column you want to drop your piece in: "
    let mutable column = Console.ReadLine()
    try
        int_column <- column |> int
    with
        | :? System.FormatException -> not_char <- 1;

    while flag3.Equals(1) do
        if not_char.Equals(1) then 
            printf "Invalid entry, type a number 1-7: "
            column <- Console.ReadLine()
            int_column <- column |> int
            not_char <- 0
        flag3 <- 0

    flag3 <- 1
    //Check if column number is valid
    while flag2.Equals(1) do 
        if int_column > 7  || int_column < 1 then
            printf "That column doesn't exist, enter a number 1-7: "
            column <- Console.ReadLine()
            int_column <- column |> int
        else
            flag2 <- 0
    flag2 <- 1
    int_column <- int_column - 1

    //Places pieces 
    while flag1.Equals(1) do
        if (gameBoard.[row,int_column].Equals('O') || gameBoard.[row,int_column].Equals('X')) then
            row <- row - 1
            if row < 0 then
                printf "That column is already full! Choose a different one: "
                column <- Console.ReadLine()
                int_column <- column |> int
                int_column <- int_column - 1
                row <- 5
        else if (gameBoard.[row,int_column].Equals('_')) then
            gameBoard.[row,int_column] <- piece
            flag1 <- 0
    
    flag1 <- 1

let check_row_won() =
    let mutable row = 5
    while row > 0 do 
        if gameBoard.[row,0].Equals(gameBoard.[row,1]) && gameBoard.[row,0].Equals(gameBoard.[row,2]) && gameBoard.[row,0].Equals(gameBoard.[row,3]) && gameBoard.[row,0].Equals(piece) then
            pass <- 1
        elif gameBoard.[row,1].Equals(gameBoard.[row,2]) && gameBoard.[row,1].Equals(gameBoard.[row,3]) && gameBoard.[row,1].Equals(gameBoard.[row,4]) && gameBoard.[row,1].Equals(piece) then
            pass <- 1
        elif gameBoard.[row,2].Equals(gameBoard.[row,3]) && gameBoard.[row,2].Equals(gameBoard.[row,4]) && gameBoard.[row,2].Equals(gameBoard.[row,5]) && gameBoard.[row,2].Equals(piece) then
            pass <- 1
        elif gameBoard.[row,3].Equals(gameBoard.[row,4]) && gameBoard.[row,3].Equals(gameBoard.[row,5]) && gameBoard.[row,3].Equals(gameBoard.[row,6]) && gameBoard.[row,3].Equals(piece) then
            pass <- 1

        row <- row - 1
    
let check_column_won() =
    let mutable col = 0
    while col < 7 do
        if gameBoard.[0,col].Equals(gameBoard.[1,col]) && gameBoard.[0,col].Equals(gameBoard.[2,col]) && gameBoard.[0,col].Equals(gameBoard.[3,col]) && gameBoard.[0,col].Equals(piece) then
            pass <- 1
        elif gameBoard.[1,col].Equals(gameBoard.[2,col]) && gameBoard.[1,col].Equals(gameBoard.[3,col]) && gameBoard.[1,col].Equals(gameBoard.[4,col]) && gameBoard.[1,col].Equals(piece) then
            pass <- 1
        elif gameBoard.[2,col].Equals(gameBoard.[3,col]) && gameBoard.[2,col].Equals(gameBoard.[4,col]) && gameBoard.[2,col].Equals(gameBoard.[5,col]) && gameBoard.[2,col].Equals(piece) then
            pass <- 1
        col <- col + 1
    
let check_diag_won() =
    let mutable col = 0
    let mutable cl2 = 6
    while col < 4 do
        if gameBoard.[3,col].Equals(gameBoard.[2,col+1]) && gameBoard.[3,col].Equals(gameBoard.[1,col+2]) && gameBoard.[3,col].Equals(gameBoard.[0,col+3]) && gameBoard.[3,col].Equals(piece) then
            pass <- 1
        if gameBoard.[4,col].Equals(gameBoard.[3,col+1]) && gameBoard.[4,col].Equals(gameBoard.[2,col+2]) && gameBoard.[4,col].Equals(gameBoard.[1,col+3]) && gameBoard.[4,col].Equals(piece) then
            pass <- 1
        if gameBoard.[5,col].Equals(gameBoard.[4,col+1]) && gameBoard.[5,col].Equals(gameBoard.[3,col+2]) && gameBoard.[5,col].Equals(gameBoard.[2,col+3]) && gameBoard.[5,col].Equals(piece) then
            pass <- 1
        col <- col + 1
    
    while cl2 > 2 do
        if gameBoard.[3,cl2].Equals(gameBoard.[2,cl2-1]) && gameBoard.[3,cl2].Equals(gameBoard.[1,cl2-2]) && gameBoard.[3,cl2].Equals(gameBoard.[0,cl2-3]) && gameBoard.[3,cl2].Equals(piece) then
            pass <- 1
        if gameBoard.[4,cl2].Equals(gameBoard.[3,cl2-1]) && gameBoard.[4,cl2].Equals(gameBoard.[2,cl2-2]) && gameBoard.[4,cl2].Equals(gameBoard.[1,cl2-3]) && gameBoard.[4,cl2].Equals(piece) then
            pass <- 1
        if gameBoard.[5,cl2].Equals(gameBoard.[4,cl2-1]) && gameBoard.[5,cl2].Equals(gameBoard.[3,cl2-2]) && gameBoard.[5,cl2].Equals(gameBoard.[2,cl2-3]) && gameBoard.[5,cl2].Equals(piece) then
            pass <- 1
        cl2 <- cl2 - 1

let check_if_tie() =
    for y = 0 to 5 do
        for x = 0 to 6 do
            if not (gameBoard.[y,x].Equals('_')) then
                tie <- tie + 1

let intro() = 
    Console.ForegroundColor <- ConsoleColor.Magenta
    printfn "   ________         __  __           ______                            __     ______                "
    printfn "  / ____/ /_  ___  / /_/ /_____     / ____/___  ____  ____  ___  _____/ /_   / ____/___  __  _______"
    printfn " / / __/ __ \/ _ \/ __/ __/ __ \   / /   / __ \/ __ \/ __ \/ _ \/ ___/ __/  / /_  / __ \/ / / / ___/"
    printfn "/ /_/ / / / /  __/ /_/ /_/ /_/ /  / /___/ /_/ / / / / / / /  __/ /__/ /_   / __/ / /_/ / /_/ / /    "
    printfn "\____/_/ /_/\___/\__/\__/\____/   \____/\____/_/ /_/_/ /_/\___/\___/\__/  /_/    \____/\__,_/_/     "
    printfn "\n\n"
    Console.ForegroundColor <- ConsoleColor.Cyan
    printfn "_________                  _____     _________   ______                   ______________                     "
    printfn "__  ____/_________________ __  /___________  /   ___  /______  ______     __  ____/__  /_______ ____________ "
    printfn "_  /    __  ___/  _ \  __ `/  __/  _ \  __  /    __  __ \_  / / /__(_)    _  /    __  __ \  __ `/_  ___/  _ \\"
    printfn "/ /___  _  /   /  __/ /_/ // /_ /  __/ /_/ /     _  /_/ /  /_/ /___       / /___  _  / / / /_/ /_(__  )/  __/"
    printfn "\____/  /_/    \___/\__,_/ \__/ \___/\__,_/      /_.___/_\__, / _(_)      \____/  /_/ /_/\__,_/ /____/ \___/ "
    printfn "                                                        /____/                                               "
    printfn "\n\n"
    Console.ForegroundColor <- ConsoleColor.White
    Console.WriteLine("Press Enter to play...")
    Console.ResetColor()
    Console.ReadKey() |> ignore

let main =
    intro()
    while win.Equals(0) do
        Console.Clear()
        print_gameBoard()
        play_game()
        check_row_won()
        check_column_won()
        check_diag_won()
        if pass > 0 then
            Console.Clear()
            print_gameBoard()
            Console.ForegroundColor <- ConsoleColor.Green
            printfn "\n%c is the winner!\nCongratulations!!!" piece
            win <- 1
        if tie >=41 then
            Console.ForegroundColor <- ConsoleColor.Green
            printfn "\nIT IS A TIE WTF!!!!"
        if (turn.Equals(0)) then
            turn <- 1
        elif (turn.Equals(1)) then
            turn <- 0
        
Console.ForegroundColor <- ConsoleColor.Yellow
printfn "\nThanks for playing GHETTO CONNECT 4!\n"
Console.ResetColor()
printf "Hit Enter to Quit..."
Console.ReadKey() |> ignore




