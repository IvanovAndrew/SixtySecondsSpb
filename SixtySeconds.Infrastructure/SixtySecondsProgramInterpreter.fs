namespace SixtySeconds.Infrastructure

module SixtySecondsProgramInterpreter =
    
    let rec private interpretCardProgram prog =
        match prog with
        | GetCard (cardNumber, next) ->
            cardNumber |> getCardAsync mongoDb |> bindAsync (next >> interpretCardProgram mongoDb)
        | GetCardWithAccountInfo (number, next) ->
            number |> getCardWithAccInfoAsync mongoDb |> bindAsync (next >> interpretCardProgram mongoDb)
        | CreateCard ((card,acc), next) ->
            (card, acc) |> createCardAsync mongoDb |> bindAsync (next >> interpretCardProgram mongoDb)
        | ReplaceCard (card, next) ->
            card |> replaceCardAsync mongoDb |> bindAsync (next >> interpretCardProgram mongoDb)
        | GetUser (id, next) ->
            getUserAsync mongoDb id |> bindAsync (next >> interpretCardProgram mongoDb)
        | CreateUser (user, next) ->
            user |> createUserAsync mongoDb |> bindAsync (next >> interpretCardProgram mongoDb)
        | GetBalanceOperations (request, next) ->
            getBalanceOperationsAsync mongoDb request |> bindAsync (next >> interpretCardProgram mongoDb)
        | SaveBalanceOperation (op, next) ->
             saveBalanceOperationAsync mongoDb op |> bindAsync (next >> interpretCardProgram mongoDb)
        | Stop a -> async.Return a

    let interpret prog =
        try
            let interpret = interpretCardProgram()
            interpret prog
        with
        | failure -> Bug failure |> Error |> async.Return

    let interpretSimple prog =
        try
            let interpret = interpretCardProgram()
            async {
                let! result = interpret prog
                return Ok result
            }
        with
        | failure -> Bug failure |> Error |> async.Return
