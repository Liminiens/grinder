namespace Grinder

open System
   
module Control =
    open FSharp.Text.RegexProvider
    
    type CommandTextType =
        | BanCommandText of string
        | UnbanCommandText of string
        
    type Command =
        | Ban of DateTime
        | Unban of DateTime
        | IgnoreCommand
        
    module CommandType =
        let [<Literal>] banText = "ban"
        let [<Literal>] unbanText = "unban"
            
        let parse (text: string) =
            if text.StartsWith(banText) then
                text.Substring(banText.Length, text.Length - banText.Length).Trim()
                |> BanCommandText
                |> Some
            elif text.StartsWith(unbanText) then
                text.Substring(unbanText.Length, text.Length - unbanText.Length).Trim()
                |> UnbanCommandText
                |> Some
            else
                None
    
    type ParsedCommand = {
        Usernames: string list
        Text: CommandTextType
    }

    type MessageToBotValidationResult = 
        | ValidMessage of string
        | InvalidMessage
            
    type CommandParsingResult =
        | Command of ParsedCommand
        | InvalidCommand
    
    let validate (botUsername: string) (text: string) = 
        let text = text.Trim()
        if text.StartsWith(botUsername) then
            let message = text.Substring(0, botUsername.Length - 1).Trim()
            ValidMessage message
        else
            InvalidMessage
   
    type MinutesRegex = Regex< "(?<value>\d+)\s+min(s)?" >
    
    type DaysRegex = Regex< "(?<value>\d+)\s+day(s)?" >
    
    type MonthsRegex = Regex< "(?<value>\d+)\s+month(s)?" >

    type Minutes =
        private Minutes of int32
            static member Parse(text) =
                let result = MinutesRegex().TypedMatch(text)
                if result.value.Success then
                    let value = int32 result.value.Value
                    if value > 0 then 
                        value |> Minutes |> Some
                    else
                        None
                else
                    None
            
            member __.Value =
                let (Minutes value) = __
                value

    type Days =
        private Days of int32
            static member Parse(text) =
                let result = DaysRegex().TypedMatch(text)
                if result.value.Success then
                    let value = int32 result.value.Value
                    if value > 0 then 
                        value |> Days |> Some
                    else
                        None
                else
                    None
            
            member __.Value =
                let (Days value) = __
                value

    type Months =
        private Months of int32
            static member Parse(text) =
                let result = MonthsRegex().TypedMatch(text)
                if result.value.Success then
                    let value = int32 result.value.Value
                    if value > 0 then 
                        value |> Months |> Some
                    else
                        None
                else
                    None
            
            member __.Value =
                let (Months value) = __
                value

    let getUsernamesAndCommand text =
        let matches = Regex.Matches(text, "@(\w|\d)+")
        let usernames = 
            matches
            |> Seq.map ^ fun m -> m.Value
            |> List.ofSeq
        match List.length usernames with
        | 0 -> 
            InvalidCommand
        | _ ->
            let lastMatch = matches |> Seq.maxBy ^ fun el -> el.Index
            let commandText =
                text.Substring(lastMatch.Index + lastMatch.Length, text.Length - (lastMatch.Index + lastMatch.Length))
                    .Trim()
            match CommandType.parse commandText with
            | Some(command) ->
                Command { Usernames = usernames
                          Text = command }
            | None ->
                InvalidCommand

    let parse botUsername text =
        match validate botUsername text with
        | ValidMessage text -> 
            match getUsernamesAndCommand text with
            | Command data ->
                match data.Text with
                | BanCommandText commantText ->
                    match commantText with
                    | ""
                    | "forever" ->
                        DateTime.UtcNow.AddMonths(13)
                        |> Ban
                    | other ->
                        let mutable now = DateTime.UtcNow
                        let minutes = Minutes.Parse other
                        let days = Days.Parse other
                        let months = Months.Parse other
                        if [minutes; days; months] then
                            IgnoreCommand
                | UnbanCommandText commantText ->
                    IgnoreCommand
            | InvalidCommand ->
                IgnoreCommand  
        | InvalidMessage ->
            IgnoreCommand

