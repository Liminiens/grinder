module Grinder.ParserTests

open System
open Xunit
open Grinder.Commands.Parser
open FParsec
    
[<Theory>]
[<InlineData("@name123", "@name123")>]
[<InlineData("@name_123  123", "@name_123")>]
[<InlineData("@ab123  @other", "@ab123")>]
[<InlineData("@name123@other", "@name123")>]
let ``pusername parses username`` (command: string, expected: string) =
  match run pusername command with
  | Success(res, _, _) ->
    Assert.Equal(expected, res)
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)

[<Theory>]
[<InlineData("@_12345")>]
[<InlineData("@12345")>]
[<InlineData("@")>]
[<InlineData("@name")>]
let ``pusername fails`` (command: string) =
  match run pusername command with
  | Success(_, _, _) ->
    Assert.Fail()
  | Failure(error, _, _ ) ->
    Assert.Success()

[<Theory>]
[<InlineData("@bot", "@bot")>]
[<InlineData(" @bot ", "@bot")>]
[<InlineData("\r\n@bot ", "@bot")>]
let ``pbotUsername parses bot username`` (command: string, expected: string) =
  match run (pbotUsername expected) command with
  | Success(res, _, _) ->
    Assert.Equal(expected, res)
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)

[<Theory>]
[<InlineData("1 minutes", 6u)>]
[<InlineData("4 min", 6u)>]
[<InlineData("12 mins", 12u)>]
[<InlineData("31 min", 31u)>]
let ``UsernameCommands.pminutes parses minutes`` (command: string, expected: uint32) =
  match run UsernameCommands.pminutes command with
  | Success(res, _, _) ->
    Assert.Equal(expected, res)
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)
        
[<Theory>]
[<InlineData("1 day", 1u)>]
[<InlineData("4 days", 4u)>]
let ``UsernameCommands.pdays parses days`` (command: string, expected: uint32) =
  match run UsernameCommands.pdays command with
  | Success(res, _, _) ->
    Assert.Equal(expected, res)
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)
        
[<Theory>]
[<InlineData("1 month", 1u)>]
[<InlineData("4 months", 4u)>]
let ``UsernameCommands.pmonths parses months`` (command: string, expected: uint32) =
  match run UsernameCommands.pmonths command with
  | Success(res, _, _) ->
    Assert.Equal(expected, res)
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)
        
[<Fact>]
let ``UsernameCommands.many1Usernames parses one+ usernames``() =
  let command = "@bot123 @bot231 @bot321 123"
  match run UsernameCommands.many1Usernames command with
  | Success(res, _, _) ->
    Assert.Equal<string seq>(res, ["@bot123"; "@bot231"; "@bot321"])
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)

[<Fact>]
let ``UsernameCommands.pdistinctTimeFractions parses ban duration`` () =
  let command = "2 months 1 day 1 min"
  match run UsernameCommands.pdistinctTimeFractions command with
  | Success(Timed(res), _, _) ->
    let time = DateTime.UtcNow.AddMinutes(6.).AddMonths(2).AddDays(1.)
    Assert.Equal(time.ToString("yyyyMMddTHH:mm"), res.ToString("yyyyMMddTHH:mm"))
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)
  | _ ->
    Assert.Fail()
        
[<Fact>]
let ``UsernameCommands.pforeverBan parses forever ban as eof`` () =
  let command = " \r\n\n\n\r    "
  match run UsernameCommands.pforeverBan command with
  | Success(Forever, _, _) ->
    Assert.Success()
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)
  | _ ->
    Assert.Fail()
        
[<Fact>]
let ``UsernameCommands.pforeverBan parses forever ban as forever text`` () =
  let command = " forever   "
  match run UsernameCommands.pforeverBan command with
  | Success(Forever, _, _) ->
    Assert.Success()
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)
  | _ ->
    Assert.Fail()
        
[<Theory>]
[<InlineData("@bot @first @second ban 1 day 12 months")>]
[<InlineData("    @bot @first @second ban 1 day 12 months")>]
[<InlineData(" \n\r \n@bot @first @second ban 1 day 12 months")>]
let ``UsernameCommands.parseCommand returns correct command for ban`` (command) =
  match run (UsernameCommands.parseCommand "@bot") command with
  | Success((Usernames(usernames), UsernameBan(Timed(duration))), _, _) ->
    let time = DateTime.UtcNow.AddMonths(12).AddDays(1.)
    Assert.Equal(time.ToString("yyyyMMddTHH:mm"), duration.ToString("yyyyMMddTHH:mm"))
    Assert.Equal<string seq>(["@first"; "@second"], usernames)
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)
  | _ ->
    Assert.Fail()

[<Theory>]
[<InlineData("@bot @first @second ban ")>]
[<InlineData("@bot @first@second ban ")>]
[<InlineData("@bot @first @second ban")>]
[<InlineData("@bot @first @second ban\r\n\n")>]
let ``UsernameCommands.parseCommand returns correct command for forever ban`` (command: string) =
  match run (UsernameCommands.parseCommand "@bot") command with
  | Success((Usernames(usernames), UsernameBan(Forever)), _, _) ->
    Assert.Equal<string seq>(["@first"; "@second"], usernames)
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)
  | _ ->
    Assert.Fail()
 
[<Fact>]
let ``UsernameCommands.parseCommand returns correct command for unban`` () =
  let command = "@bot @first @second unban"
  match run (UsernameCommands.parseCommand "@bot") command with
  | Success((Usernames(usernames), UsernameUnban), _, _) ->
    Assert.Equal<string seq>(["@first"; "@second"], usernames)
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)
  | _ ->
    Assert.Fail()
        
[<Theory>]
[<InlineData("@bot")>]
[<InlineData("@bot 123123")>]
[<InlineData("\r\n@bot ")>]
[<InlineData("\r\n@bot @text @text")>]
[<InlineData("\r\n@bot @text @text notban")>]
[<InlineData("@text @text ban 1 min 2 days")>]
[<InlineData("@bot1 @text @text ban 1 min 2 days")>]
let ``UsernameCommands.parseCommand fails`` (command: string) =
  match run (UsernameCommands.parseCommand "@bot") command with
  | Failure(error, _, _ ) ->
    Assert.Success()
  | _ ->
    Assert.Fail()

[<Fact>]
let ``ReplyCommands.parseCommand returns correct command for /ban`` () =
  let command = "/ban"
  match run (ReplyCommands.parseCommand String.Empty) command with
  | Success((TextBan), _, _) ->
    Assert.Success()
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)

[<Fact>]
let ``ReplyCommands.parseCommand returns correct command for ban`` () =
  let command = "@bot ban"
  match run (ReplyCommands.parseCommand "@bot") command with
  | Success((TextBan), _, _) ->
    Assert.Success()
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)

[<Fact>]
let ``AdminPrivateCommands.parseCommand parses /config`` () =
  match AdminPrivateCommands.runCommandParser "/config" with
  | Success(AdminPrivateCommand.Config, _, _) ->
    Assert.Success()
  | Success(_, _, _) ->
    Assert.Fail()
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)

[<Fact>]
let ``AdminPrivateCommands.parseCommand parses /help`` () =
  match AdminPrivateCommands.runCommandParser "/help" with
  | Success(AdminPrivateCommand.Help, _, _) ->
    Assert.Success()
  | Success(_, _, _) ->
    Assert.Fail()
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)

[<Fact>]
let ``AdminPrivateCommands.parseCommand parses /add_admin`` () =
  match AdminPrivateCommands.runCommandParser "/add_admin @user1" with
  | Success(AdminPrivateCommand.AddAdminUser(username), _, _) ->
    Assert.Equal("@user1", username)
  | Success(_, _, _) ->
    Assert.Fail()
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)

[<Fact>]
let ``AdminPrivateCommands.parseCommand parses /remove_admin`` () =
  match AdminPrivateCommands.runCommandParser "/remove_admin @user1" with
  | Success(AdminPrivateCommand.RemoveAdminUser(username), _, _) ->
    Assert.Equal("@user1", username)
  | Success(_, _, _) ->
    Assert.Fail()
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)

[<Fact>]
let ``AdminPrivateCommands.parseCommand parses /add_chat`` () =
  match AdminPrivateCommands.runCommandParser "/add_chat @chat1" with
  | Success(AdminPrivateCommand.AddChat(username), _, _) ->
    Assert.Equal("@chat1", username)
  | Success(_, _, _) ->
    Assert.Fail()
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)

[<Fact>]
let ``AdminPrivateCommands.parseCommand parses /remove_chat`` () =
  match AdminPrivateCommands.runCommandParser "/remove_chat @chat1" with
  | Success(AdminPrivateCommand.RemoveChat(username), _, _) ->
    Assert.Equal("@chat1", username)
  | Success(_, _, _) ->
    Assert.Fail()
  | Failure(error, _, _ ) ->
    Assert.FailWithMessage(error)

[<Theory>]
[<InlineData("")>]
[<InlineData(" /config ")>]
[<InlineData("/config ")>]
[<InlineData("/config 123")>]
[<InlineData("/help ")>]
[<InlineData("/help 123")>]
[<InlineData("/add_admin")>]
[<InlineData("/add_admin 123")>]
[<InlineData("/add_admin @")>]
[<InlineData("/remove_admin")>]
[<InlineData("/remove_admin 123")>]
[<InlineData("/remove_admin @")>]
[<InlineData("/add_chat")>]
[<InlineData("/add_chat 123")>]
[<InlineData("/add_chat @")>]
[<InlineData("/remove_chat")>]
[<InlineData("/remove_chat 123")>]
[<InlineData("/remove_chat @")>]
let ``AdminPrivateCommands.parseCommand parses fails`` (command: string) =
  match AdminPrivateCommands.runCommandParser command with
  | Success(_, _, _) ->
    Assert.Fail()
  | Failure(error, _, _ ) ->
    Assert.Success()