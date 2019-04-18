namespace Tests

open System
open Grinder
open Grinder.Control
open Xunit
    
module Assert =
    let Fail() = Assert.True(false)
    let Success() = Assert.True(true)
        
module ControlTests =
    [<Fact>]
    let ``Validate returns message without bot username`` () =
        let bot = "@bot"
        let text = sprintf "%s @hi  @hi1  @hi2 ban  1test" bot
        match Control.validate bot text with
        | ValidMessage message ->
            Assert.Equal("@hi  @hi1  @hi2 ban  1test", message)
        | _ ->
            Assert.Fail()
        
    [<Fact>]
    let ``Usernames and command are extracted from text`` () =
        let text = "@hi  @hi1  @hi2 ban  1test"
        match Control.getUsernamesAndCommand text with
        | InvalidCommand -> 
            Assert.Fail()
        | Command data ->
            let (UsernameList usernames) = data.Usernames
            Assert.Equal<string>(["@hi"; "@hi1"; "@hi2"], usernames)
            match data.Text with
            | BanCommandText command ->
                Assert.Equal<string>("1test", command)
            | _ ->
                Assert.Fail()
                
    [<Fact>]
    let ``CommandType.parse returns restrict command`` () =
        let text = "ban text 123"
        match CommandType.parse text with
        | (Some(BanCommandText command)) ->
            Assert.Equal<string>("text 123", command)
        | _ ->
            Assert.Fail()
                
    [<Fact>]
    let ``CommandType.parse returns unrestrict command`` () =
        let text = "unban some text 123"
        match CommandType.parse text with
        | (Some(UnbanCommandText command)) ->
            Assert.Equal<string>("some text 123", command)
        | _ ->
            Assert.Fail()
            
    [<Fact>]
    let ``Invalid command when there are no usernames`` () =
        let text = "test"
        match Control.getUsernamesAndCommand text with
        | InvalidCommand -> 
            Assert.Success()
        | Command data ->
            Assert.Fail()
            
    [<Theory>]
    [<InlineData("10 minutes 20 days 1 month", 10)>]
    [<InlineData("20 mins 20 days 1 month", 20)>]
    [<InlineData("25 min", 25)>]
    [<InlineData("5 min 30 min", 5)>]
    let ``Minutes.Parse returns minutes from text`` (text: string, expected: int32) =
        match Minutes.Parse text with
        | Some minutes -> 
            Assert.Equal(expected, minutes.Value)
        | None ->
            Assert.Fail()
            
    [<Theory>]
    [<InlineData("10 minutes 20 days 1 month", 20)>]
    [<InlineData("20 mins 25 days 1 month", 25)>]
    let ``Days.Parse returns days from text`` (text: string, expected: int32) =
        match Days.Parse text with
        | Some days -> 
            Assert.Equal(expected, days.Value)
        | None ->
            Assert.Fail()
            
    [<Fact>]
    let ``Months.Parse returns months from text`` () =
        let text = "10 minutes  20 days 1 month"
        match Months.Parse text with
        | Some months -> 
            Assert.Equal(1, months.Value)
        | None ->
            Assert.Fail()
    
    [<Fact>]
    let ``Duration IsSet returns correct value when not set`` () =       
        let duration = Duration()
        Assert.False(duration.IsSet())
        Assert.Equal(Unchecked.defaultof<DateTime>, duration.GetValue())
        
    [<Fact>]
    let ``Duration IsSet returns correct value when set`` () =       
        let duration = Duration()
        duration.Add(Minutes 1)
        Assert.True(duration.IsSet())
    
    [<Fact>]
    let ``Duration Add with minutes gives correct value`` () =       
        let duration = Duration()
        let minutes = 5
        duration.Add(Minutes minutes)
        Assert.Equal(DateTime.UtcNow.AddMinutes(float minutes).Minute, duration.GetValue().Minute)
        
    [<Fact>]
    let ``Duration Add with days gives correct value`` () =       
        let duration = Duration()
        let days = 5
        duration.Add(Days days)
        Assert.Equal(DateTime.UtcNow.AddDays(float days).Day, duration.GetValue().Day)
        
    [<Fact>]
    let ``Parse returns correct command for ban message`` =
        let bot = "@bot"
        let command = sprintf "%s @user @user1 ban 5 min 10 days 1 month" bot
        match Control.parse bot command with
        | Ban(UsernameList(users), until) ->
            Assert.Equal<string>(["@user";"@user1"], users)
            
            let now = DateTime.UtcNow.AddMinutes(float 5).AddDays(float 10).AddMonths(1)
            Assert.Equal(now.Minute, until.Minute)
            Assert.Equal(now.Day, until.Day)
            Assert.Equal(now.Month, until.Month)
            Assert.Equal(now.Year, until.Year)
        | _ ->
            Assert.Fail()
    
    [<Theory>]
    [<InlineData("@bot @user @user1 ban        ")>]
    [<InlineData("@bot @user @user1 ban")>]
    [<InlineData("@bot @user @user1 ban forever")>]
    let ``Parse returns correct command for permament ban`` (command: string)=
        let bot = "@bot"
        match Control.parse bot command with
        | Ban(UsernameList(users), until) ->
            Assert.Equal<string>(["@user";"@user1"], users)
            
            let now = DateTime.UtcNow.AddMonths(13)
            Assert.Equal(now.Month, until.Month)
            Assert.Equal(now.Year, until.Year)
        | _ ->
            Assert.Fail()
             
    [<Theory>]
    [<InlineData("@bot @user @123user unban      \r  ")>]
    [<InlineData("@bot @user @123user unban")>]
    let ``Parse returns correct command for unban`` (command: string)=
        let bot = "@bot"
        match Control.parse bot command with
        | Unban(UsernameList(users)) ->
            Assert.Equal<string>(["@user";"@123user"], users)
        | _ ->
            Assert.Fail()