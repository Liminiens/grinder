module Tests

open Xunit
open Grinder
open Control

module Assert =
    let Fail() = Assert.True(false)
    let Success() = Assert.True(true)

[<Fact>]
let ``Usernames and command are extracted from text`` () =
    let expected = ["@hi"; "@hi1"; "@hi2"]
    let text = "@hi  @hi1  @hi2 ban  1test"
    match Control.getUsernamesAndCommand text with
    | InvalidCommand -> 
        Assert.Fail()
    | Command data ->
        Assert.Equal<string>(expected, data.Usernames)
        match data.Text with
        | Restrict command ->
            Assert.Equal<string>("1test", command)
        | _ ->
            Assert.Fail()
            
[<Fact>]
let ``CommandType.parse returns restrict command`` () =
    let text = "ban text 123"
    match CommandType.parse text with
    | (Some(Restrict command)) ->
        Assert.Equal<string>("text 123", command)
    | _ ->
        Assert.Fail()
            
[<Fact>]
let ``CommandType.parse returns unrestrict command`` () =
    let text = "unban some text 123"
    match CommandType.parse text with
    | (Some(UnRestrict command)) ->
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
        
[<Fact>]
let ``Minutes.Parse returns minutes from text`` () =
    let text = "10 minutes  20 days 1 month"
    match Minutes.Parse text with
    | Some(minutes) -> 
        Assert.Equal(10, minutes.Value)
    | None ->
        Assert.Fail()
        
[<Fact>]
let ``Days.Parse returns days from text`` () =
    let text = "10 minutes  20 days 1 month"
    match Days.Parse text with
    | Some(days) -> 
        Assert.Equal(20, days.Value)
    | None ->
        Assert.Fail()
        
[<Fact>]
let ``Months.Parse returns months from text`` () =
    let text = "10 minutes  20 days 1 month"
    match Months.Parse text with
    | Some(months) -> 
        Assert.Equal(1, months.Value)
    | None ->
        Assert.Fail()